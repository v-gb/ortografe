open Core
module Unix = Core_unix
let (^/) = Eio.Path.(/)

let root ~from =
  let (/) = Eio.Path.(/) in
  match Sys.getenv "DUNEROOT" with
  | Some v -> from / v
  | None ->
     let from = ref from in
     let i = ref 0 in
     while not (Eio.Path.is_file (!from / "dune-project")) do
       if Eio.Path.is_file (!from ^/ ".digest-db")
       then raise_s [%sexp "escaping from dune dir", (Stdlib.Sys.getcwd () : string)];
       from := !from / "..";
       i := !i + 1;
       if !i > 1000 then failwith ("can't find repo root " ^ Eio.Path.native_exn !from)
     done;
     !from

let simplify_mapping tbl =
  (* remove identity mappings and trivial plurals *)
  Hashtbl.filteri_inplace tbl ~f:(fun ~key:old ~data:(new_, _) ->
      String.(<>) old new_
      && (match String.chop_suffix old ~suffix:"s", String.chop_suffix new_ ~suffix:"s" with
         | Some old_no_s, Some new_no_s ->
            (match Hashtbl.find tbl old_no_s with
             | Some (new', _) when String.(=) new' new_no_s -> false
             | _ -> true)
         | _ -> true))

let add_post90_entries base post90 =
  Hashtbl.iteri post90 ~f:(fun ~key:pre90 ~data:post90 ->
      match Hashtbl.find base pre90 with
      | Some _ -> () (* erofa dictionary contains pre-1990 spellings, like frisotter rather than
                        frisoter *)
      | None ->
         match Hashtbl.find base post90 with
         | Some (new_ortho, rank) ->
            (* keep rank so related words together. When rank < 0, the word comes from the erofa
               dictionary and is going to be filtered out, so don't keep that. *)
            Hashtbl.add_exn base ~key:pre90
              ~data:(new_ortho, if rank >= 0 then rank else 100000000)
         | None ->
            (* filter out changes such as chariotage->charriotage *)
            if String.is_prefix post90 ~prefix:"charr"
               || String.is_prefix post90 ~prefix:"combatt"
               || String.is_prefix post90 ~prefix:"hindou" (* don't understand if the h is really
                                                              dropped here? *)
               || String.(=) post90 "reboursouffler"
            then ()
            else
              (* we created fixed ranks to generated the same ordering as previous code *)
              Hashtbl.add_exn base ~key:pre90 ~data:(post90, 100000000))

let add_ranked tbl ~key ~data =
  ignore (Hashtbl.add tbl ~key ~data:(data, Hashtbl.length tbl) : [ `Ok | `Duplicate ])

let ranked tbl =
  Hashtbl.to_alist tbl
  |> List.map ~f:(fun (o, (n, rank)) -> (rank, (o, n)))
  |> List.sort ~compare:[%compare: int * (string * _)]
  |> List.map ~f:snd  

let build_lexique_post90 (lexique : Data_src.Lexique.t list) post90 =
  (* this causes a few regressions like
     allécherait,alécherait
     becomes
     allécherait,allècherait
     because the change of direction in accent is actually not taken into account
     in the prononciation, and so the erofa rewriting fails to apply.

     Or even graffito -> grafito becoming graffito -> graffiti.         
   *)
  List.map lexique ~f:(fun r ->
      match Hashtbl.find post90 r.ortho with
      | None -> r
      | Some new_ortho -> { r with ortho = new_ortho })

let build_erofa_ext ~root =
  let combined_erofa =
    (* start with whole erofa db, so [simplify_mapping] considers singular in the erofa csv *)
    let base = Hashtbl.map (Data_src.load_erofa ~root) ~f:(fun data -> data, -1) in
    let post90 = Data_src.load_post90 ~root in
    ignore (
        let lexique = Data_src.Lexique.load ~root () in
        let lexique_post90 = build_lexique_post90 lexique post90 in
        Rewrite.gen ~root ~lexique:lexique_post90
          ~skip_not_understood:true
          (fun old new_ -> add_ranked base ~key:old ~data:new_)
        : Rewrite.stats);
    add_post90_entries base post90;
    simplify_mapping base;
    Hashtbl.filter_inplace base ~f:(fun (_, rank) -> rank >= 0 (* i.e. "not from erofa" *));
    ranked base
  in
  List.iter combined_erofa ~f:(fun (old, new_) ->
      print_endline [%string "%{old},%{new_}"])

let with_flow ~env ~write ~diff ~f =
  match write, diff with
  | Some _, Some _ -> failwith "can't specify both --write and --diff"
  | Some fname, None ->
     Eio.Path.with_open_out (Eio.Path.(/) (Eio.Stdenv.fs env) fname)
       ~create:(`Or_truncate 0o666)
       (fun flow -> Eio.Buf_write.with_flow flow f);
     None
  | None, Some fname ->
     let tmp_fname = fname ^ ".tmp" in
     Eio.Path.with_open_out (Eio.Path.(/) (Eio.Stdenv.fs env) tmp_fname)
       ~create:(`Or_truncate 0o666)
       (fun flow -> Eio.Buf_write.with_flow flow f);
     Some (`Diff (fname, tmp_fname))
  | None, None ->
     Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) f;
     None

let gen ~env ~rules ~rect90 ~all ~write ~diff ~drop =
  let rect90 = rect90 || List.is_empty rules in
  let root = root ~from:(Eio.Stdenv.fs env) in
  let lexique = Data_src.Lexique.load ~root () in
  let post90, lexique =
    (* One problem that complicate enabling this by default is the resulting lexique
       has spelling that are out of sync with the pronunciation, thus preventing them
       from being rewritten. Annoying for ortograf.net or alfonic, where everything
       is supposed to be rewritten. *)
    if rect90
    then
      let post90 = Data_src.load_post90 ~root in
      post90, build_lexique_post90 lexique post90
    else Hashtbl.create (module String), lexique
  in
  match
    with_flow ~env ~write ~diff ~f:(fun buf ->
        let print, after =
          if drop
          then (fun _ _ -> ()), ignore
          else if all
          then (fun old new_ ->
                (* doesn't contain rect1990 mappings currently *)
                let mod_ = if String.(=) old new_ then "=" else "M" in
                Eio.Buf_write.string buf [%string "%{old},%{new_},%{mod_}\n"]),
               ignore
          else (
            let all = Hashtbl.create (module String) ~size:(List.length lexique) in
            (fun old new_ -> add_ranked all ~key:old ~data:new_),
            (fun () ->
              add_post90_entries all post90;
              simplify_mapping all;
              (let name =
                 ((if rect90 then ["1990"] else [])
                  @ if List.is_empty rules
                    then ["érofa" ]
                    else List.map rules ~f:Rewrite.name)
                 |> List.sort ~compare:String.compare
                 |> String.concat ~sep:" "
               in
               [ "desc", `String name
               ; "lang", `String "fr"
               ; "supports_repeated_rewrites",
                 `Bool (List.for_all rules ~f:Rewrite.supports_repeated_rewrites)
               ]
               |> List.map ~f:(fun (k, v) ->
                      sprintf "%S: %s" k
                        (match v with
                         | `String s -> sprintf "%S" s
                         | `Bool b -> Bool.to_string b))
               |> String.concat ~sep:", "
               |> sprintf "{%s}\n"
               |> Eio.Buf_write.string buf
              );
              List.iter (ranked all) ~f:(fun (old, new_) ->
                  Eio.Buf_write.string buf [%string "%{old},%{new_}\n"])))
        in
        let stats =
          Rewrite.gen ~skip_not_understood:rect90
            ~lexique ~root ~rules print in
        after ();
        if Unix.isatty Unix.stderr then
          eprint_s [%sexp ~~(stats : Rewrite.stats)];
      )
  with
  | None -> ()
  | Some (`Diff (a, b)) ->
     Eio.Process.run (Eio.Stdenv.process_mgr env)
       [ "bash"; "-e"; "-u"; "-o"; "pipefail"; "-c"
       ; Sys.concat_quoted [ "patdiff"; "-context"; "1"; a; b ] ^ " | less -R"
       ]
       ~is_success:(function 0 | 1 -> true | _ -> false)

let gen_cmd ?doc name =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  C.Cmd.v (C.Cmd.info ?doc name)
    (let+ rules =
       List.fold_right
         ~init:(return [])
         (force Rewrite.all)
         ~f:(fun rule acc ->
           let+ present = C.Arg.value (C.Arg.flag (C.Arg.info
                                                     ~doc:(Rewrite.doc rule)
                                                     [Rewrite.name rule]))
           and+ acc in
           if present then rule :: acc else acc)
     and+ rect90 = 
       C.Arg.value (C.Arg.flag
                      (C.Arg.info ~doc:"appliquer les rectifications de 1990" ["90"]))
     and+ all = C.Arg.value (C.Arg.flag (C.Arg.info ~doc:"inclure les mots inchangés" ["all"]))
     and+ write =
       C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.string) None
                      (C.Arg.info ~doc:"écrire le dictionnaire dans le fichier spécifié" ["write"]))
     and+ diff =
       C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.string) None
                      (C.Arg.info ~doc:"diff le dictionnaire avec le fichier spécifié" ["diff"]))
     and+ drop =
       C.Arg.value (C.Arg.flag
                      (C.Arg.info ~doc:"(pour profiler) jeter le dictionnaire calculé" ["drop"]))
     in
     Eio_main.run (fun env ->
         try gen ~env ~rules ~rect90 ~all ~write ~diff ~drop
         with Eio.Exn.Io (Eio.Net.E (Connection_reset (Eio_unix.Unix_error (EPIPE, _, _))), _) ->
           ()))

let main () =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let cmd =
    C.Cmd.group (C.Cmd.info "dict-gen")
      [ C.Cmd.v (C.Cmd.info "check-rules")
          (let+ post90 = C.Arg.value (C.Arg.flag (C.Arg.info ["90"])) in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               let lexique = Data_src.Lexique.load ~root () in
               let lexique =
                 if post90
                 then
                   let post90 = Data_src.load_post90 ~root in
                   build_lexique_post90 lexique post90
                 else lexique
               in
               Rules.check lexique ~skip:(Rewrite.load_skip ())))
      ; gen_cmd "gen"
      ; C.Cmd.v (C.Cmd.info "erofa-ext")
          (let+ () = return () in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               build_erofa_ext ~root))
      ]
  in
  C.Cmd.eval cmd |> exit

