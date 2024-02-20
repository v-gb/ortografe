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

let simplify_mapping tbl ~plurals_in_s =
  (* remove identity mappings and trivial plurals *)
  Hashtbl.filteri_inplace tbl ~f:(fun ~key:old ~data:(new_, _) ->
      String.(<>) old new_
      && (not plurals_in_s
          || match String.chop_suffix old ~suffix:"s", String.chop_suffix new_ ~suffix:"s" with
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

let build_lexique_post90 (lexique : Data.Lexique.t list) post90 ~fix_90 =
  (* this causes a few regressions like
     allécherait,alécherait
     becomes
     allécherait,allècherait
     because the change of direction in accent is actually not taken into account
     in the prononciation, and so the erofa rewriting fails to apply.

     Or even graffito -> grafito becoming graffito -> graffiti.         
   *)
  if fix_90
  then
    List.map lexique ~f:(fun r ->
      match Hashtbl.find post90 r.ortho with
      | None -> r
      | Some new_ortho -> { r with ortho = new_ortho })
  else    
    List.concat_map lexique ~f:(fun r ->
        match Hashtbl.find post90 r.ortho with
        | None -> [ r ]
        | Some new_ortho -> [ r; { r with ortho = new_ortho } ])

let build_erofa_ext ~root =
  let combined_erofa =
    (* start with whole erofa db, so [simplify_mapping] considers singular in the erofa csv *)
    let base = Hashtbl.map (Data_fs.load_erofa (`Root root)) ~f:(fun data -> data, -1) in
    let post90 = Data_fs.load_post90 (`Root root) in
    ignore (
        let lexique = Data_fs.load_lexique (`Root root) in
        let lexique_post90 = build_lexique_post90 lexique post90 ~fix_90:true in
        Rewrite.gen
          ~not_understood:`Ignore
          ~fix_oe:true (* really only matters for proper nouns like Œdipe, since all other
                          œ get removed *)
          lexique_post90
          (fun old new_ -> add_ranked base ~key:old ~data:new_)
        : Rewrite.stats);
    add_post90_entries base post90;
    simplify_mapping base ~plurals_in_s:true;
    Hashtbl.filter_inplace base ~f:(fun (_, rank) -> rank >= 0 (* i.e. "not from erofa" *));
    ranked base
  in
  List.iter combined_erofa ~f:(fun (old, new_) ->
      print_endline [%string "%{old},%{new_}"])

let with_flow ~env ~write ~diff ~drop ~f =
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
     if not drop && Unix.isatty Unix.stdout then
       Eio.Switch.run (fun sw_pr ->
           let process =
             Eio.Switch.run (fun sw_fd ->
                 let r, w = Eio.Process.pipe ~sw:sw_fd (Eio.Stdenv.process_mgr env) in
                 let process =
                   Eio.Process.spawn ~sw:sw_pr
                     (Eio.Stdenv.process_mgr env) ~stdin:r [ "less" ] in
                 Eio.Flow.close r;
                 Eio.Buf_write.with_flow w f;
                 process)
           in
           Eio.Process.await_exn process)
     else Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) f;
     None

type static =
  { data_lexique_Lexique383_gen_tsv : string
  ; extension_dict1990_gen_csv : string }
let gen ~env ?static ~rules ~rect90 ~all ~write ~diff ~drop ~oe () =
  let rect90 =
    rect90
    || List.is_empty rules
    || List.exists rules ~f:(fun r -> String.(=) (Rewrite.name r) "erofa")
  in
  let root = lazy (root ~from:(Eio.Stdenv.fs env)) in
  let lexique =
    Data_fs.load_lexique (match static with
                           | None -> `Root (force root)
                           | Some r -> `Str r.data_lexique_Lexique383_gen_tsv)
  in
  let post90, lexique =
    let post90 =
      Data_fs.load_post90 (match static with
                            | None -> `Root (force root)
                            | Some r -> `Str r.extension_dict1990_gen_csv)
    in
    (if rect90 then post90 else Hashtbl.create (module String)),
    build_lexique_post90 lexique post90 ~fix_90:rect90
  in
  match
    with_flow ~env ~write ~diff ~drop ~f:(fun buf ->
        let print, after =
          if all
          then (fun old new_ ->
                let mod_ = if String.(=) old new_ then "=" else "M" in
                Eio.Buf_write.string buf [%string "%{old},%{new_},%{mod_}\n"]),
               ignore
          else (
            let all = Hashtbl.create (module String) ~size:(List.length lexique) in
            (fun old new_ -> add_ranked all ~key:old ~data:new_),
            (fun () ->
              add_post90_entries all post90;
              let plurals_in_s = List.for_all rules ~f:Rewrite.plurals_in_s in
              simplify_mapping all ~plurals_in_s;
              let write = if drop then ignore else (fun str -> Eio.Buf_write.string buf str) in
              (let name =
                 ((if rect90 then ["1990"] else [])
                  @ if List.is_empty rules
                    then ["érofa" ]
                    else List.map rules ~f:Rewrite.name)
                 |> List.sort ~compare:String.compare
                 |> String.concat ~sep:" "
               in
               `Assoc
                 [ "desc", `String name
                 ; "lang", `String "fr"
                 ; "supports_repeated_rewrites",
                   `Bool (List.for_all rules ~f:Rewrite.supports_repeated_rewrites)
                 ; "plurals_in_s", `Bool plurals_in_s
                 ]
               |> Yojson.to_string
               |> (fun s -> s ^ "\n")
               |> write
              );
              List.iter (ranked all) ~f:(fun (old, new_) ->
                  write [%string "%{old},%{new_}\n"])))
        in
        let stats =
          Rewrite.gen
            ~not_understood:`Ignore
            ~fix_oe:oe
            ~rules
            lexique print in
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

let gen_cmd ?static ?doc name =
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
     and+ oe =
       C.Arg.value (C.Arg.flag
                      (C.Arg.info ~doc:"réécrire oe en œ quand c'est possible" ["oe"]))
     in
     Eio_main.run (fun env ->
         try gen ?static ~env ~rules ~rect90 ~all ~write ~diff ~drop ~oe ()
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
               let lexique = Data_fs.load_lexique (`Root root) in
               let lexique =
                 if post90
                 then
                   let post90 = Data_fs.load_post90 (`Root root) in
                   build_lexique_post90 lexique post90 ~fix_90:false (* to check both versions *)
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

