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

let hashtbl_of_alist_multi m l =
  (* Map.of_alist_exn guarantees that the inner list have the same order as the
     initial list, but hashtbl reverses the lists ! Argh. *)
  let h = Hashtbl.create m ~size:(List.length l) in
  List.iter (List.rev l) ~f:(fun (key, data) ->
      Hashtbl.add_multi h ~key ~data);
  h

let read_key_value_comma_sep_file f =
  Eio.Path.load f
  |> String.split_lines
  |> List.map ~f:(fun line ->
         match String.split line ~on:',' with
         | [a; b] -> (a, b)
         | _ -> raise_s [%sexp "bad line", (line : string)])
  |> hashtbl_of_alist_multi (module String)
  |> Hashtbl.map ~f:(fun l -> List.hd_exn l)

let load_erofa ~root =
  read_key_value_comma_sep_file
    (root ^/ "extension/dict.external-sources.gen.csv")

let load_post90 ~root =
  read_key_value_comma_sep_file
    (root ^/ "extension/dict1990.gen.csv")

let simplify_mapping old_new_list =
  (* remove identity mappings and trivial plurals *)
  let new_by_old = Hashtbl.of_alist_exn (module String) old_new_list in
  List.filter old_new_list ~f:(fun (old, new_) ->
      String.(<>) old new_
      && (match String.chop_suffix old ~suffix:"s", String.chop_suffix new_ ~suffix:"s" with
         | Some old_no_s, Some new_no_s ->
            (match Hashtbl.find new_by_old old_no_s with
             | Some new' when String.(=) new' new_no_s -> false
             | _ -> true)
         | _ -> true))

let combined_erofa erofa lexiquepost90_to_erofa post90 =
  let erofa_by_post90 =
    hashtbl_of_alist_multi (module String) lexiquepost90_to_erofa
    |> Hashtbl.map ~f:List.hd_exn
  in
  let post90_entries =
    Hashtbl.filter_mapi post90 ~f:(fun ~key:pre90 ~data:post90 ->
        match Hashtbl.find erofa_by_post90 pre90 with
        | Some erofa -> raise_s [%sexp ~~(pre90 : string),
                                 ~~(post90 : string),
                                 ~~(erofa : string)]
        | None ->
           match Option.first_some
                   (Hashtbl.find erofa post90)
                   (Hashtbl.find erofa_by_post90 post90)
           with
           | Some _ as o -> o
           | None ->
              (* filter out changes such as chariotage->charriotage *)
              if String.is_prefix post90 ~prefix:"charr"
              || String.is_prefix post90 ~prefix:"combatt"
              || String.is_prefix post90 ~prefix:"hindou" (* don't understand if the h is really
                                                             dropped here? *)
              || String.(=) post90 "reboursouffler"
              then None
              else Some post90)
    |> Hashtbl.to_alist
    |> List.sort ~compare:[%compare: string * string]
  in
  let all_entries =
    (Hashtbl.to_alist erofa (* so [simplify_mapping] considers singular in the erofa csv *)
     @ lexiquepost90_to_erofa
     @ post90_entries
    ) |> Staged.unstage (List.stable_dedup_staged ~compare:[%compare: string * _])
  in
  let all_entries = simplify_mapping all_entries in
  let all_entries = List.filter all_entries ~f:(fun (old, _) -> not (Hashtbl.mem erofa old)) in
  all_entries

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

let build_erofa_ext ~root ~header =
  let erofa = load_erofa ~root in
  let post90 = load_post90 ~root in
  let lexique = Data_src.Lexique.load ~root () in
  let lexique_post90 = build_lexique_post90 lexique post90 in
  let lexiquepost90_to_erofa =
    let r = ref [] in
    let _ : Rewrite.stats =
      Rewrite.gen ~root ~lexique:lexique_post90
        ~skip_not_understood:true
        (fun old new_ -> r := (old, new_) :: !r)
    in
    List.rev !r
  in
  if false then print_s [%sexp (lexiquepost90_to_erofa : (string * string) list)];
  let combined_erofa = combined_erofa erofa lexiquepost90_to_erofa post90 in
  if header then print_endline [%string "old,new"];
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

let gen ~env ~rules ~all ~write ~diff =
  let root = root ~from:(Eio.Stdenv.fs env) in
  match
    with_flow ~env ~write ~diff ~f:(fun buf ->
        let print =
          if all
          then (fun old new_ ->
                let mod_ = if String.(=) old new_ then "=" else "M" in
                Eio.Buf_write.string buf [%string "%{old},%{new_},%{mod_}\n"])
          else (
            let seen = Hash_set.create (module String) in
            fun old new_ ->
            match Hash_set.strict_add seen old with
            | Error _ -> ()
            | Ok () ->
               if String.(<>) old new_
               then Eio.Buf_write.string buf [%string "%{old},%{new_}\n"])
        in
        let stats = Rewrite.gen ~root ~rules print in
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
     and+ all = C.Arg.value (C.Arg.flag (C.Arg.info ~doc:"inclure les mots inchangés" ["all"]))
     and+ write =
       C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.string) None
                      (C.Arg.info ~doc:"écrire le dictionnaire dans le fichier spéficié" ["write"]))
     and+ diff =
       C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.string) None
                      (C.Arg.info ~doc:"diff le dictionnaire avec le fichier spéficié" ["diff"]))
     in
     Eio_main.run (fun env ->
         try gen ~env ~rules ~all ~write ~diff
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
                   let post90 = load_post90 ~root in
                   build_lexique_post90 lexique post90
                 else lexique
               in
               Rules.check lexique ~skip:(Rewrite.load_skip ())))
      ; gen_cmd "gen"
      ; C.Cmd.v (C.Cmd.info "erofa-ext")
          (let+ header = C.Arg.value (C.Arg.flag (C.Arg.info ["header"])) in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               build_erofa_ext ~root ~header))
      ]
  in
  C.Cmd.eval cmd |> exit

