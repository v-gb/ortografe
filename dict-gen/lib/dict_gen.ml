open Core
module Data = Dict_gen_common.Data
module Rules = Dict_gen_common.Rules
module Rewrite = Dict_gen_common.Rewrite
module Dict_gen = Dict_gen_common.Dict_gen
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

let build_erofa_ext ~root ~dict_search =
  let combined_erofa =
    Dict_gen.build_erofa_ext
      ~erofa:(Data_fs.load_erofa (`Root root))
      ~post90:(Data_fs.load_post90 (`Root root))
      ~lexique:(Data_fs.load_lexique (`Root root))
      ~all:dict_search
  in
  if dict_search
  then
    let special_cases =
      [ "héro", "éro (drogue)"
      ; "héroïne", "éroïne (drogue), héroïne"
      (* The DOR sometimes contains several entries (like barillet below), or entries
         that make sense in specifics sense (like héroïne) above, which the rewriting
         tools can't use. But for dict-search, it can make sense to keep them.

         They come from this DOR query, or seeing what gets filtered out in
         extension/import-dict:
         select old,group_concat(new)
         from (select distinct "Grafie initiale" as old, "Grafie modifiée" as new from t)
         group by old
         having count( * ) > 1
         order by old;
       *)
      ; "barillet", "barilet, barillet"
      ; "hyène", "hiène, iène"
      ; "malachite", "malaquite, malachite"
      ; "sakieh", "sakiè, sakié"
      ; "surrénal", "surénal, surrénal"
      ; "surrénale", "surénale, surrénale"
      ; "surrénalien", "surénalien, surrénalien"
      ; "surrénalienne", "surénaliène, surrénaliène"
      ; "surrénalite", "surénalite, surrénalite"
      ; "surrénaux", "surénaus, surrénaus"
      ; "tyrolienne", "tyroliène, tiroliène"
      ; "œcuménicité", "eucuménicité, écuménicité"
      ; "œcuménique", "eucuménique, écuménique"
      ; "œcuménisme", "eucuménisme, écuménisme"
      ; "œdème", "eudème, édème"
      ]
      |> List.concat_map ~f:(fun (k, v) ->
             let k' = String.substr_replace_all k ~pattern:"œ" ~with_:"oe" in
             if String.(=) k k'
             then [ (k, v) ]
             else [ (k, v); (k', v) ])
      |> Hashtbl.of_alist_exn (module String)
    in
    Dict_search.create
      (fun f -> List.iter combined_erofa ~f:(fun (a, b) ->
                    match Hashtbl.find special_cases a with
                    | Some b' -> f a b'
                    | None -> f a b))
    |> Dict_search.to_persist
    |> print_string
  else
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

let gen ~env ?(embedded : Dict_gen.embedded option) ~rules ~all ~write ~diff ~drop () =
  let root = lazy (root ~from:(Eio.Stdenv.fs env)) in
  let lexique =
    Data_fs.load_lexique (match embedded with
                           | None -> `Root (force root)
                           | Some r -> `Str r.data_lexique_Lexique383_gen_tsv)
  in
  let post90 =
    Data_fs.load_post90 (match embedded with
                         | None -> `Root (force root)
                         | Some r -> `Str r.extension_dict1990_gen_csv)
  in
  match
    with_flow ~env ~write ~diff ~drop ~f:(fun buf ->
        let write = if drop then ignore else (fun str -> Eio.Buf_write.string buf str) in
        let `Stats stats =
          Dict_gen.gen
            ~rules
            ~all
            ~json_to_string:Yojson.to_string
            ~output:write
            (`Values { post90; lexique })
        in
        if Unix.isatty Unix.stderr then
          eprint_s stats
      )
  with
  | None -> ()
  | Some (`Diff (a, b)) ->
     Eio.Process.run (Eio.Stdenv.process_mgr env)
       [ "bash"; "-e"; "-u"; "-o"; "pipefail"; "-c"
       ; Sys.concat_quoted [ "patdiff"; "-context"; "1"; a; b ] ^ " | less -R"
       ]
       ~is_success:(function 0 | 1 -> true | _ -> false)

let gen_cmd ?embedded ?doc name =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  C.Cmd.v (C.Cmd.info ?doc name)
    (let+ rules = Dict_gen_nonbrowser.rules_cli ()
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
         try gen ?embedded ~env ~rules ~all ~write ~diff ~drop ()
         with Eio.Exn.Io (Eio.Net.E (Connection_reset (Eio_unix.Unix_error (EPIPE, _, _))), _) ->
           ()))

let check (lexique : Data.Lexique.t) ~skip =
  let rules = Rules.create () in
  List.iteri lexique ~f:(fun i row ->
      if skip row
      then ()
      else
        match Rules.search rules row.ortho row.phon with
        | Error s -> prerr_endline (Sexp_with_utf8.to_string_hum s)
        | Ok search_res -> printf "%d %s%!" i (Rules.to_string search_res)
    )

let main () =
  Sexp_with_utf8.linkme;
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
                   Dict_gen.build_lexique_post90
                     lexique
                     post90
                     ~rect1990:false (* to check both versions *)
                 else lexique
               in
               check lexique ~skip:(Rewrite.load_skip ())))
      ; gen_cmd "gen"
      ; C.Cmd.v (C.Cmd.info "erofa-ext")
          (let+ () = return ()
           and+ dict_search = C.Arg.value (C.Arg.flag (C.Arg.info ~doc:"afficher un Dict_search.t persisté" ["dict-search"]))
           in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               build_erofa_ext ~root ~dict_search))
      ]
  in
  C.Cmd.eval cmd |> exit
