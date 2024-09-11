open Core
module Data = Dict_gen_common.Data
module Rules = Dict_gen_common.Rules
module Rewrite = Dict_gen_common.Rewrite
module Dict_gen = Dict_gen_common.Dict_gen
module Unix = Core_unix

let ( ^/ ) = Eio.Path.( / )

let root ~from =
  let ( / ) = Eio.Path.( / ) in
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

type string_sp = string

let equal_string_sp s1 s2 = String.( = ) (s1 ^ "s") s2

let create_dict_search ~post90 ~(lexique : Data.Lexique.t) ~combined_erofa =
  let regular_plurals =
    let h =
      Hashtbl.of_alist_multi
        (module String)
        (List.map lexique ~f:(fun x -> (x.lemme, x)))
    in
    Of_iter.list (fun yield ->
        Hashtbl.iter h ~f:(fun data ->
            let data =
              List.map data ~f:(fun d -> (d.ortho, d.phon))
              |> List.dedup_and_sort ~compare:[%compare: string * string]
            in
            (* Le test de longueur évite d'attraper des verbes genre etudiai,etudiais. *)
            if List.length data <= 4
            then
              let rec loop = function
                | [] -> ()
                | e1 :: tl ->
                    (match
                       List.find tl ~f:(fun e2 ->
                           String.( = ) (snd e1) (snd e2)
                           && equal_string_sp (fst e1) (fst e2))
                     with
                    | None -> ()
                    | Some e2 ->
                        let conflicts = ref false in
                        yield (fst e1, (conflicts, (`S, fst e2)));
                        yield (fst e2, (conflicts, (`P, fst e1))));
                    loop tl
              in
              loop data))
    |> Map.of_alist_multi (module String)
    |> (fun m ->
         Map.iter m ~f:(function
           | [] | [ _ ] -> ()
           | _ :: _ :: _ as l ->
               List.iter l ~f:(fun (conflicts, _) -> conflicts := true));
         m)
    |> Map.filter_map ~f:(function
         | [ (conflicts, s) ] -> if !conflicts then None else Some s
         | _ -> None)
  in
  let special_cases =
    [ ("héro", "éro (drogue)")
    ; ("héroïne", "éroïne (drogue), héroïne")
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
    ; ("barillet", "barilet, barillet")
    ; ("hyène", "hiène, iène")
    ; ("malachite", "malaquite, malachite")
    ; ("sakieh", "sakiè, sakié")
    ; ("surrénal", "surénal, surrénal")
    ; ("surrénale", "surénale, surrénale")
    ; ("surrénalien", "surénalien, surrénalien")
    ; ("surrénalienne", "surénaliène, surrénaliène")
    ; ("surrénalite", "surénalite, surrénalite")
    ; ("surrénaux", "surénaus, surrénaus")
    ; ("tyrolienne", "tiroliène (nom), tyroliène (adj)")
    ; ("œcuménicité", "eucuménicité, écuménicité")
    ; ("œcuménique", "eucuménique, écuménique")
    ; ("œcuménisme", "eucuménisme, écuménisme")
    ; ("œdème", "eudème, édème")
    ]
    |> List.concat_map ~f:(fun (k, v) ->
           let k' = String.substr_replace_all k ~pattern:"œ" ~with_:"oe" in
           if String.( = ) k k' then [ (k, v) ] else [ (k, v); (k', v) ])
    |> Hashtbl.of_alist_exn (module String)
  in
  let oe_pattern = Core.String.Search_pattern.create "œ" in
  let discard_oe l =
    (* Les oe sont déjà gérés séparement dans la recherche, pas besoin d'avoir
       tous les entrées en double en plus. *)
    let to_drop =
      Of_iter.list (fun yield ->
          List.iter l ~f:(fun (a, _) ->
              let a' =
                Core.String.Search_pattern.replace_all oe_pattern ~in_:a ~with_:"oe"
              in
              if String.( <> ) a a' then yield a'))
      |> Set.of_list (module String)
    in
    List.filter l ~f:(fun (a, _) -> not (Set.mem to_drop a))
  in
  let fail = ref false in
  let data_keyed_by_post90 =
    (* Dans le site, il est bruyant de voir des lignes en doubles
         du genre :

         interpella -> interpela -> -
         interpela - >     -     -> -

         Donc pour éviter ça, on supprime la deuxième ligne en
         considerant que l'information de la première suffit.
    *)
    Of_iter.list (List.iter combined_erofa ~f:__)
    |> discard_oe
    |> List.map ~f:(fun (pre_erofa, erofa) ->
           let post90 = Hashtbl.find post90 pre_erofa ||? pre_erofa in
           (post90, (pre_erofa, erofa)))
    |> Map.of_alist_multi (module String)
    |> Map.mapi ~f:(fun ~key:post90 ~data:l ->
           (* On a souvent multiples orthographes Érofa car combined_erofa contient des
              entrées du style apparaître->aparaitre and appara*i*tre->aparaitre, et
              donc erofas contient les deux valeurs à droite des flèches. Donc on
              regroupe par orthographe Érofa. *)
           let l2 =
             List.map l ~f:Tuple.T2.swap
             |> Map.of_alist_multi (module String)
             |> Map.map ~f:(fun l ->
                    List.filter l ~f:(fun s -> String.( <> ) s post90)
                    |> List.dedup_and_sort ~compare:String.compare)
             |> Map.to_alist
           in
           if List.length l2 > 1
           then (
             Printf.eprintf "%s\n%!"
               (Sexp_with_utf8.to_string_hum
                  [%sexp
                    "Incohérence entre Érofa et 1990"
                    , (post90 : string)
                    , (l2 : (string * string list) list)]);
             fail := true);
           List.map l2 ~f:Tuple.T2.swap)
    |> (fun m ->
         Map.mapi m ~f:(fun ~key:post90 ~data ->
             let len_1 = List.length data = 1 in
             List.filter_map data ~f:(fun data ->
                 let data0 = (fst data, snd data, 0) in
                 if not len_1
                 then Some data0
                 else
                   match Map.find regular_plurals post90 with
                   | None -> Some data0
                   | Some (sp, other) -> (
                       match Map.find m other with
                       | None | Some [] | Some (_ :: _ :: _) -> Some data0
                       | Some [ data2 ] ->
                           let data_s, data_p =
                             match sp with `S -> (data, data2) | `P -> (data2, data)
                           in
                           if [%equal: string_sp list * string_sp] data_s data_p
                           then
                             match sp with
                             | `S -> Some (fst data, snd data, 1)
                             | `P -> None
                           else Some data0))))
    |> Map.mapi ~f:(fun ~key:post90 ~data ->
           List.map data ~f:(fun ((pre_erofas, _, flags) as v) ->
               match Hashtbl.find special_cases post90 with
               | Some erofa' -> (pre_erofas, erofa', flags)
               | None -> v))
    |> Map.to_alist
    |> List.concat_map ~f:(fun (post90, l) ->
           List.map l ~f:(fun (a, b, flags) -> (a, post90, b, flags)))
  in
  if !fail then assert false;
  let q = Queue.create () in
  let noop = ref 0 in
  let dict_search =
    Dict_search.create (fun yield ->
        List.iter data_keyed_by_post90 ~f:(fun (as_, b, c, flags) ->
            assert (flags = 0 || flags = 1);
            let yield =
              if flags = 1
              then (fun a b ->
                yield a b;
                yield (if a >= 0 then a else a - 2) (b ^ "s"))
              else yield
            in
            if String.( = ) b c && List.is_empty as_
            then (
              noop := !noop + 1;
              yield (-((4 * !noop) + flags)) c)
            else
              let n = Queue.length q in
              Queue.enqueue q (as_, b, c, flags);
              List.iter as_ ~f:(fun a -> yield n a);
              yield n b;
              yield n c))
  in
  (Queue.to_array q, dict_search)

let build_erofa_ext ~root ~dict_search =
  let post90 = Data_fs.load_post90 (`Root root) in
  let lexique = Data_fs.load_lexique (`Root root) in
  let combined_erofa =
    Dict_gen.build_erofa_ext
      ~erofa:(Data_fs.load_erofa (`Root root))
      ~post90 ~lexique ~all:dict_search
  in
  if dict_search
  then
    Dict_search.Erofa.to_persist (create_dict_search ~post90 ~lexique ~combined_erofa)
    |> Printf.sprintf "%S"
    |> print_string
  else
    List.iter combined_erofa ~f:(fun (old, new_) ->
        print_endline [%string "%{old},%{new_}"])

let with_flow ~env ~write ~diff ~drop ~f =
  match (write, diff) with
  | Some _, Some _ -> failwith "can't specify both --write and --diff"
  | Some fname, None ->
      Eio.Path.with_open_out
        (Eio.Path.( / ) (Eio.Stdenv.fs env) fname)
        ~create:(`Or_truncate 0o666)
        (fun flow -> Eio.Buf_write.with_flow flow f);
      None
  | None, Some fname ->
      let tmp_fname = fname ^ ".tmp" in
      Eio.Path.with_open_out
        (Eio.Path.( / ) (Eio.Stdenv.fs env) tmp_fname)
        ~create:(`Or_truncate 0o666)
        (fun flow -> Eio.Buf_write.with_flow flow f);
      Some (`Diff (fname, tmp_fname))
  | None, None ->
      if (not drop) && Unix.isatty Unix.stdout
      then
        Eio.Switch.run (fun sw_pr ->
            let process =
              Eio.Switch.run (fun sw_fd ->
                  let r, w = Eio.Process.pipe ~sw:sw_fd (Eio.Stdenv.process_mgr env) in
                  let process =
                    Eio.Process.spawn ~sw:sw_pr (Eio.Stdenv.process_mgr env) ~stdin:r
                      [ "less" ]
                  in
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
    Data_fs.load_lexique
      (match embedded with
      | None -> `Root (force root)
      | Some r -> `Str r.data_lexique_Lexique383_gen_tsv)
  in
  let post90 =
    Data_fs.load_post90
      (match embedded with
      | None -> `Root (force root)
      | Some r -> `Str r.extension_dict1990_gen_csv)
  in
  match
    with_flow ~env ~write ~diff ~drop ~f:(fun buf ->
        let write = if drop then ignore else fun str -> Eio.Buf_write.string buf str in
        let (`Stats stats) =
          Dict_gen.gen ~rules ~all ~json_to_string:Yojson.to_string ~output:write
            (`Values { post90; lexique })
        in
        if Unix.isatty Unix.stderr then eprint_s stats)
  with
  | None -> ()
  | Some (`Diff (a, b)) ->
      Eio.Process.run (Eio.Stdenv.process_mgr env)
        [ "bash"
        ; "-e"
        ; "-u"
        ; "-o"
        ; "pipefail"
        ; "-c"
        ; Sys.concat_quoted [ "patdiff"; "-context"; "1"; a; b ] ^ " | less -R"
        ]
        ~is_success:(function 0 | 1 -> true | _ -> false)

let gen_cmd ?embedded ?doc name =
  let module C = Cmdliner in
  let open Cmdliner.Term.Syntax in
  C.Cmd.v (C.Cmd.info ?doc name)
    (let+ rules = Dict_gen_nonbrowser.rules_cli ()
     and+ all =
       C.Arg.value (C.Arg.flag (C.Arg.info ~doc:"inclure les mots inchangés" [ "all" ]))
     and+ write =
       C.Arg.value
         (C.Arg.opt (C.Arg.some C.Arg.string) None
            (C.Arg.info ~doc:"écrire le dictionnaire dans le fichier spécifié"
               [ "write" ]))
     and+ diff =
       C.Arg.value
         (C.Arg.opt (C.Arg.some C.Arg.string) None
            (C.Arg.info ~doc:"diff le dictionnaire avec le fichier spécifié" [ "diff" ]))
     and+ drop =
       C.Arg.value
         (C.Arg.flag
            (C.Arg.info ~doc:"(pour profiler) jeter le dictionnaire calculé" [ "drop" ]))
     in
     Eio_main.run (fun env ->
         try gen ?embedded ~env ~rules ~all ~write ~diff ~drop ()
         with
         | Eio.Exn.Io
             (Eio.Net.E (Connection_reset (Eio_unix.Unix_error (EPIPE, _, _))), _)
         ->
           ()))

let check (lexique : Data.Lexique.t) ~skip =
  let rules = Rules.create () in
  List.iteri lexique ~f:(fun i row ->
      if skip row
      then ()
      else
        match Rules.search rules row.ortho row.phon with
        | Error s -> prerr_endline (Sexp_with_utf8.to_string_hum ~which:`All s)
        | Ok search_res -> printf "%d %s%!" i (Rules.to_string search_res))

let main () =
  Sexp_with_utf8.linkme;
  let module C = Cmdliner in
  let open Cmdliner.Term.Syntax in
  let cmd =
    C.Cmd.group (C.Cmd.info "dict-gen")
      [ C.Cmd.v (C.Cmd.info "check-rules")
          (let+ post90 = C.Arg.value (C.Arg.flag (C.Arg.info [ "90" ])) in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               let lexique = Data_fs.load_lexique (`Root root) in
               let lexique =
                 if post90
                 then
                   let post90 = Data_fs.load_post90 (`Root root) in
                   Dict_gen.build_lexique_post90 lexique post90
                     ~rect1990:false (* to check both versions *)
                 else lexique
               in
               check lexique ~skip:(Rewrite.load_skip ())))
      ; gen_cmd "gen"
      ; C.Cmd.v (C.Cmd.info "erofa-ext")
          (let+ dict_search =
             C.Arg.value
               (C.Arg.flag
                  (C.Arg.info ~doc:"afficher un Dict_search.t persisté"
                     [ "dict-search" ]))
           in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               build_erofa_ext ~root ~dict_search))
      ]
  in
  C.Cmd.eval cmd |> exit
