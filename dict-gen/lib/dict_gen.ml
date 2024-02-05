open Core
module Unix = Core_unix
let (^/) = Eio.Path.(/)

let root ~from =
  let (/) = Eio.Path.(/) in
  let from = ref from in
  let i = ref 0 in
  while not (Eio.Path.is_file (!from / "dune-project")) do
    from := !from / "..";
    i := !i + 1;
    if !i > 1000 then failwith ("can't find repo root " ^ Eio.Path.native_exn !from)
  done;
  !from

let read_key_value_comma_sep_file f =
  Eio.Path.load f
  |> String.split_lines
  |> List.map ~f:(fun line ->
         match String.split line ~on:',' with
         | [a; b] -> (a, b)
         | _ -> raise_s [%sexp "bad line", (line : string)])
  |> Hashtbl.of_alist_multi (module String)
  |> Hashtbl.map ~f:(fun l -> List.last_exn l)

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
    Hashtbl.of_alist_multi (module String) lexiquepost90_to_erofa
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

let build_erofa_ext ~root =
  let erofa = load_erofa ~root in
  let post90 = load_post90 ~root in
  let lexique = Data_src.Lexique.load ~root () in
  let lexique_post90 = build_lexique_post90 lexique post90 in
  let lexiquepost90_to_erofa =
    let r = ref [] in
    Rewrite.gen ~root ~lexique:lexique_post90
      ~skip_not_understood:true
      (fun old new_ -> r := (old, new_) :: !r);
    List.rev !r
  in
  if false then print_s [%sexp (lexiquepost90_to_erofa : (string * string) list)];
  let combined_erofa = combined_erofa erofa lexiquepost90_to_erofa post90 in
  List.iter combined_erofa ~f:(fun (old, new_) ->
      print_endline [%string "%{old},%{new_}"])

let gen ?doc name =
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
     in
     Eio_main.run (fun env ->
         let root = root ~from:(Eio.Stdenv.fs env) in
         try
           Eio.Buf_write.with_flow (Eio.Stdenv.stdout env) (fun buf ->
               Rewrite.gen ~root ~rules (fun old new_ ->
                   if String.(<>) old new_
                   then Eio.Buf_write.string buf [%string "%{old},%{new_}\n"]))
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
      ; gen "gen"
      ; C.Cmd.v (C.Cmd.info "erofa-ext")
          (let+ () = return () in
           Eio_main.run (fun env ->
               let root = root ~from:(Eio.Stdenv.fs env) in
               build_erofa_ext ~root))
      ]
  in
  C.Cmd.eval cmd |> exit

