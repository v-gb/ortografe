open Base

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

let build_erofa_ext ~erofa ~post90 ~lexique =
  (* start with whole erofa db, so [simplify_mapping] considers singular in the erofa csv *)
  let base = Hashtbl.map erofa ~f:(fun data -> data, -1) in
  ignore (
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

type static =
  { data_lexique_Lexique383_gen_tsv : string
  ; extension_dict1990_gen_csv : string }

let gen ~rules ~rect90 ~all ~oe ~output ~json_to_string data =
  let post90, lexique =
    match data with
    | `Values (`Post90 post90, `Lexique lexique) -> post90, lexique
    | `Static { data_lexique_Lexique383_gen_tsv; extension_dict1990_gen_csv } ->
       Data.parse_post90 extension_dict1990_gen_csv,
       Data.Lexique.parse data_lexique_Lexique383_gen_tsv
  in
  let rect90 =
    rect90
    || List.is_empty rules
    || List.exists rules ~f:(fun r -> String.(=) (Rewrite.name r) "erofa")
  in
  let post90, lexique =
    (if rect90 then post90 else Hashtbl.create (module String)),
    build_lexique_post90 lexique post90 ~fix_90:rect90
  in
  let print, after =
    if all
    then (fun old new_ ->
      let mod_ = if String.(=) old new_ then "=" else "M" in
      output [%string "%{old},%{new_},%{mod_}\n"]),
         ignore
    else (
      let all = Hashtbl.create (module String) ~size:(List.length lexique) in
      (fun old new_ -> add_ranked all ~key:old ~data:new_),
      (fun () ->
        add_post90_entries all post90;
        let plurals_in_s = List.for_all rules ~f:Rewrite.plurals_in_s in
        simplify_mapping all ~plurals_in_s;
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
         |> json_to_string
         |> (fun s -> s ^ "\n")
         |> output
        );
        List.iter (ranked all) ~f:(fun (old, new_) ->
            output [%string "%{old},%{new_}\n"])))
  in
  let stats =
    Rewrite.gen
      ~not_understood:`Ignore
      ~fix_oe:oe
      ~rules
      lexique print in
  after ();
  `Stats [%sexp ~~(stats : Rewrite.stats)]
