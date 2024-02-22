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

let build_lexique_post90 (lexique : Data.Lexique.t) post90 ~rect1990 =
  (* this causes a few regressions like
     allécherait,alécherait
     becomes
     allécherait,allècherait
     because the change of direction in accent is actually not taken into account
     in the prononciation, and so the erofa rewriting fails to apply.

     Or even graffito -> grafito becoming graffito -> graffiti.
   *)
  if rect1990
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
      let lexique_post90 = build_lexique_post90 lexique post90 ~rect1990:true in
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
  ; extension_dict1990_gen_csv : string
  }
type values =
  { post90 : (string, string) Hashtbl.t
  ; lexique : Data.Lexique.t
  }

type rule = [ `Oe | `Rect1990 | `Rewrite of string ]
let rewrite_rule_by_name =
  lazy (
      List.map (force Rewrite.all)
        ~f:(fun r -> Rewrite.name r, r)
      |> Map.of_alist_exn (module String))
let all = lazy (`Oe
                :: `Rect1990
                :: List.map ~f:(fun r -> `Rewrite (Rewrite.name r))
                     (force Rewrite.all))

let name = function
  | `Rewrite name -> name
  | `Oe -> "oe"
  | `Rect1990 -> "1990"
let doc = function
  | `Rewrite name -> Rewrite.doc (Map.find_exn (force rewrite_rule_by_name) name)
  | `Oe -> "Corriger les @oe en @œ, comme @coeur -> @cœur"
  | `Rect1990 -> "Appliquer les rectifications de 1990"

let time ~profile name f =
  if profile then (
    let t1 = Stdlib.Sys.time () in
    let x = f () in
    let t2 = Stdlib.Sys.time () in
    Stdlib.prerr_endline [%string "%{name}: %{Float.to_string_hum ~decimals:2 (t2 -. t1)}s"];
    x
  ) else f ()

type 'a json =
  [> `Assoc of (string * 'a json) list
  | `Bool of bool
  | `String of string ] as 'a

let gen ?(profile = false) ~rules ~all ~output ~json_to_string data =
  let rules = if List.is_empty rules then [ `Rewrite "erofa"; `Rect1990; `Oe ] else rules in
  let rewrite_rules, oe, rect1990 =
    let oe = ref false in
    let rect1990 = ref false in
    let rules =
      List.filter_map rules ~f:(function
          | `Rewrite s -> Some (Map.find_exn (force rewrite_rule_by_name) s)
          | `Oe -> oe := true; None
          | `Rect1990 -> rect1990 := true; None)
    in
    rules, !oe, !rect1990
  in
  let post90, lexique =
    match data with
    | `Values { post90; lexique } -> post90, lexique
    | `Static { data_lexique_Lexique383_gen_tsv; extension_dict1990_gen_csv } ->
       time ~profile "read1990" (fun () ->
           Data.parse_post90 extension_dict1990_gen_csv),
       time ~profile "readerofa" (fun () ->
           Data.Lexique.parse data_lexique_Lexique383_gen_tsv)
  in
  let post90, lexique =
    time ~profile "post1990lexique" (fun () ->
        (if rect1990 then post90 else Hashtbl.create (module String)),
        build_lexique_post90 lexique post90 ~rect1990)
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
        let plurals_in_s = List.for_all rewrite_rules ~f:Rewrite.plurals_in_s in
        simplify_mapping all ~plurals_in_s;
        (let name =
           List.map rules ~f:name
           |> List.sort ~compare:String.compare
           |> String.concat ~sep:" "
         in
         `Assoc
           [ "desc", `String name
           ; "lang", `String "fr"
           ; "supports_repeated_rewrites",
             `Bool (List.for_all rewrite_rules ~f:Rewrite.supports_repeated_rewrites)
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
    time ~profile "rewrite" (fun () ->
    Rewrite.gen
      ~not_understood:`Ignore
      ~fix_oe:oe
      ~rules:rewrite_rules
      lexique print)
  in
  time ~profile "post-computation" (fun () ->
      after ());
  `Stats [%sexp ~~(stats : Rewrite.stats)]
