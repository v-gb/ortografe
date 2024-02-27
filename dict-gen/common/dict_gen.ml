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

let change_from_1990_is_undesirable post90 =
  (* filter out changes such as chariotage->charriotage. In principle, this only make
     sense if we're simplifying h or double consonants. Close enough for now. *)
  String.is_prefix post90 ~prefix:"charr"
  || String.is_prefix post90 ~prefix:"combatt"
  || String.is_prefix post90 ~prefix:"hindou" (* don't understand if the h is really
                                                 dropped here? *)
  || String.(=) post90 "reboursouffler"
  
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
            if change_from_1990_is_undesirable post90
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

type embedded =
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
let of_name =
  let tbl =
    lazy (Hashtbl.of_alist_exn (module String)
            (List.map (force all) ~f:(fun rule -> name rule, rule)))
  in
  fun n -> Hashtbl.find (force tbl) n

let doc = function
  | `Rewrite name -> Rewrite.doc (Map.find_exn (force rewrite_rule_by_name) name)
  | `Oe -> "Corriger les @oe en @œ, comme @coeur -> @cœur"
  | `Rect1990 -> "Appliquer les rectifications de 1990"
let html_doc =
  let re =
    lazy (
        Re.(compile (seq [ str "@"; rep (compl [set " ->@,."]) ])),
        Re.(compile (seq [ str "http"; rep (compl [set " ,"]) ]))
      ) in
  fun rule ->
  let re_word, re_url = force re in
  doc rule
  |> String.substr_replace_all
       ~pattern:"->"
       ~with_:"→"
  |> Re.replace re_word ~all:true ~f:(fun group ->
         let word = String.chop_prefix_exn (Re.Group.get group 0) ~prefix:"@" in
         [%string "<i>%{word}</i>"])
  |> Re.replace re_url ~all:true ~f:(fun group ->
         let url = Re.Group.get group 0 in
         let display_url =
           url
           |> String.chop_prefix_if_exists ~prefix:"http://"
           |> String.chop_prefix_if_exists ~prefix:"https://"
           |> String.chop_prefix_if_exists ~prefix:"www."
           |> String.chop_suffix_if_exists ~suffix:"/"
         in
         [%string "<a href=\"%{url}\">%{display_url}</a>"])

let html ~id_prefix ~name_prefix rule =
  String.strip [%string {|
<input type="checkbox" id="%{id_prefix ^ name rule}" name="%{name_prefix ^ name rule}">
<label for="%{id_prefix ^ name rule}"><span><strong>%{name rule}</strong> %{html_doc rule}</span></label>
|}]

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

type metadata =
  { desc : string option
  ; lang : string option
  ; supports_repeated_rewrites : bool option
  ; plurals_in_s : bool option
  }
let no_metadata =
  { desc = None
  ; lang = None
  ; supports_repeated_rewrites = None
  ; plurals_in_s = None
  }
let metadata_of_json (json : _ json) =
  let desc = ref None in
  let lang = ref None in
  let supports_repeated_rewrites = ref None in
  let plurals_in_s = ref None in
  (match json with
   | `Assoc l ->
      List.iter l ~f:(function
          | "desc", `String s -> desc := Some s
          | "lang", `String s -> lang := Some s
          | "supports_repeated_rewrites", `Bool b -> supports_repeated_rewrites := Some b
          | "plurals_in_s", `Bool b -> plurals_in_s := Some b
          | _ -> ())
   | _ -> ());
  { desc = !desc
  ; lang = !lang
  ; supports_repeated_rewrites = !supports_repeated_rewrites
  ; plurals_in_s = !plurals_in_s
  }

let json_of_metadata t =
  `Assoc
    (List.filter_opt
       [ Option.map t.desc ~f:(fun s -> "desc", `String s)
       ; Option.map t.lang ~f:(fun s -> "lang", `String s)
       ; Option.map t.supports_repeated_rewrites ~f:(fun b -> "supports_repeated_rewrites", `Bool b)
       ; Option.map t.plurals_in_s ~f:(fun b -> "plurals_in_s", `Bool b)
       ])

let interpret_rules rules =
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
  let metadata =
    let name =
      List.map rules ~f:name
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:" "
    in
    let plurals_in_s = List.for_all rewrite_rules ~f:Rewrite.plurals_in_s in
    { desc = Some name
    ; lang = Some "fr"
    ; supports_repeated_rewrites =
        Some (List.for_all rewrite_rules ~f:Rewrite.supports_repeated_rewrites)
    ; plurals_in_s = Some plurals_in_s
    }
  in
  rewrite_rules, oe, rect1990, metadata

let gen ?(profile = false) ~rules ~all ~output ~json_to_string data =
  let rewrite_rules, oe, rect1990, metadata = interpret_rules rules in
  let plurals_in_s = metadata.plurals_in_s ||? failwith "missing plurals_in_s" in
  let post90, lexique =
    match data with
    | `Values { post90; lexique } -> post90, lexique
    | `Embedded { data_lexique_Lexique383_gen_tsv; extension_dict1990_gen_csv } ->
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
        simplify_mapping all ~plurals_in_s;
        (json_of_metadata metadata
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

let staged_gen data =
  let rect1990 = false (* Simpler for now. We can do the erofa or 1990 rewriting with the
                          regular [gen] function, which can handle this. *) in
  let post90, lexique =
    match data with
    | `Values { post90; lexique } -> post90, lexique
    | `Embedded { data_lexique_Lexique383_gen_tsv; extension_dict1990_gen_csv } ->
       Data.parse_post90 extension_dict1990_gen_csv,
       Data.Lexique.parse data_lexique_Lexique383_gen_tsv
  in
  let lexique = build_lexique_post90 lexique post90 ~rect1990 in
  let table = Hashtbl.create (module String) ~size:(List.length lexique) in
  List.iter lexique ~f:(fun row ->
      ignore (Hashtbl.add table ~key:row.ortho ~data:row : [ `Ok | `Duplicate ]));
  fun rules ->
    let rewrite_rules, _oe, _rect1990, metadata = interpret_rules rules in
    let cache = Hashtbl.create (module String) ~size:200 in
    let staged = Rewrite.staged_gen ~rules:rewrite_rules () in
    (fun ortho ->
      match Hashtbl.find cache ortho with
      | Some opt -> opt
      | None ->
         match Hashtbl.find table ortho with
         | None -> None (* maybe we should handle the oe and the rect1990 case here: if the word
                           contains œ, rewrite that to oe, and retry the lookup in [table]. For
                           rect1990, it should be the same thing if we can pull out the body
                           of add_post90_entries and call it here. *)
         | Some row ->
            let new_ortho = staged row in
            let opt =
              if phys_equal ortho new_ortho || String.(=) ortho new_ortho
              then None
              else Some new_ortho
            in
            Hashtbl.add_exn cache ~key:ortho ~data:opt;
            opt),
    metadata

let parse str ~json_of_string =
  let lines = String.split_lines str in
  let lines, metadata =
    match lines with
    | first :: lines when String.is_prefix first ~prefix:"{" ->
       let json = json_of_string first in
       let metadata = metadata_of_json json in
       lines, metadata
    | _ -> lines, no_metadata
  in
  let h = Stdlib.Hashtbl.create (List.length lines) in
  List.iter lines ~f:(fun l ->
      match String.split ~on:',' l with
      | [ a; b ] -> Stdlib.Hashtbl.replace h a b
      | _ -> failwith (Printf.sprintf
                         "didn't get exactly 2 items in line %S" l));
  Stdlib.Hashtbl.find_opt h, metadata

