open Base

let simplify_mapping tbl ~plurals_in_s =
  (* Remove identity mappings and trivial plurals. *)
  let keep =
    match plurals_in_s with
    | None -> fun ~key:old ~data:(new_, _) -> String.( <> ) old new_
    | Some plural_marker ->
        let rec keep ~key:old ~data:(new_, _) =
          let is_id = String.( = ) old new_ in
          match
            ( String.chop_suffix old ~suffix:"s"
            , String.chop_suffix new_ ~suffix:plural_marker )
          with
          | Some old_no_s, Some new_no_s -> (
              match Hashtbl.find tbl old_no_s with
              | Some ((new', _) as data_no_s) when String.( = ) new' new_no_s ->
                  (* Plural is inferrable from singular. But we can't simply filter
                     out our entry, as the singular may have been filtered out
                     itself, with a mapping like (a->a, as->a) and plurals_in_s="",
                     hence the recursive call. *)
                  (not (keep ~key:old_no_s ~data:data_no_s)) && not is_id
              | Some _ ->
                  (* Keep the entry even if is_id, because the "singular" implies a
                     rewriting inconsistent with our entry. This would happen with a
                     mapping like (a->b, as->as). This is quite difficult to trigger
                     accidentally (sen, sens with ortograf.net would possibly do it),
                     but triggers ~15 times when the érofa dict and lexique disagree on
                     pronunciation (pupille->pupile in érofa, pupilles->pupilles from
                     lexique). This is technically better behavior, but it is confusing
                     to have a compression technique modify behavior like this. *)
                  true
              | None -> not is_id)
          | _ -> not is_id
        in
        keep
  in
  Hashtbl.filter_mapi_inplace tbl ~f:(fun ~key ~data ->
      if keep ~key ~data
      then Some (if String.( = ) key (fst data) then ("", snd data) else data)
      else None)

let change_from_1990_is_undesirable ~has_erofa post90 =
  has_erofa
  && ((* filter out changes such as chariotage->charriotage *)
      String.is_prefix post90 ~prefix:"charr"
     || String.is_prefix post90 ~prefix:"combatt"
     || String.is_prefix post90 ~prefix:"hindou"
        (* don't understand if the h is really
           dropped here? *)
     || String.( = ) post90 "reboursouffler")

let add_post90_entries base post90 ~has_erofa =
  Hashtbl.iteri post90 ~f:(fun ~key:pre90 ~data:post90 ->
      match Hashtbl.find base pre90 with
      | Some _ ->
          ()
          (* erofa dictionary contains pre-1990 spellings, like frisotter rather than
             frisoter *)
      | None -> (
          match Hashtbl.find base post90 with
          | Some (new_ortho, rank) ->
              (* keep rank so related words together. When rank < 0, the word comes from the erofa
                 dictionary and is going to be filtered out, so don't keep that. *)
              Hashtbl.add_exn base ~key:pre90
                ~data:(new_ortho, if rank >= 0 then rank else 100000000)
          | None ->
              if change_from_1990_is_undesirable post90 ~has_erofa
              then ()
              else
                (* we created fixed ranks to generated the same ordering as previous code *)
                Hashtbl.add_exn base ~key:pre90 ~data:(post90, 100000000)))

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
  (* The code might be simpler if we returned instead:
     List.concat_map lexique ~f:(fun r ->
        match Hashtbl.find post90 r.ortho with
        | None -> [ r ]
        | Some new_ortho ->
          let r_new = { r with ortho = new_ortho } in
          [ r.ortho, (if rect1990 then r_new else t)
          ; new_ortho, r_new
          ])

      This would increase computations a bit when applying 1990 rectifications (we'd try to
      convert both « maitre » and « maître » separately instead of once), but maybe that
      makes no difference in practice. *)
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

let build_erofa_ext ~erofa ~post90 ~lexique ~all =
  (* start with whole erofa db, so [simplify_mapping] considers singular in the erofa csv *)
  let base = Hashtbl.map erofa ~f:(fun data -> (data, -1)) in
  ignore
    (let lexique_post90 = build_lexique_post90 lexique post90 ~rect1990:true in
     Rewrite.gen ~rules:[ Rewrite.erofa ] ~not_understood:`Ignore ~fix_oe:true
       (* really only matters for proper nouns like Œdipe, since all other
            œ get removed *)
       lexique_post90 (fun old new_ -> add_ranked base ~key:old ~data:new_)
      : Rewrite.stats);
  add_post90_entries base post90 ~has_erofa:true;
  if not all
  then (
    simplify_mapping base ~plurals_in_s:(Some "s");
    Hashtbl.filter_inplace base ~f:(fun (_, rank) ->
        rank >= 0 (* i.e. "not from erofa" *)));
  ranked base

type embedded =
  { data_lexique_Lexique383_gen_tsv : string
  ; extension_dict1990_gen_csv : string
  }

type values =
  { post90 : (string, string) Hashtbl.t
  ; lexique : Data.Lexique.t
  }

(* This type must be serializable, because in javascript, it's passed from regular
   javascript to a webworker. As a result, we can't have [`Rewrite of Rewrite.rule]. *)
type rule =
  [ `Oe
  | `Rect1990
  | `Rewrite of [ `Builtin of string | `Custom of (string * string) list ]
  ]
[@@deriving compare]

type rules = rule list

let custom_rule s =
  (* we should probably normalize to NFC *)
  match
    String.prefix s 1000 (* reduce the chance of server problems *)
    |> String.split ~on:' '
    |> List.filter ~f:(String.( <> ) "")
    |> List.filter_map ~f:(String.lsplit2 ~on:'/')
    |> List.filter ~f:(function "", _ -> false | _ -> true)
    |> List.take __ 100 (* reduce the chance of server problems *)
  with
  | [] -> None
  | _ :: _ as l -> Some (`Rewrite (`Custom l))

let rewrite_rule_by_name_builtin =
  lazy
    (List.map (force Rewrite.all_builtin) ~f:(fun r -> (Rewrite.name r, r))
    |> Map.of_alist_exn (module String))

let all_builtin =
  lazy
    (`Oe
    :: `Rect1990
    :: List.map
         ~f:(fun r -> `Rewrite (`Builtin (Rewrite.name r)))
         (force Rewrite.all_builtin))

let name : rule -> _ = function
  | `Rewrite (`Builtin name) -> name
  | `Rewrite (`Custom l) -> Rewrite.name (Rewrite.custom_rule l)
  | `Oe -> "oe"
  | `Rect1990 -> "1990"

let of_name_builtin =
  let tbl =
    lazy
      (Hashtbl.of_alist_exn
         (module String)
         (List.map (force all_builtin) ~f:(fun rule -> (name rule, rule))))
  in
  fun n -> Hashtbl.find (force tbl) n

let doc = function
  | `Rewrite (`Builtin name) ->
      Rewrite.doc (Map.find_exn (force rewrite_rule_by_name_builtin) name)
  | `Rewrite (`Custom l) -> Rewrite.doc (Rewrite.custom_rule l)
  | `Oe -> "Corriger les @oe en @œ, comme @coeur -> @cœur"
  | `Rect1990 -> "Appliquer les rectifications de 1990"

let problems = function
  | `Rewrite (`Builtin name) ->
      Rewrite.problems (Map.find_exn (force rewrite_rule_by_name_builtin) name)
  | `Rewrite (`Custom _) | `Oe | `Rect1990 -> []

let html_doc =
  let re =
    lazy
      ( Re.(compile (seq [ str "@"; rep (compl [ set " ->@,." ]) ]))
      , Re.(compile (seq [ str "http"; rep (compl [ set " ," ]) ])) )
  in
  fun rule ->
    let re_word, re_url = force re in
    doc rule
    |> String.substr_replace_all ~pattern:"->" ~with_:"→"
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

let all_selection_html ~url_prefix ~id_prefix ~name_prefix ?(checked = Fn.const false)
    () =
  let builtin =
    List.map (force all_builtin) ~f:(fun rule ->
        let checked = if checked rule then " checked" else "" in
        String.strip
          [%string
            {|
<div>
  <input type="checkbox" id="%{id_prefix ^ name rule}" name="%{name_prefix ^ name rule}"%{checked}>
  <label for="%{id_prefix ^ name rule}"><span><strong>%{name rule}</strong> %{html_doc rule}</span></label>
</div>
|}])
    |> String.concat
  in
  let custom =
    let name = "custom" in
    [%string
      {|
<div>
  Autres
  <a style="background-color: #1e90ff; border-radius: 50%; color: white; font-weight: bold; text-align:center; display: inline-block; width: 1.3em; height: 1.3em; text-decoration: none;" href="%{url_prefix}rules-format.html" target="_blank">?</a>
  :
  <input type="text" id="%{id_prefix ^ name}" name="%{name_prefix ^ name}"
    placeholder="eaux/ôs eau/ô, par exemple"
    style="min-width:min(100%,30em)"
  >
</div> 
|}]
  in
  builtin ^ custom

let time ~profile name f =
  if profile
  then (
    let t1 = Stdlib.Sys.time () in
    let x = f () in
    let t2 = Stdlib.Sys.time () in
    Stdlib.prerr_endline
      [%string "%{name}: %{Float.to_string_hum ~decimals:2 (t2 -. t1)}s"];
    x)
  else f ()

type 'a json =
  [> `Assoc of (string * 'a json) list | `Bool of bool | `String of string ] as 'a

type metadata =
  { desc : string option
  ; lang : string option
  ; supports_repeated_rewrites : bool option
  ; plurals_in_s : string option option
  }

let no_metadata =
  { desc = None; lang = None; supports_repeated_rewrites = None; plurals_in_s = None }

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
        | "plurals_in_s", `Bool b -> plurals_in_s := Some (if b then Some "s" else None)
        | "plurals_in_s", `String s -> plurals_in_s := Some (Some s)
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
       [ Option.map t.desc ~f:(fun s -> ("desc", `String s))
       ; Option.map t.lang ~f:(fun s -> ("lang", `String s))
       ; Option.map t.supports_repeated_rewrites ~f:(fun b ->
             ("supports_repeated_rewrites", `Bool b))
       ; Option.map t.plurals_in_s ~f:(fun o ->
             ( "plurals_in_s"
             , match o with
               | None -> `Bool false
               | Some "s" -> `Bool true
               | Some s -> `String s ))
       ])

let merge_metadata_right_biased m1 m2 =
  { desc = Option.first_some m2.desc m1.desc
  ; lang = Option.first_some m2.lang m1.lang
  ; supports_repeated_rewrites =
      Option.first_some m2.supports_repeated_rewrites m1.supports_repeated_rewrites
  ; plurals_in_s = Option.first_some m2.plurals_in_s m1.plurals_in_s
  }

let interpret_rules rules =
  let erofa = `Builtin "erofa" in
  let rules =
    if List.is_empty rules then [ `Rewrite erofa; `Rect1990; `Oe ] else rules
  in
  let has_erofa = List.mem rules (`Rewrite erofa) ~equal:[%compare.equal: rule] in
  let rewrite_rules, oe, rect1990 =
    let oe = ref false in
    let rect1990 = ref false in
    let rules =
      List.filter_map rules ~f:(function
        | `Rewrite (`Builtin s) ->
            Some (Map.find_exn (force rewrite_rule_by_name_builtin) s)
        | `Rewrite (`Custom l) -> Some (Rewrite.custom_rule l)
        | `Oe ->
            oe := true;
            None
        | `Rect1990 ->
            rect1990 := true;
            None)
    in
    (rules, !oe, !rect1990)
  in
  let metadata =
    let name =
      List.map rules ~f:name
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:" "
    in
    let plurals_in_s =
      List.filter_map rewrite_rules ~f:Rewrite.plurals_in_s
      |> List.hd
      |> (__ ||? Some "s")
      (* until we roll this code to the safari extension, it seems best to avoid this *)
      |>
      function
      | (None | Some "s") as opt -> opt
      | _ -> None
    in

    { desc = Some name
    ; lang = Some "fr"
    ; supports_repeated_rewrites =
        Some (List.for_all rewrite_rules ~f:Rewrite.supports_repeated_rewrites)
    ; plurals_in_s = Some plurals_in_s
    }
  in
  (has_erofa, rewrite_rules, oe, rect1990, metadata)

let gen ?(profile = false) ?progress ~rules ~all ~output ~json_to_string data =
  let has_erofa, rewrite_rules, oe, rect1990, metadata = interpret_rules rules in
  let plurals_in_s = metadata.plurals_in_s ||? failwith "missing plurals_in_s" in
  let post90, lexique =
    match data with
    | `Values { post90; lexique } -> (post90, lexique)
    | `Embedded { data_lexique_Lexique383_gen_tsv; extension_dict1990_gen_csv } ->
        ( time ~profile "read1990" (fun () ->
              Data.parse_post90 extension_dict1990_gen_csv)
        , time ~profile "readerofa" (fun () ->
              Data.Lexique.parse data_lexique_Lexique383_gen_tsv) )
  in
  let post90, lexique =
    time ~profile "post1990lexique" (fun () ->
        ( (if rect1990 then post90 else Hashtbl.create (module String))
        , build_lexique_post90 lexique post90 ~rect1990 ))
  in
  let print, after =
    let all_tbl = Hashtbl.create (module String) ~size:(List.length lexique) in
    ( (fun old new_ -> add_ranked all_tbl ~key:old ~data:new_)
    , fun () ->
        add_post90_entries all_tbl post90 ~has_erofa;
        if not all
        then (
          simplify_mapping all_tbl ~plurals_in_s;
          json_of_metadata metadata |> json_to_string |> (fun s -> s ^ "\n") |> output);
        if all
        then
          List.iter (ranked all_tbl) ~f:(fun (old, new_) ->
              let mod_ = if String.( = ) old new_ then "=" else "M" in
              output [%string "%{old},%{new_},%{mod_}\n"])
        else
          List.iter (ranked all_tbl) ~f:(fun (old, new_) ->
              output [%string "%{old},%{new_}\n"]) )
  in
  Option.iter progress ~f:(fun f -> f 10);
  let stats =
    time ~profile "rewrite" (fun () ->
        Rewrite.gen
          ?progress:
            (Option.map progress ~f:(fun wrapper n -> wrapper (10 + (n * 8 / 10))))
          ~not_understood:`Ignore ~fix_oe:oe ~rules:rewrite_rules lexique print)
  in
  time ~profile "post-computation" (fun () -> after ());
  Option.iter progress ~f:(fun f -> f 100);
  `Stats [%sexp ~~(stats : Rewrite.stats)]

let staged_gen data =
  let post90, lexique =
    match data with
    | `Values { post90; lexique } -> (post90, lexique)
    | `Embedded { data_lexique_Lexique383_gen_tsv; extension_dict1990_gen_csv } ->
        ( Data.parse_post90 extension_dict1990_gen_csv
        , Data.Lexique.parse data_lexique_Lexique383_gen_tsv )
  in
  let lexique =
    build_lexique_post90 lexique post90 ~rect1990:false
    (* [rect1990:false] so the lexique has one entry for each of « maitre » and « maître ».
       When rewriting « maître » once we know the specific rules in effect, if we want to
       apply the 1990 rectifications, we will use [post90] to transform « maître » into
       « maitre » first. *)
  in
  let table = Hashtbl.create (module String) ~size:(List.length lexique) in
  List.iter lexique ~f:(fun row ->
      ignore
        (Hashtbl.add table ~key:row.ortho ~data:(`Lexique row) : [ `Ok | `Duplicate ]));
  (let pattern_oe = String.Search_pattern.create "oe" in
   Hashtbl.fold table ~init:[] ~f:(fun ~key:_ ~data acc ->
       match data with
       | `Lexique row ->
           if String.Search_pattern.matches pattern_oe row.ortho
           then
             (* This creates nonsense entries like cœxiste, but it doesn't matter as texts
                won't contain such words, and even if they did by mistake, rewrite.ml won't
                know how to explain their prononciation and so it won't touch them. *)
             let ortho =
               String.Search_pattern.replace_all pattern_oe ~in_:row.ortho ~with_:"œ"
             in
             (ortho, `Lexique { row with ortho }) :: acc
           else acc
       | `Rect1990 _ -> assert false)
   |> List.iter ~f:(fun (key, data) -> Hashtbl.add_exn table ~key ~data));
  Hashtbl.iteri post90 ~f:(fun ~key:pre90 ~data:post90 ->
      match Hashtbl.find table pre90 with
      | Some _ -> ()
      | None -> (
          match Hashtbl.find table post90 with
          | Some _ -> ()
          | None ->
              Hashtbl.add_exn table ~key:pre90
                ~data:
                  (`Rect1990
                    (post90, change_from_1990_is_undesirable post90 ~has_erofa:true))));
  fun rules ->
    let has_erofa, rewrite_rules, oe, rect1990, metadata = interpret_rules rules in
    let cache = Hashtbl.create (module String) ~size:200 in
    let staged = Rewrite.staged_gen ~fix_oe:oe ~rules:rewrite_rules () in
    ( (fun ortho ->
        match Hashtbl.find cache ortho with
        | Some opt -> opt
        | None -> (
            match Hashtbl.find table ortho with
            | None -> None
            | Some (`Rect1990 (post90, undesirable_if_erofa)) ->
                let opt =
                  if rect1990 && ((not undesirable_if_erofa) || not has_erofa)
                  then Some post90
                  else None
                in
                Hashtbl.add_exn cache ~key:ortho ~data:opt;
                opt
            | Some (`Lexique row) ->
                let new_ortho =
                  let row =
                    if rect1990
                    then
                      match Hashtbl.find post90 ortho with
                      | None -> row
                      | Some post90_ortho -> { row with ortho = post90_ortho }
                    else row
                  in
                  staged row
                in
                let opt =
                  if phys_equal ortho new_ortho || String.( = ) ortho new_ortho
                  then None
                  else Some new_ortho
                in
                Hashtbl.add_exn cache ~key:ortho ~data:opt;
                opt))
    , metadata )

let parse str ~json_of_string =
  let lines = String.split_lines str in
  let lines, metadata =
    match lines with
    | first :: lines when String.is_prefix first ~prefix:"{" ->
        let json = json_of_string first in
        let metadata = metadata_of_json json in
        (lines, metadata)
    | _ -> (lines, no_metadata)
  in
  let h = Stdlib.Hashtbl.create (List.length lines) in
  List.iter lines ~f:(fun l ->
      match String.split ~on:',' l with
      | [ a; b ] -> Stdlib.Hashtbl.replace h a b
      | _ -> failwith (Printf.sprintf "didn't get exactly 2 items in line %S" l));
  (Stdlib.Hashtbl.find_opt h, metadata)

let merge_right_biased (f1, m1) (f2, m2) =
  ( (fun word -> match f2 word with Some _ as opt -> opt | None -> f1 word)
  , merge_metadata_right_biased m1 m2 )

let merge_right_biased_opt d1 = function
  | None -> d1
  | Some d2 -> merge_right_biased d1 d2
