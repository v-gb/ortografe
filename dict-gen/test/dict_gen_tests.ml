open Core

let diff_strings ?(context = 1) str1 str2 =
  if String.( = ) str1 str2
  then ""
  else
    let lines1 = String.split_lines str1 |> Array.of_list in
    let lines2 = String.split_lines str2 |> Array.of_list in
    let hunks =
      Patience_diff_lib.Patience_diff.String.get_hunks ~transform:Fn.id ~context
        ~prev:lines1 ~next:lines2 ()
    in
    let ranges = Patience_diff_lib.Patience_diff.Hunks.ranges hunks in
    let b = ref [] in
    List.iter ranges ~f:(fun range ->
        match range with
        | Same a -> Array.iter a ~f:(fun (s, _) -> b := (" ", s) :: !b)
        | Unified (a, _move_opt) -> Array.iter a ~f:(fun s -> b := (" ", s) :: !b)
        | Prev (a, _move_opt) -> Array.iter a ~f:(fun s -> b := ("-", s) :: !b)
        | Next (a, _move_opt) -> Array.iter a ~f:(fun s -> b := ("+", s) :: !b)
        | Replace (a, a', _move_opt) ->
            Array.iter a ~f:(fun s -> b := ("-", s) :: !b);
            Array.iter a' ~f:(fun s -> b := ("+", s) :: !b));
    List.rev !b |> List.map ~f:(fun (sign, s) -> sign ^ s ^ "\n") |> String.concat

let embedded : Dict_gen_common.Dict_gen.embedded =
  { data_lexique_Lexique383_gen_tsv = Ortografe_embedded.data_lexique_Lexique383_gen_tsv
  ; extension_dict1990_gen_csv = Ortografe_embedded.extension_dict1990_gen_csv
  }

let convert ~rules ~which_dict str =
  let rules =
    List.map rules ~f:(function
      | `Custom c ->
          Dict_gen_common.Dict_gen.custom_rule c ||? failwith ("empty custom rule " ^ c)
      | `Builtin b ->
          Dict_gen_common.Dict_gen.of_name_builtin b ||? failwith ("can't find " ^ b))
  in
  let dict, metadata =
    match which_dict with
    | `All_at_once ->
        let b = Buffer.create 100 in
        let (`Stats _) =
          Dict_gen_common.Dict_gen.gen ~rules ~all:false ~output:(Buffer.add_string b)
            ~json_to_string:Yojson.to_string (`Embedded embedded)
        in
        Dict_gen_common.Dict_gen.parse ~json_of_string:Yojson.Basic.from_string
          (Buffer.contents b)
    | `Staged -> Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded) rules
  in
  Ortografe.pure_text ~dst:String
    ~options:
      { convert_uppercase = true
      ; dict
      ; interleaved = true
      ; plurals_in_s = metadata.plurals_in_s ||? Some "s"
      }
    str

let check_and_compare str rules =
  let all_at_once = convert ~rules ~which_dict:`All_at_once str in
  let staged = convert ~rules ~which_dict:`Staged str in
  print_endline all_at_once;
  print_endline (diff_strings all_at_once staged)

let%expect_test _ =
  let str =
    {|
rien: rien
Érofa: rythme apparaitre
1990: maître wallabies
1990 + Érofa: apparaître
oe: oedipien oedipienne
œ: œdipien œdipienne
cas difficile: chariot chariotage
|}
  in
  let check_and_compare rules = check_and_compare str rules in
  let () = check_and_compare [ `Builtin "erofa" ] in
  [%expect
    {|
    rien: rien
    Érofa: ritme aparaitre
    1990: maître wallabies
    1990 + Érofa: aparaître
    oe: oedipien oedipiène
    œ: œdipien œdipiène
    cas dificile: chariot chariotage |}];
  let () = check_and_compare [ `Builtin "erofa"; `Builtin "1990" ] in
  [%expect
    {|
    rien: rien
    Érofa: ritme aparaitre
    1990: maitre wallabys
    1990 + Érofa: aparaitre
    oe: oedipien oedipiène
    œ: œdipien œdipiène
    cas dificile: chariot chariotage |}];
  let () = check_and_compare [ `Builtin "erofa"; `Builtin "oe" ] in
  [%expect
    {|
    rien: rien
    Érofa: ritme aparaitre
    1990: maître wallabies
    1990 + Érofa: aparaître
    oe: œdipien œdipiène
    œ: œdipien œdipiène
    cas dificile: chariot chariotage |}];
  let () = check_and_compare [ `Builtin "erofa"; `Builtin "1990"; `Builtin "oe" ] in
  [%expect
    {|
    rien: rien
    Érofa: ritme aparaitre
    1990: maitre wallabys
    1990 + Érofa: aparaitre
    oe: œdipien œdipiène
    œ: œdipien œdipiène
    cas dificile: chariot chariotage |}];
  let () = check_and_compare [ `Builtin "1990" ] in
  [%expect
    {|
    rien: rien
    Érofa: rythme apparaitre
    1990: maitre wallabys
    1990 + Érofa: apparaitre
    oe: oedipien oedipienne
    œ: œdipien œdipienne
    cas difficile: charriot charriotage |}];
  let () = check_and_compare [ `Builtin "oe" ] in
  [%expect
    {|
    rien: rien
    Érofa: rythme apparaitre
    1990: maître wallabies
    1990 + Érofa: apparaître
    oe: œdipien œdipienne
    œ: œdipien œdipienne
    cas difficile: chariot chariotage |}];
  let () = check_and_compare [ `Builtin "1990"; `Builtin "oe" ] in
  [%expect
    {|
    rien: rien
    Érofa: rythme apparaitre
    1990: maitre wallabys
    1990 + Érofa: apparaitre
    oe: œdipien œdipienne
    œ: œdipien œdipienne
    cas difficile: charriot charriotage |}];
  ()

let%expect_test _ =
  check_and_compare {|
château
châteaux
souriceau
coeur
|}
    [ `Custom "eau/ô â/a"; `Builtin "oe" ];
  [%expect {|
    chatô
    chatôx
    souriceau
    cœur |}];
  ()
