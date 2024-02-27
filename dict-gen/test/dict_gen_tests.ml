open Core

let diff_strings ?(context = 1) str1 str2 =
  if String.(=) str1 str2
  then ""
  else
    let lines1 = String.split_lines str1 |> Array.of_list in
    let lines2 = String.split_lines str2 |> Array.of_list  in
    let hunks =
      Patience_diff_lib.Patience_diff.String.get_hunks
        ~transform:Fn.id
        ~context
        ~prev:lines1
        ~next:lines2
        ()
    in
    let ranges = Patience_diff_lib.Patience_diff.Hunks.ranges hunks in
    let b = ref [] in
    List.iter ranges ~f:(fun range ->
        match range with
        | Same a -> Array.iter a ~f:(fun (s, _) -> b := (" ", s) :: !b)
        | Unified a -> Array.iter a ~f:(fun s -> b := (" ", s) :: !b)
        | Prev a -> Array.iter a ~f:(fun s -> b := ("-", s) :: !b)
        | Next a -> Array.iter a ~f:(fun s -> b := ("+", s) :: !b)
        | Replace (a, a') ->
           Array.iter a ~f:(fun s -> b := ("-", s) :: !b);
           Array.iter a' ~f:(fun s -> b := ("+", s) :: !b);
      );
    List.rev !b
    |> List.map ~f:(fun (sign, s) -> sign ^ s ^ "\n")
    |> String.concat

let embedded : Dict_gen_common.Dict_gen.embedded =
  { data_lexique_Lexique383_gen_tsv = Ortografe_embedded.data_lexique_Lexique383_gen_tsv
  ; extension_dict1990_gen_csv = Ortografe.extension_dict1990_gen_csv
  }  
  
let%expect_test _ =
  let convert ~rules ~which_dict str =
    let rules =
      List.map rules
        ~f:(fun elt ->
          Dict_gen_common.Dict_gen.of_name elt
          ||? failwith ("can't find " ^ elt))
    in
    let dict, metadata =
      match which_dict with
      | `All_at_once ->
         let b = Buffer.create 100 in
         let `Stats _ =
           Dict_gen_common.Dict_gen.gen
             ~rules
             ~all:false
             ~output:(Buffer.add_string b)
             ~json_to_string:Yojson.to_string
             (`Embedded embedded)
         in
         Dict_gen_common.Dict_gen.parse
           ~json_of_string:Yojson.Basic.from_string
           (Buffer.contents b)
      | `Staged ->
         Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded)
           rules
    in
    Ortografe.pure_text
      ~dst:String
      ~options:{ convert_uppercase = true
               ; dict
               ; interleaved = true
               ; plurals_in_s = metadata.plurals_in_s ||? true
               }
      str
  in
  let str = {|
rien: rien
érofa: rythme apparaitre
1990: maître wallabies
1990 + érofa: apparaître
oe: oedipien oedipienne
œ: œdipien œdipienne
cas difficile: chariot
|} in
  let check_and_compare rules =
    let all_at_once = convert ~rules ~which_dict:`All_at_once str in
    let staged = convert ~rules ~which_dict:`Staged str in
    print_endline all_at_once;
    print_endline (diff_strings all_at_once staged)
  in
  let () = check_and_compare [ "erofa" ] in
  [%expect {|
    rien: rien
    érofa: ritme aparaitre
    1990: maître wallabies
    1990 + érofa: aparaître
    oe: oedipien oedipiène
    œ: œdipien œdipiène
    cas dificile: chariot

     oe: oedipien oedipiène
    -œ: œdipien œdipiène
    +œ: œdipien œdipienne
     cas dificile: chariot |}];
  let () = check_and_compare [ "erofa"; "1990" ] in
  [%expect {|
    rien: rien
    érofa: ritme aparaitre
    1990: maitre wallabys
    1990 + érofa: aparaitre
    oe: oedipien oedipiène
    œ: œdipien œdipiène
    cas dificile: chariot

     érofa: ritme aparaitre
    -1990: maitre wallabys
    -1990 + érofa: aparaitre
    +1990: maître wallabies
    +1990 + érofa: aparaître
     oe: oedipien oedipiène
    -œ: œdipien œdipiène
    +œ: œdipien œdipienne
     cas dificile: chariot |}];
  let () = check_and_compare [ "erofa"; "oe" ] in
  [%expect {|
    rien: rien
    érofa: ritme aparaitre
    1990: maître wallabies
    1990 + érofa: aparaître
    oe: œdipien œdipiène
    œ: œdipien œdipiène
    cas dificile: chariot

     1990 + érofa: aparaître
    -oe: œdipien œdipiène
    -œ: œdipien œdipiène
    +oe: oedipien oedipiène
    +œ: œdipien œdipienne
     cas dificile: chariot |}];
  let () = check_and_compare [ "erofa"; "1990"; "oe" ] in
  [%expect {|
    rien: rien
    érofa: ritme aparaitre
    1990: maitre wallabys
    1990 + érofa: aparaitre
    oe: œdipien œdipiène
    œ: œdipien œdipiène
    cas dificile: chariot

     érofa: ritme aparaitre
    -1990: maitre wallabys
    -1990 + érofa: aparaitre
    -oe: œdipien œdipiène
    -œ: œdipien œdipiène
    +1990: maître wallabies
    +1990 + érofa: aparaître
    +oe: oedipien oedipiène
    +œ: œdipien œdipienne
     cas dificile: chariot |}];
  ()
