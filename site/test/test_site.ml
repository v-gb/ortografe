open Core

let print_s s =
  print_endline (Sexp_with_utf8.to_string_hum s)

let%expect_test "dict_search"  =
  let t =
    Dict_search.create (fun yield ->
        let yield1 s = yield s s in
        yield "choeur" "queur";
        yield "chœur" "queur";
        yield1 "met";
        yield1 "méta";
        yield "mette" "mète";
        yield1 "mets";
        yield1 "métal";
        yield1 "météo";
        yield "mettes" "mètes";
        yield "mettez" "métez";
        yield1 "métis";
        yield1 "métra";
      )
  in
  let search term =
    print_s [%sexp (Dict_search.search t term ~limit:10 : (string * string) list)]
  in
  (* lookup is: diacritics-insensitive, and goes in increasing length order *)
  search "met";
  [%expect {|
    ((met met) (méta méta) (mette mète) (mets mets) (métal métal)
     (météo météo) (mettes mètes) (mettez métez) (métis métis)
     (métra métra)) |}];
  (* diacritics in the pattern are respected though *)
  search "mét";
  [%expect {|
    ((méta méta) (métal métal) (météo météo) (mettez métez)
     (métis métis) (métra métra)) |}];
  search "mèt";
  [%expect {| ((mette mète) (mettes mètes)) |}];

  (* œ can be looked up with either "oe" or "œ" *)
  search "cho";
  [%expect {| ((choeur queur) (chœur queur)) |}];
  search "chœ";
  [%expect {| ((choeur queur) (chœur queur)) |}];

  (* both old and new spellings are searched/retrieved *)
  search "q";
  [%expect {| ((choeur queur) (chœur queur)) |}];
  ()
