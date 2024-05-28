open Core
let%expect_test _ =
  print_endline (Sexp_with_utf8.to_string_hum (Atom "accentué"));
  [%expect {| accentué |}];
  print_endline (Sexp_with_utf8.to_string_hum (Atom "et là"));
  [%expect {| "et l\195\160" |}]
