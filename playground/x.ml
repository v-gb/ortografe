open Base

type json =
  [ `Array of json list
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Null
  | `Number of float
  | `String of string
  ]
[@@deriving sexp_of]

let () =
  let json =
    Brrex.json_of_string
      {|
{ "a": [ "2", null, true, false, -2.34, { "a" : 2, "b" : 3 } ] }
|}
  in
  Stdlib.print_endline (Sexp.to_string_hum (sexp_of_json json));
  ignore (Brrex.json_of_string "{")
