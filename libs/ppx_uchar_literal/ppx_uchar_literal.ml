open Ppxlib

let rule =
  let fun_ = "!!" in
  Context_free.Rule.special_function fun_ (fun e ->
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_apply (_, [ (Nolabel, x) ]) -> (
          match x.pexp_desc with
          | Pexp_constant (Pconst_string (str, _, _)) ->
              let utf_decode = String.get_utf_8_uchar str 0 in
              if not (Uchar.utf_decode_is_valid utf_decode)
              then Location.raise_errorf ~loc "%s is applied to invalid utf8" fun_;
              if Uchar.utf_decode_length utf_decode <> String.length str
              then
                Location.raise_errorf ~loc
                  "%s is applied to multiple utf8 code points, only one was expected" fun_;
              let uchar = Uchar.utf_decode_uchar utf_decode in
              Some
                [%expr
                  Stdlib.Uchar.unsafe_of_int
                    [%e Ast_builder.Default.eint ~loc (Stdlib.Uchar.to_int uchar)]]
          | _ -> Location.raise_errorf ~loc "%s must be applied to a string literal" fun_)
      | _ -> Location.raise_errorf ~loc "%s must be applied to a string literal" fun_)

let () = Driver.register_transformation "ppx_uchar_literal" ~rules:[ rule ]
