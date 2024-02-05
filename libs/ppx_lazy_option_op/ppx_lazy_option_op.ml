open Ppxlib

let rule =
  let fun_ = "||?" in
  Context_free.Rule.special_function fun_
    (fun e ->
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_apply (_, [(Nolabel, x); (Nolabel, y)]) ->
         Some [%expr match [%e x] with None -> [%e y] | Some o -> o]
      | _ -> Location.raise_errorf ~loc
               "%s must be applied to two arguments" fun_)

let () =
  Driver.register_transformation "ppx_lazy_option_op"
    ~rules:[ rule ]
;;
