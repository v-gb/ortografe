open Ppxlib

let rec extract_list_literal rev_acc e =
  match e with
  | [%expr ([%e? a] :: [%e? b])] ->
     extract_list_literal (a :: rev_acc) b
  | [%expr []] -> rev_acc, None
  | _ -> rev_acc, Some e
let extract_list_literal e = extract_list_literal [] e

let add_list_literal (x, xs) =
  match x with
  | [] -> xs
  | first :: _ -> Ast_builder.Default.elist ~loc:first.pexp_loc x :: xs

let list_literal e =
  let rev_acc, tail = extract_list_literal e in
  if List.exists (function
         | [%expr +[%e? _]] -> true
         | _ -> false) rev_acc
  then
    rev_acc
    |> List.fold_left (fun (cur, acc) e ->
           match e with
           | [%expr +[%e? e]] ->
              (match extract_list_literal e with
              | [], None -> (cur, acc)
              | rev_l, None -> (List.rev_append rev_l cur, acc)
              | rev_l, Some tail -> (List.rev rev_l, tail :: add_list_literal (cur, acc)))
           | _ -> (e :: cur, acc))
         ([], match tail with None -> [] | Some e -> [e])
    |> add_list_literal
    |> (function
        | [e] -> e
        | l ->
           let loc = e.pexp_loc in
           let e = Ast_builder.Default.elist ~loc l in
           [%expr Stdlib.List.concat [%e e]])
    |> Option.some
  else None

let rec list_literal_fallback rec_ e =
  match e with
  | { pexp_desc = Pexp_construct ({ txt = Lident "::"; _ } as id, Some ({ pexp_desc = Pexp_tuple [ a; b ]; _ } as e2)); _ } as e1 ->
     let a' = rec_ a in
     let b' = list_literal_fallback rec_ b in
     if a == a' && b == b'
     then e
     else { e1 with pexp_desc = Pexp_construct (id, Some { e2 with pexp_desc = Pexp_tuple [ a'; b' ] }) }
  | _ -> rec_ e

let () =
  Driver.register_transformation
    "ppx_list_include"
    ~impl:(
      object (self)
        inherit Ppxlib_traverse_builtins.map
        inherit map as super
        method! expression e =
          match e with
          | [%expr ([%e? _] :: [%e? _])] ->
             (match list_literal e with
              | None -> list_literal_fallback self#expression e
              | Some e -> super#expression e)
          | _ -> super#expression e
      end#structure
    )
