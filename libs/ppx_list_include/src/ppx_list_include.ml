open Ppxlib

let rec extract_list_literal rev_acc e =
  match e with
  | [%expr [%e? a] :: [%e? b]] -> extract_list_literal (a :: rev_acc) b
  | [%expr []] -> (rev_acc, None)
  | _ -> (rev_acc, Some e)

let extract_list_literal e = extract_list_literal [] e

let add_list_literal (x, xs) =
  match x with [] -> xs | first :: _ -> `Literal (first.pexp_loc, x) :: xs

let list_literal e =
  let rev_acc, tail = extract_list_literal e in
  if List.exists (function [%expr +[%e? _]] -> true | _ -> false) rev_acc
  then
    rev_acc
    |> List.fold_left
         (fun (cur, acc) e ->
           match e with
           | [%expr +[%e? e]] -> (
               match extract_list_literal e with
               | rev_l, None -> (List.rev_append rev_l cur, acc)
               | rev_l, Some tail ->
                   (List.rev rev_l, `Dyn tail :: add_list_literal (cur, acc)))
           | _ -> (e :: cur, acc))
         ([], match tail with None -> [] | Some e -> [ `Dyn e ])
    |> add_list_literal
    |> (function
         | [ `Literal (loc, l) ] -> Ast_builder.Default.elist ~loc l
         | [ `Literal (loc, l); `Dyn tail ] -> Ast_builder.Default.elist_tail ~loc l tail
         | [ `Dyn e ] ->
             (* Don't optimize type errors away in things like [+""] *)
             let loc = e.pexp_loc in
             [%expr ([%e e] : _ Stdlib.List.t)]
         | l ->
             let loc = e.pexp_loc in
             let e =
               Ast_builder.Default.elist ~loc
                 (List.map
                    (function `Dyn e -> e
                            | `Literal (loc, l) -> Ast_builder.Default.elist ~loc l)
                    l)
             in
             (* Maybe we should use Base.List.concat, because Stdlib.List.concat
                is not tail rec, and rebuilds the last element of the list because
                it uses Stdlib.(@) which fails to optimize l @ [], unlike Base.(@). *)
             [%expr Stdlib.List.concat [%e e]])
    |> Option.some
  else None

let () =
  Driver.register_transformation "ppx_list_include"
    ~impl:
      (object (self)
         inherit Ppxlib_traverse_builtins.map
         inherit map as super

         method expression_skip_cons e =
           (* Skip over the conses to avoid a quadratic complexity in the length of list
              literals *)
           match e with
           | { pexp_desc =
                 Pexp_construct
                   ( ({ txt = Lident "::"; _ } as id)
                   , Some ({ pexp_desc = Pexp_tuple [ a; b ]; _ } as e2) )
             ; _
             } ->
               let a' = self#expression a in
               let b' = self#expression_skip_cons b in
               if a == a' && b == b'
               then e
               else
                 { e with
                   pexp_desc =
                     Pexp_construct
                       (id, Some { e2 with pexp_desc = Pexp_tuple [ a'; b' ] })
                 }
           | _ -> self#expression e

         method! expression e =
           match e with
           | [%expr [%e? _] :: [%e? _]] -> (
               match list_literal e with
               | None -> self#expression_skip_cons e
               | Some e -> super#expression e)
           | _ -> super#expression e
      end)
        #structure
