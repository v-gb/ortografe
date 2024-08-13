include Data

let load_dict str =
  let l = String.split_on_char '\n' str in
  let h = Hashtbl.create 25000 in
  List.iter
    (fun str ->
      match String.split_on_char ',' str with
      | [] | [ "" ] -> ()
      | [ a; b ] -> Hashtbl.replace h a b
      | _ -> failwith ("what " ^ str))
    l;
  h

let erofa, rect1990 =
  ( lazy (load_dict Data.extension_dict_gen_csv)
  , lazy (load_dict Data.extension_dict1990_gen_csv) )
