let () =
  Js_of_ocaml.Js.export "zzz_dict_gen"
    (fun a b ->
      let t1 = Sys.time () in
      let `Stats stats =
        Dict_gen_common.Dict_gen.gen
          (`Static { data_lexique_Lexique383_gen_tsv = Js_of_ocaml.Js.to_string a
                   ; extension_dict1990_gen_csv = Js_of_ocaml.Js.to_string b })
          ~rules:[]
          ~rect90:true
          ~all:false
          ~oe:true
          ~output:ignore
          ~json_to_string:(fun _ -> "")
      in
      print_endline (Sexplib.Sexp.to_string_hum stats);
      let t2 = Sys.time () in
      string_of_float (t2 -. t1)
    )
