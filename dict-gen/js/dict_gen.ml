let () =
  Js_of_ocaml.Js.export "generate_dict"
    (fun a b ->
      let t1 = Sys.time () in
      let buf = Buffer.create 1_000_000 in
      let `Stats stats =
        Dict_gen_common.Dict_gen.gen
          (`Static { data_lexique_Lexique383_gen_tsv = Js_of_ocaml.Js.to_string a
                   ; extension_dict1990_gen_csv = Js_of_ocaml.Js.to_string b })
          ~rules:[]
          ~rect90:true
          ~all:false
          ~oe:true
          ~output:(Buffer.add_string buf)
          ~json_to_string:(
            let rec to_js = function
              | `Bool b -> Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.bool b)
              | `String s -> Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string s)
              | `Assoc l ->
                 List.map (fun (k, v) -> (k, to_js v)) l
                 |> Array.of_list
                 |> Js_of_ocaml.Js.Unsafe.obj
            in
            fun v ->
            Js_of_ocaml.Js.Unsafe.meth_call
              (Js_of_ocaml.Js._JSON)
              "stringify"
              [| to_js v |]
            |> Js_of_ocaml.Js.to_string
          )
      in
      print_endline (Sexplib.Sexp.to_string_hum stats);
      let t2 = Sys.time () in
      Js_of_ocaml.Js.array
        [| Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string (Buffer.contents buf))
         ; Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string (string_of_float (t2 -. t1)))
        |]
    )
