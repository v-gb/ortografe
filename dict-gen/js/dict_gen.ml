let any = Js_of_ocaml.Js.Unsafe.inject
let string s = any (Js_of_ocaml.Js.string s)
let bool b = any (Js_of_ocaml.Js.bool b)
let obj l = Js_of_ocaml.Js.Unsafe.obj (Array.of_list l)
let list l = any (Js_of_ocaml.Js.array (Array.of_list l))

let () =
  Js_of_ocaml.Js.export "dict_gen"
    (obj [ "generate", any (fun a b rules ->
             let t1 = Sys.time () in
             let buf = Buffer.create 1_000_000 in
             let `Stats stats =
               Dict_gen_common.Dict_gen.gen
                 (`Static { data_lexique_Lexique383_gen_tsv = Js_of_ocaml.Js.to_string a
                          ; extension_dict1990_gen_csv = Js_of_ocaml.Js.to_string b })
                 ~rules:(Array.to_list (Js_of_ocaml.Js.to_array rules))
                 ~rect90:false
                 ~all:false
                 ~oe:true
                 ~output:(Buffer.add_string buf)
                 ~json_to_string:(
                   let rec to_js = function
                     | `Bool b -> bool b
                     | `String s -> string s
                     | `Assoc l -> obj (List.map (fun (k, v) -> (k, to_js v)) l)
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
             list [ string (Buffer.contents buf) ; string (string_of_float (t2 -. t1)) ]
           )
         ; "rules", any (fun () ->
             let rules = Lazy.force Dict_gen_common.Rewrite.all in
             List.map (fun rule ->
                 obj
                   [ "name", string (Dict_gen_common.Rewrite.name rule)
                   ; "doc", string (Dict_gen_common.Rewrite.doc rule)
                   ; "v", any rule
                   ]) rules
             |> list
           )
       ])
