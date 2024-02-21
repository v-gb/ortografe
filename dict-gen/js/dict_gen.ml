let any = Js_of_ocaml.Js.Unsafe.inject
let string s = any (Js_of_ocaml.Js.string s)
let bool b = any (Js_of_ocaml.Js.bool b)
let obj l = Js_of_ocaml.Js.Unsafe.obj (Array.of_list l)
let list l = any (Js_of_ocaml.Js.array (Array.of_list l))

let generate a b rules =
  let t1 = Sys.time () in
  let buf = Buffer.create 1_000_000 in
  let rules =
    (* we take rule names rather than rules so the argument to this function
         are serializable (so they can be written in postMessage). *)
    let rules_set =
      rules
      |> Js_of_ocaml.Js.to_array
      |> Array.to_list
      |> List.map Js_of_ocaml.Js.to_string
      |> Base.Set.of_list (module Base.String)
    in
    List.filter
      (fun rule ->
        Base.Set.mem rules_set (Dict_gen_common.Rewrite.name rule))
      (Lazy.force Dict_gen_common.Rewrite.all)
  in
  let `Stats stats =
    Dict_gen_common.Dict_gen.gen
      (`Static { data_lexique_Lexique383_gen_tsv = Js_of_ocaml.Js.to_string a
               ; extension_dict1990_gen_csv = Js_of_ocaml.Js.to_string b })
      ~rules
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

let rules () =
  let rules = Lazy.force Dict_gen_common.Rewrite.all in
  List.map (fun rule ->
      obj
        [ "name", string (Dict_gen_common.Rewrite.name rule)
        ; "doc", string (Dict_gen_common.Rewrite.doc rule)
    ]) rules
  |> list  

let () =
  Js_of_ocaml.Js.Unsafe.set
    (Js_of_ocaml.Js.Unsafe.global)
    "onmessage"
    (Js_of_ocaml.Js.Unsafe.pure_js_expr {|async function(e) {
    const lexique383 = await (await fetch("./Lexique383.gen.tsv")).text()
    const rect1990 = await (await fetch("./dict1990.gen.csv")).text()
    const res = globalThis.dict_gen.generate(lexique383, rect1990, e.data)
    postMessage(res)
}|})

let () =
  Js_of_ocaml.Js.export "dict_gen"
    (obj [ "generate", any generate
         ; "rules", any rules
         ])
