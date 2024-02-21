let any = Js_of_ocaml.Js.Unsafe.inject
let string s = any (Js_of_ocaml.Js.string s)
let bool b = any (Js_of_ocaml.Js.bool b)
let obj l = Js_of_ocaml.Js.Unsafe.obj (Array.of_list l)
let list l = any (Js_of_ocaml.Js.array (Array.of_list l))

let generate a b rules =
  let t1 = Sys.time () in
  let buf = Buffer.create 1_000_000 in
  let rules = rules |> Js_of_ocaml.Js.to_array |> Array.to_list in
  let `Stats stats =
    Dict_gen_common.Dict_gen.gen
      (`Static { data_lexique_Lexique383_gen_tsv = Js_of_ocaml.Js.to_string a
               ; extension_dict1990_gen_csv = Js_of_ocaml.Js.to_string b })
      ~rules
      ~all:false
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
  let rules = Lazy.force Dict_gen_common.Dict_gen.all in
  let ui_doc =
    let re_word = Re.(compile (seq [ str "@"; rep (compl [set " ->@,."]) ])) in
    let re_url = Re.(compile (seq [ str "http"; rep (compl [set " ,"]) ])) in
    fun doc ->
    doc
    |> Base.String.substr_replace_all
         ~pattern:"->"
         ~with_:"→"
    |> Re.replace re_word ~all:true ~f:(fun group ->
           let word = Base.String.chop_prefix_exn (Re.Group.get group 0) ~prefix:"@" in
           [%string "<i>%{word}</i>"])
    |> Re.replace re_url ~all:true ~f:(fun group ->
           let url = Re.Group.get group 0 in
           let display_url =
             url
             |> Base.String.chop_prefix_if_exists ~prefix:"http://"
             |> Base.String.chop_prefix_if_exists ~prefix:"https://"
             |> Base.String.chop_prefix_if_exists ~prefix:"www."
             |> Base.String.chop_suffix_if_exists ~suffix:"/"
           in
           [%string "<a href=\"%{url}\">%{display_url}</a>"])
  in
  rules
  |> List.map (fun rule ->
         obj [ "name", string (Dict_gen_common.Dict_gen.name rule)
             ; "doc", string (ui_doc (Dict_gen_common.Dict_gen.doc rule))
             ; "v", any rule ])
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
