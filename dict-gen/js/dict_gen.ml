let any = Js_of_ocaml.Js.Unsafe.inject
let string s = any (Js_of_ocaml.Js.string s)
let bool b = any (Js_of_ocaml.Js.bool b)
let obj l = Js_of_ocaml.Js.Unsafe.obj (Array.of_list l)
let list l = any (Js_of_ocaml.Js.array (Array.of_list l))

let json_to_string v =
  let rec to_js = function
    | `Bool b -> bool b
    | `String s -> string s
    | `Assoc l -> obj (List.map (fun (k, v) -> (k, to_js v)) l)
  in
  Js_of_ocaml.Js.Unsafe.meth_call
    (Js_of_ocaml.Js._JSON)
    "stringify"
    [| to_js v |]
  |> Js_of_ocaml.Js.to_string

let generate static rules =
  let t1 = Sys.time () in
  let buf = Buffer.create 1_000_000 in
  let `Stats stats =
    Dict_gen_common.Dict_gen.gen
      (`Static static)
      ~rules
      ~all:false
      ~output:(Buffer.add_string buf)
      ~json_to_string
  in
  print_endline (Sexplib.Sexp.to_string_hum stats);
  let t2 = Sys.time () in
  (Buffer.contents buf, string_of_float (t2 -. t1))

let generate_js a b rules =
  let dict, duration =
    generate
      { data_lexique_Lexique383_gen_tsv = Js_of_ocaml.Js.to_string a
      ; extension_dict1990_gen_csv = Js_of_ocaml.Js.to_string b }
      (rules |> Js_of_ocaml.Js.to_array |> Array.to_list)
  in
  list [ string dict ; string duration ]

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

let fetch url =
  let open Fut.Result_syntax in
  let* response = Brr_io.Fetch.url url in
  let* text = Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body response) in
  Fut.return (Ok (Jstr.to_string text))
  
let on_message data =
  let jv = Jv.to_jv_array data in
  let lexique_url = jv.(0) |> Jv.to_jstr in
  let dict1990_url = jv.(1) |> Jv.to_jstr in
  let rules = jv.(2) |> Jv.to_list (Obj.magic : Jv.t -> Dict_gen_common.Dict_gen.rule) in
  let open Fut.Result_syntax in
  let* data_lexique_Lexique383_gen_tsv = fetch lexique_url in
  let* extension_dict1990_gen_csv = fetch dict1990_url in
  let* (dict, duration) =
    match
      generate
        { data_lexique_Lexique383_gen_tsv
        ; extension_dict1990_gen_csv
        }
        rules
    with
    | exception e -> Fut.error (Jv.Error.v (Jstr.of_string (Printexc.to_string e)))
    | v -> Fut.ok v
  in
  Brr_webworkers.Worker.G.post (list [ string dict ; string duration ]);
  Fut.return (Ok ())

let throw e =
  (Js_of_ocaml.Js.Unsafe.pure_js_expr
     "(function (exn) { throw exn })" : Jv.Error.t -> _) e

let () =
    if Brr_webworkers.Worker.ami ()
    then
      let open Fut.Syntax in
      Fut.await
        (let* event = Brr.Ev.next Brr_io.Message.Ev.message Brr.G.target in
         let data : Jv.t = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
         on_message data)
        (function
         | Ok () -> ()
         | Error e -> throw e)
    else
      Js_of_ocaml.Js.export "dict_gen"
        (obj [ "generate", any generate_js
             ; "rules", any rules
        ])
