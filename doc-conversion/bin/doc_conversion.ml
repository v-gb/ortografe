open Base
   
external js_expr : string -> 'a = "caml_js_expr"
let or_throw = function
  | Ok v -> v
  | Error e -> Brrex.throw e

let convert_string dict_contents =
  let dict_contents = Jv.to_string dict_contents in
  let dict, metadata =
    Dict_gen_common.Dict_gen.parse dict_contents
      ~json_of_string:Brrex.json_of_string
  in
  fun ~filename file_contents ->
  let `ext new_ext, new_text =
    Ortografe.convert_string
      ~ext:(Stdlib.Filename.extension filename)
      ~options:{ convert_uppercase = true
               ; dict
               ; interleaved = true
               ; plurals_in_s = metadata.plurals_in_s ||? true
               ; impl =
                   { parse = (fun ~flavor src ->
                       Fun (Markup_js.parse ~flavor src))
                   ; print = Ortografe.markup_print_impl.print
                   }
      }
      file_contents
    ||? failwith ("type de fichier non supporté: " ^ filename)
  in
  Stdlib.Filename.remove_extension filename ^ "-conv" ^ new_ext,
  new_text

let download ~filename text =
  (* One possible problem is there's a size limit on the size of inline
     data. https://developer.mozilla.org/en-US/docs/web/http/basics_of_http/data_urls
     says 32MB in firefox and more in chrome/safari, so that seems good enough. *)
  let el =
    Brr.El.v
      ~at:[ Brr.At.v
              (Jstr.v "href")
              (match text with
               | `Jstr str ->
                  (Jstr.append
                     (Jstr.v "data:text/plain;charset=utf-8,")
                     (or_throw (Brr.Uri.encode str)))
               | `Str str ->
                  (Jstr.append
                     (Jstr.v "data:text/plain;base64,")
                     (Jstr.v (Base64.encode_string str))))
          ; Brr.At.v
              (Jstr.v "download")
              filename
          ; Brr.At.v
              (Jstr.v "style")
              (Jstr.v "display: none")
      ]
      (Jstr.v "a") []
  in
  Brr.El.append_children
    (Brr.Document.body Brr.G.document)
    [ el ];
  Brr.El.click el;
  Brr.El.remove el

let convert_file dict_content =
  let f = convert_string dict_content in
  fun file_object ->
    let open Fut.Result_syntax in
    let* text = Brrex.read_bytes file_object in
    let filename = Brr.File.name file_object in
    let new_name, new_text = f ~filename:(Jstr.to_string filename) text in
    download ~filename:(Jstr.v new_name) (`Str new_text);
    Fut.ok ()

let with_exn_in_dom_sync id f =
  (js_expr {|((id, f) => {
    console.log('id', id, document.getElementById(id))
    document.getElementById(id).textContent = "";
    try {
        return f();
    } catch (e) {
        document.getElementById(id).textContent =
            (new Date()).toLocaleTimeString() + ": " + e.toString();
        throw e;
    }
  })|} : Jstr.t -> Jv.t -> 'a)
    id (Jv.callback ~arity:1 (f : unit -> 'a))

let with_exn_in_dom_async id (f : unit -> 'a Fut.or_error) : 'a Fut.or_error =
  (js_expr {|(async (id, f) => {
    document.getElementById(id).textContent = "";
    try {
        return await f();
    } catch (e) {
        document.getElementById(id).textContent =
            (new Date()).toLocaleTimeString() + ": " + e.toString();
        throw e;
    }
  })|} : Jstr.t -> Jv.t -> Jv.t)
    id (Jv.callback ~arity:1
          (fun () ->
            Brrex.fut_to_promise ~ok:Fn.id (f ())))
  |> Fut.of_promise ~ok:Fn.id

let convert_file_handle_errors id dict_content =
  let f =
    with_exn_in_dom_sync id
      (fun () -> convert_file dict_content) in
  fun file_object ->
    with_exn_in_dom_async id (fun () ->
        let open Fut.Result_syntax in
        let* () = f file_object in
        Fut.ok (Jv.undefined))

let () =
  Js_of_ocaml.Js.export "doc_conversion"
    (Js_of_ocaml.Js.Unsafe.inject
       (Jv.obj
          [| "convert",
             Jv.callback ~arity:2 (fun a b ->
                 Jv.callback ~arity:1 (convert_file_handle_errors a b))
          |]))
