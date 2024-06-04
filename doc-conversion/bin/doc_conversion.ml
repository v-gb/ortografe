open Base
   
external js_expr : string -> 'a = "caml_js_expr"

let convert_string dict_contents =
  let dict_contents = Jv.to_string dict_contents in
  let dict, metadata =
    Dict_gen_common.Dict_gen.parse dict_contents
      ~json_of_string:Brrex.json_of_string
  in
  fun ~filename file_contents ->
  let `ext new_ext, new_text =
    try
      Ortografe.convert_string
        ~ext:(Stdlib.Filename.extension filename)
        ~options:{ convert_uppercase = true
                 ; dict
                 ; interleaved = true
                 ; plurals_in_s = metadata.plurals_in_s ||? true
                 }
        file_contents
      ||? failwith ("type de fichier non supporté: " ^ filename)
    with e ->
      (* The "Failure" special-case is to avoid escaping utf8. We'd need the
         Sexp.to_string functions we have elsewhere here as well. Although ideally, the
         conversion of ocaml exception into javascript exceptions would be more
         general, not a hack like here. *)
      (match e with
       | Failure s -> s
       | _ -> Exn.to_string e)
      |> Jstr.of_string
      |> Jv.throw
  in
  `name (Stdlib.Filename.remove_extension filename ^ "-conv" ^ new_ext),
  `mime (Ortografe.mimetype_by_ext new_ext ||? failwith "unknown extension??"),
  new_text

let convert_file dict_content =
  let f = convert_string dict_content in
  fun file_object ->
    let open Fut.Result_syntax in
    let* text = Brrex.read_bytes file_object in
    let filename = Brr.File.name file_object in
    let `name new_name, `mime mime, new_text =
      f ~filename:(Jstr.to_string filename) text
    in
    Brrex.download_from_memory
      ~mime
      ~filename:(Jstr.v new_name)
      (`Str_in_base64 (Base64.encode_string new_text));
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
