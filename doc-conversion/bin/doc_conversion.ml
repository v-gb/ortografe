open Base
   
external js_expr : string -> 'a = "caml_js_expr"

let impls, convert_string =
  let z = ref None in
  let stage1_impl, stage1 =
    Brrex.rpc (fun dict_contents ->
        z := Some (
                 Dict_gen_common.Dict_gen.parse dict_contents
                   ~json_of_string:Brrex.json_of_string);
        Fut.ok ())
  in
  let stage2_impl, stage2 =
    Brrex.rpc_with_progress (fun ?progress (filename, file_contents) ->
        let `ext new_ext, new_text =
          try
            let dict, metadata = !z ||? failwith "z should always be set" in
            Ortografe.convert_string
              ?progress
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
               Sexp.to_string functions we have elsewhere here as well. Although
               ideally, the conversion of ocaml exception into javascript exceptions
               would be more general, not a hack like here. *)
            (match e with
             | Failure s -> s
             | _ -> Exn.to_string e)
            |> Jstr.of_string
            |> Jv.throw
        in
        Option.iter ~f:(fun f -> f 100) progress;
        Fut.ok (
            `name (Stdlib.Filename.remove_extension filename ^ "-conv" ^ new_ext),
            `mime (Ortografe.mimetype_by_ext new_ext ||? failwith "unknown extension??"),
            new_text))
  in
  [stage1_impl; stage2_impl],
  (fun dict_contents ->
    let open Fut.Result_syntax in
    let ww_cache = Brrex.ww_cache () in
    let* () = stage1 ~ww_cache dict_contents in
    Fut.ok (fun ?progress ~filename file_contents ->
        stage2 ~ww_cache ?progress (filename, file_contents)))

let convert_file dict_content =
  let open Fut.Result_syntax in
  let* f = convert_string dict_content in
  Fut.ok (fun ?progress doc_file ->
      let* text = Brrex.read_bytes_from_file doc_file in
      let filename = Brr.File.name doc_file in
      let* `name new_name, `mime mime, new_text =
        f ?progress ~filename:(Jstr.to_string filename) text
      in
      Brrex.download_from_memory
        ~mime
        ~filename:(Jstr.v new_name)
        (`Str_in_base64 (Base64.encode_string new_text));
      Fut.ok ())

let with_exn_in_dom_async (type a) id (f : unit -> a Fut.or_error) : a Fut.or_error =
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
            Brrex.fut_to_promise ~ok:(Stdlib.Obj.magic : a -> Jv.t) (f ())))
  |> Fut.of_promise ~ok:(Stdlib.Obj.magic : Jv.t -> a)

let convert_file_handle_errors =
  Brrex.B.(
    fun2'
      jstr
      string
      (promise_or_error'
         (fun2' Brr.File.of_jv
            (option (fun1 int' unit))
            (promise_or_error' unit'))))
    (fun id dict_content ->
      let open Fut.Result_syntax in
      let* f =
        with_exn_in_dom_async id
          (fun () -> convert_file dict_content) in
      Fut.ok (fun doc_file progress ->
          with_exn_in_dom_async id (fun () ->
              f ?progress doc_file)))

let () =
  Brrex.main
    impls
    (fun () ->
      Js_of_ocaml.Js.export "doc_conversion"
        (Js_of_ocaml.Js.Unsafe.inject
           (Jv.obj
              [| "convert", convert_file_handle_errors
              |])))
