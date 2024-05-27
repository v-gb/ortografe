open Base

external js_expr : string -> 'a = "caml_js_expr"

let impl, convert_string =
  let cache = Jv.obj [||] in
  let impl, f =
    Brrex.rpc_with_progress
      (fun ?progress (rules_params, dict_blob, filename, file_contents) ->
        let open Fut.Result_syntax in
        let* dict, metadata =
          Dict_gen_browser.staged_generate cache rules_params dict_blob
        in
        let `ext new_ext, new_text =
          try
            Option.iter progress ~f:(fun f -> f 10);
            Ortografe.convert_string
              ?progress:(Option.map progress ~f:(fun f i -> f (10 + (i * 9 / 10))))
              ~ext:(Stdlib.Filename.extension filename)
              ~options:
                { convert_uppercase = true
                ; dict
                ; interleaved = true
                ; plurals_in_s = metadata.plurals_in_s ||? Some "s"
                }
              file_contents
            ||? failwith ("type de fichier non supporté: " ^ filename)
          with e ->
            (* The "Failure" special-case is to avoid escaping utf8. We'd need the
               Sexp.to_string functions we have elsewhere here as well. Although
               ideally, the conversion of ocaml exception into javascript exceptions
               would be more general, not a hack like here. *)
            (match e with Failure s -> s | _ -> Exn.to_string e)
            |> Jstr.of_string
            |> Jv.throw
        in
        Option.iter ~f:(fun f -> f 100) progress;
        Fut.ok
          ( `name (Stdlib.Filename.remove_extension filename ^ "-conv" ^ new_ext)
          , `mime (Ortografe.mimetype_by_ext new_ext ||? failwith "unknown extension??")
          , new_text ))
  in
  ( impl
  , fun ?progress ww_cache rules_params dict_blob ~filename file_contents ->
      f ~ww_cache ?progress (rules_params, dict_blob, filename, file_contents) )

let convert_file ?progress ww_cache rules_params dict_blob doc_file =
  let open Fut.Result_syntax in
  let* text = Brrex.read_bytes_from_file doc_file in
  let filename = Brr.File.name doc_file in
  let* `name new_name, `mime mime, new_text =
    convert_string ww_cache ?progress rules_params dict_blob
      ~filename:(Jstr.to_string filename) text
  in
  Brrex.download_from_memory ~mime ~filename:(Jstr.v new_name)
    (`Str_in_base64 (Base64.encode_string new_text));
  Fut.ok ()

let with_exn_in_dom_async (type a) id (f : unit -> a Fut.or_error) : a Fut.or_error =
  (js_expr
     {|(async (id, f) => {
    document.getElementById(id).textContent = "";
    try {
        return await f();
    } catch (e) {
        document.getElementById(id).textContent =
            (new Date()).toLocaleTimeString() + ": " + e.toString();
        throw e;
    }
  })|}
    : Jstr.t -> Jv.t -> Jv.t)
    id
    (Jv.callback ~arity:1 (fun () ->
         Brrex.fut_to_promise ~ok:(Stdlib.Obj.magic : a -> Jv.t) (f ())))
  |> Fut.of_promise ~ok:(Stdlib.Obj.magic : Jv.t -> a)

let create () = Brrex.ww_cache ()
let ww_cache, ww_cache' = Brrex.B.magic_ ()

let convert_file_handle_errors =
  Brrex.B.(
    fun6' ww_cache jstr
      (option (t3 (t3 Dict_gen_browser.selected_rules string bool) jstr jstr))
      (option Brr.Blob.of_jv) Brr.File.of_jv
      (option (fun1 int' unit))
      (promise_or_error' unit'))
    (fun ww_cache id rules_params dict_blob doc_file progress ->
      with_exn_in_dom_async id (fun () ->
          convert_file ?progress ww_cache rules_params dict_blob doc_file))

let () =
  Brrex.main [ impl ] (fun () ->
      Js_of_ocaml.Js.export "doc_conversion"
        (Js_of_ocaml.Js.Unsafe.inject
           (Jv.obj
              [| ("create", Brrex.B.(fun1' unit ww_cache') create)
               ; ("convert", convert_file_handle_errors)
              |])))
