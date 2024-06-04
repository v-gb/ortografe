external js_expr : string -> 'a = "caml_js_expr"
let throw e =
  (js_expr "((exn) => { throw exn })" : Jv.Error.t -> _) e

let or_throw = function
  | Ok v -> v
  | Error e -> throw e

let raw_promise (fut : _ Fut.t) : Jv.Promise.t =
  Jv.get (Obj.magic fut) "fut"

let fut_to_promise'
    : ok:('a -> Jv.t) -> error:('b -> Jv.t) -> ('a, 'b) Fut.result -> Jv.Promise.t
  = fun ~ok ~error f ->
  Jv.Promise.bind (raw_promise f)
    (function
     | Ok v -> Jv.Promise.resolve (ok v)
     | Error e -> Jv.Promise.reject (error e))

let fut_to_promise
    : 'a. ok:('a -> Jv.t) -> 'a Fut.or_error -> Jv.Promise.t
  = fun ~ok f ->
  fut_to_promise' ~ok ~error:Jv.of_error f

let fut_await fut f =
  ignore (Jv.Promise.then'
            (raw_promise fut)
            (fun a -> f (Ok a); Jv.Promise.resolve ())
            (fun e -> f (Error e); Jv.Promise.resolve ()))
  
let is_array jv =
  Jv.call (Jv.get Jv.global "Array") "isArray" [| jv |]
  |> Jv.to_bool

let json_of_string str =
  match Brr.Json.decode (Jstr.of_string str) with
  | Error e -> throw e
  | Ok jv ->
     let rec conv jv =
       match Jstr.to_string (Jv.typeof jv) with
       | "object" -> (* null, object, array *)
          if Jv.is_null jv
          then `Null
          else if is_array jv
          then `Array (Jv.to_list conv jv)
          else
            Jv.It.fold_bindings
              ~key:Jv.to_string
              ~value:conv
              (fun k v acc -> (k, v) :: acc)
              (Jv.It.iterator
                 (Jv.call (Jv.get Jv.global "Object") "entries" [| jv |]))
              []
            |> List.rev
            |> `Assoc __
       | "string" -> `String (Jv.to_string jv)
       | "number" -> `Number (Jv.to_float jv)
       | "boolean" -> `Bool (Jv.to_bool jv)
       | s -> failwith ("unknown type in json " ^ s)
     in
     conv jv
;;

let json_to_string v =
  let rec to_js = function
    | `Bool b -> Jv.of_bool b
    | `String s -> Jv.of_string s
    | `Null -> Jv.null
    | `Number f -> Jv.of_float f
    | `Array l -> Jv.of_list to_js l
    | `Assoc l ->
       List.map (fun (k, v) -> (k, to_js v)) l
       |> Array.of_list
       |> Jv.obj
  in
  Jstr.to_string (Brr.Json.encode (to_js v))

let read_bytes file_object =
  let open Fut.Result_syntax in
  let* buf = Brr.Blob.array_buffer (Brr.File.as_blob file_object) in
  Fut.ok (Brr.Tarray.to_string (Brr.Tarray.of_buffer Uint8 buf))


let download_from_memory ~mime ~filename text =
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
                     (Jstr.v [%string "data:%{mime};charset=utf-8,"])
                     (or_throw (Brr.Uri.encode str)))
               | `Str_in_base64 str_in_base64 ->
                  (Jstr.append
                     (Jstr.v [%string "data:%{mime};base64,"])
                     (Jstr.v str_in_base64)))
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
