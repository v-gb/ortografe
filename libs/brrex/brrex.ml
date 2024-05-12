external js_expr : string -> 'a = "caml_js_expr"
let throw e =
  (js_expr "((exn) => { throw exn })" : Jv.Error.t -> _) e
             
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
       | "boolean" -> `Boolean (Jv.to_bool jv)
       | s -> failwith ("unknown type in json " ^ s)
     in
     conv jv
;;

let read_bytes file_object =
  let open Fut.Result_syntax in
  let* buf = Brr.Blob.array_buffer (Brr.File.as_blob file_object) in
  Fut.ok (Brr.Tarray.to_string (Brr.Tarray.of_buffer Uint8 buf))
