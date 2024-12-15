external js_expr : string -> 'a = "caml_js_expr"

let throw e = (js_expr "((exn) => { throw exn })" : Jv.Error.t -> _) e
let or_throw = function Ok v -> v | Error e -> throw e
let raw_promise (fut : _ Fut.t) : Jv.Promise.t = Jv.get (Obj.magic fut) "fut"

let fut_to_promise' :
    ok:('a -> Jv.t) -> error:('b -> Jv.t) -> ('a, 'b) Fut.result -> Jv.Promise.t =
 fun ~ok ~error f ->
  Jv.Promise.bind (raw_promise f) (function
    | Ok v -> Jv.Promise.resolve (ok v)
    | Error e -> Jv.Promise.reject (error e))

let fut_to_promise : 'a. ok:('a -> Jv.t) -> 'a Fut.or_error -> Jv.Promise.t =
 fun ~ok f -> fut_to_promise' ~ok ~error:Jv.of_error f

let fut_await fut f =
  ignore
    (Jv.Promise.then' (raw_promise fut)
       (fun a ->
         f (Ok a);
         Jv.Promise.resolve ())
       (fun e ->
         f (Error e);
         Jv.Promise.resolve ()))

let json_of_string str =
  match Brr.Json.decode (Jstr.of_string str) with
  | Error e -> throw e
  | Ok jv ->
      let rec conv jv =
        match Jstr.to_string (Jv.typeof jv) with
        | "object" ->
            (* null, object, array *)
            if Jv.is_null jv
            then `Null
            else if Jv.is_array jv
            then `Array (Jv.to_list conv jv)
            else
              Jv.It.fold_bindings ~key:Jv.to_string ~value:conv
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

let json_to_string v =
  let rec to_js = function
    | `Bool b -> Jv.of_bool b
    | `String s -> Jv.of_string s
    | `Null -> Jv.null
    | `Number f -> Jv.of_float f
    | `Array l -> Jv.of_list to_js l
    | `Assoc l -> List.map (fun (k, v) -> (k, to_js v)) l |> Array.of_list |> Jv.obj
  in
  Jstr.to_string (Brr.Json.encode (to_js v))

let string_of_array_buffer buf = Brr.Tarray.to_string (Brr.Tarray.of_buffer Uint8 buf)

let read_bytes blob =
  let open Fut.Result_syntax in
  let* buf = Brr.Blob.array_buffer blob in
  Fut.ok (string_of_array_buffer buf)

let read_bytes_from_file file_object = read_bytes (Brr.File.as_blob file_object)

let download_from_memory ~mime ~filename text =
  (* One possible problem is there's a size limit on the size of inline
     data. https://developer.mozilla.org/en-US/docs/web/http/basics_of_http/data_urls
     says 32MB in firefox and more in chrome/safari, so that seems good enough. *)
  let el =
    Brr.El.v
      ~at:
        [ Brr.At.v (Jstr.v "href")
            (match text with
            | `Jstr str ->
                Jstr.append
                  (Jstr.v [%string "data:%{mime};charset=utf-8,"])
                  (or_throw (Brr.Uri.encode str))
            | `Str_in_base64 str_in_base64 ->
                Jstr.append
                  (Jstr.v [%string "data:%{mime};base64,"])
                  (Jstr.v str_in_base64))
        ; Brr.At.v (Jstr.v "download") filename
        ; Brr.At.v (Jstr.v "style") (Jstr.v "display: none")
        ]
      (Jstr.v "a") []
  in
  Brr.El.append_children (Brr.Document.body Brr.G.document) [ el ];
  Brr.El.click el;
  Brr.El.remove el

let fetch url =
  let open Fut.Result_syntax in
  let* response = Brr_io.Fetch.url url in
  let* buf = Brr_io.Fetch.Body.array_buffer (Brr_io.Fetch.Response.as_body response) in
  Fut.ok (string_of_array_buffer buf)

let document = Jv.get Jv.global "document"
let get_element_by_id id = Jv.call document "getElementById" [| Jv.of_jstr id |]

type rpc = string * (bool * Jv.t -> Jv.t Fut.or_error)

let current_script =
  if Jv.is_undefined document
  then lazy (failwith "no currentScript, because no document")
  else
    (* We need to run this at toplevel, because currentScript is only defined when
       executing the toplevel of a script. *)
    let src = Jv.get (Jv.get document "currentScript") "src" in
    lazy (Jv.to_jstr (Jv.get (Jv.new' (Jv.get Jv.global "URL") [| src |]) "pathname"))

let rpc_name =
  let r = ref (-1) in
  fun () ->
    r := !r + 1;
    "rpc" ^ Int.to_string !r

type ww_cache = Brr_webworkers.Worker.t option ref

let ww_cache () = ref None

let rpc_with_progress : type q r.
       (?progress:(int -> unit) -> q -> r Fut.or_error)
    -> rpc
       * (   ?ww_cache:ww_cache
          -> ?local:bool
          -> ?progress:(int -> unit)
          -> q
          -> r Fut.or_error) =
 fun impl ->
  let rpc_name = rpc_name () in
  ( ( rpc_name
    , fun (has_progress, q) ->
        let progress =
          if has_progress
          then Some (fun i -> Brr_webworkers.Worker.G.post ("magic-string", i))
          else None
        in
        (Obj.magic (impl ?progress (Obj.magic (q : Jv.t) : q) : r Fut.or_error)
          : Jv.t Fut.or_error) )
  , fun ?ww_cache ?(local = false) ?progress arg ->
      let open Fut.Syntax in
      if local
      then impl ?progress arg
      else
        let worker =
          match ww_cache with
          | Some { contents = Some w } -> w
          | _ ->
              let w = Brr_webworkers.Worker.create (Lazy.force current_script) in
              Option.iter (fun r -> r := Some w) ww_cache;
              w
        in
        Brr_webworkers.Worker.post worker (rpc_name, (Option.is_some progress, arg));
        let* event =
          let rec loop () =
            let* event =
              Brr.Ev.next Brr_io.Message.Ev.message
                (Brr_webworkers.Worker.as_target worker)
            in
            match Brr_io.Message.Ev.data (Brr.Ev.as_type event) with
            | "magic-string", i ->
                Option.iter (fun f -> f i) progress;
                loop ()
            | _ -> Fut.return event
          in
          loop ()
        in
        (match ww_cache with
        | None -> Brr_webworkers.Worker.terminate worker
        | Some _ -> ());
        Fut.return
          (or_throw
             (Brr_io.Message.Ev.data (Brr.Ev.as_type event)
               : ((r, Jv.Error.t) Result.t, Jv.Error.t) Result.t)) )

let rpc impl =
  let rpc, f = rpc_with_progress (fun ?progress:_ q -> impl q) in
  (rpc, fun ?ww_cache ?local q -> f ?ww_cache ?progress:None ?local q)

let main (rpcs : rpc list) f =
  let rpcs = Hashtbl.of_seq (List.to_seq rpcs) in
  if Brr_webworkers.Worker.ami ()
  then
    let open Fut.Syntax in
    let rec loop () =
      let* event = Brr.Ev.next Brr_io.Message.Ev.message Brr.G.target in
      fut_await
        (let* () = Fut.return () in
         (* so all exns are async *)
         let data = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
         let rpc_name, rpc_arg = data in
         let impl =
           Hashtbl.find_opt rpcs rpc_name ||? failwith ("unknown rpc " ^ rpc_name)
         in
         impl rpc_arg)
        (fun res ->
          Brr_webworkers.Worker.G.post
            (res : ((Jv.t, Jv.Error.t) Result.t, Jv.Error.t) Result.t));
      loop ()
    in
    ignore (loop () : [ `Never_returns ] Fut.t)
  else f ()

let js_error f =
  try f () with e -> throw (Jv.Error.v (Jstr.of_string (Printexc.to_string e)))

module B = struct
  type 'a t = Jv.t -> 'a
  type 'a t' = 'a -> Jv.t

  let unit = ignore
  let jstr = Jv.to_jstr
  let string = Jv.to_string
  let bool = Jv.to_bool
  let int = Jv.to_int
  let magic = Stdlib.Obj.magic
  let jv = Fun.id
  let option = Jv.to_option
  let map f base jv = f (base jv)
  let t2 f1 f2 jv = (f1 (Jv.Jarray.get jv 0), f2 (Jv.Jarray.get jv 1))

  let t3 f1 f2 f3 jv =
    (f1 (Jv.Jarray.get jv 0), f2 (Jv.Jarray.get jv 1), f3 (Jv.Jarray.get jv 2))

  let t4 f1 f2 f3 f4 jv =
    ( f1 (Jv.Jarray.get jv 0)
    , f2 (Jv.Jarray.get jv 1)
    , f3 (Jv.Jarray.get jv 2)
    , f4 (Jv.Jarray.get jv 3) )

  let fun1 f1 fres f a1 = fres (Jv.apply f [| f1 a1 |])
  let unit' () = Jv.undefined
  let jstr' = Jv.of_jstr
  let string' = Jv.of_string
  let bool' = Jv.of_bool
  let int' = Jv.of_int
  let magic' = Stdlib.Obj.magic
  let jv' = Fun.id
  let option' f o = Jv.of_option ~none:Jv.null f o
  let map' f base a = base (f a)
  let promise_or_error' ok t = fut_to_promise t ~ok
  let t2' f1 f2 (a1, a2) = Jv.of_jv_list [ f1 a1; f2 a2 ]
  let t3' f1 f2 f3 (a1, a2, a3) = Jv.of_jv_list [ f1 a1; f2 a2; f3 a3 ]
  let t4' f1 f2 f3 f4 (a1, a2, a3, a4) = Jv.of_jv_list [ f1 a1; f2 a2; f3 a3; f4 a4 ]

  let fun1' ?(exn = false) f1 fres f =
    Jv.callback ~arity:1 (fun a1 ->
        if exn then js_error (fun () -> fres (f (f1 a1))) else fres (f (f1 a1)))

  let fun2' ?(exn = false) f1 f2 fres f =
    Jv.callback ~arity:2 (fun a1 a2 ->
        if exn
        then js_error (fun () -> fres (f (f1 a1) (f2 a2)))
        else fres (f (f1 a1) (f2 a2)))

  let fun3' ?(exn = false) f1 f2 f3 fres f =
    Jv.callback ~arity:3 (fun a1 a2 a3 ->
        if exn
        then js_error (fun () -> fres (f (f1 a1) (f2 a2) (f3 a3)))
        else fres (f (f1 a1) (f2 a2) (f3 a3)))

  let fun4' f1 f2 f3 f4 fres f =
    Jv.callback ~arity:4 (fun a1 a2 a3 a4 -> fres (f (f1 a1) (f2 a2) (f3 a3) (f4 a4)))

  let fun5' f1 f2 f3 f4 f5 fres f =
    Jv.callback ~arity:5 (fun a1 a2 a3 a4 a5 ->
        fres (f (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)))

  let fun6' f1 f2 f3 f4 f5 f6 fres f =
    Jv.callback ~arity:6 (fun a1 a2 a3 a4 a5 a6 ->
        fres (f (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6)))

  let magic_ () = (magic, magic')
end
