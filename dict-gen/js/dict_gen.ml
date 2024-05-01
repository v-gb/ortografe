open Base

let time_fut ~profile name f =
  if profile then (
    let open Fut.Syntax in
    let t1 = Stdlib.Sys.time () in
    let* x = f () in
    let t2 = Stdlib.Sys.time () in
    Stdlib.prerr_endline [%string "%{name}: %{Float.to_string_hum ~decimals:2 (t2 -. t1)}s"];
    Fut.return x
  ) else f ()


let json_to_string v =
  let rec to_js = function
    | `Bool b -> Jv.of_bool b
    | `String s -> Jv.of_string s
    | `Assoc l ->
       List.map ~f:(fun (k, v) -> (k, to_js v)) l
       |> Array.of_list
       |> Jv.obj
  in
  Jstr.to_string (Brr.Json.encode (to_js v))

let generate ?profile embedded rules =
  let t1 = Stdlib.Sys.time () in
  let buf = Buffer.create 1_000_000 in
  let `Stats stats =
    Dict_gen_common.Dict_gen.gen
      ?profile
      ~rules
      ~all:false
      ~output:(Buffer.add_string buf)
      ~json_to_string
      (`Embedded embedded)
  in
  let t2 = Stdlib.Sys.time () in
  let sexp_str =
    let open Base in
    Sexplib.Sexp.to_string_hum [%sexp (stats : Sexp.t), (t2 -. t1 : float)]
  in
  (Buffer.contents buf, sexp_str)

let get_element_by_id id =
  Jv.call (Jv.get Jv.global "document") "getElementById" [| Jv.of_jstr id |]

let currently_selected_rules id_prefix =
  let rules = Lazy.force Dict_gen_common.Dict_gen.all in
  let selected_rules =
    List.filter rules ~f:(fun rule ->
        Jv.get
          (get_element_by_id
             (Jstr.of_string
                (id_prefix ^ Dict_gen_common.Dict_gen.name rule)))
          "checked"
        |> Jv.to_bool)
  in
  let selection_text =
    if List.is_empty selected_rules
    then "rien de sélectionné"
    else List.map selected_rules ~f:Dict_gen_common.Dict_gen.name
         |> String.concat ~sep:" "
  in
  let selection_is_nonempty = not (List.is_empty selected_rules) in
  Jv.of_list Fn.id [ Jv.Id.to_jv (selected_rules : Dict_gen_common.Dict_gen.rules)
                   ; Jv.of_string selection_text
                   ; Jv.of_bool selection_is_nonempty ]

let rules () =
  let rules = Lazy.force Dict_gen_common.Dict_gen.all in
  Jv.of_list (fun rule ->
      Jv.obj [| "name", Jv.of_string (Dict_gen_common.Dict_gen.name rule)
              ; "html", Jv.of_string (Dict_gen_common.Dict_gen.html rule
                                        ~id_prefix:"checkbox-" ~name_prefix:"load-")
             |]) rules

let fetch url =
  let open Fut.Result_syntax in
  let* response = Brr_io.Fetch.url url in
  let* text = Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body response) in
  Fut.return (Ok (Jstr.to_string text))

let on_message (lexique_url, dict1990_url, rules, profile) ~k =
  let open Fut.Result_syntax in
  let* embedded =
    time_fut ~profile "fetch" (fun () ->
        let* data_lexique_Lexique383_gen_tsv = fetch lexique_url in
        let* extension_dict1990_gen_csv = fetch dict1990_url in
        Fut.ok { Dict_gen_common.Dict_gen.data_lexique_Lexique383_gen_tsv
               ; extension_dict1990_gen_csv
          })
  in
  match generate ~profile embedded rules with
  | exception e -> Fut.error (Jv.Error.v (Jstr.of_string (Exn.to_string e)))
  | v -> Fut.ok (k v)

let rpc : type q r.
          local:bool
          -> path:string
          -> (q -> (r, 'b) Result.t Fut.t)
          -> q
          -> (q -> _)
          -> (r, 'b) Result.t Fut.t
  = fun ~local ~path impl arg constr ->
  (* This function ensures well typedness, by tying the result in the worker
     case and in the non-worker case. *)
  let open Fut.Syntax in
  if local
  then impl arg
  else (
    let worker = Brr_webworkers.Worker.create (Jstr.of_string (path ^ "/dict_gen.bc.js")) in
    Brr_webworkers.Worker.post worker (constr arg);
    let* event = Brr.Ev.next Brr_io.Message.Ev.message
                   (Brr_webworkers.Worker.as_target worker) in
    Brr_webworkers.Worker.terminate worker;
    Fut.return (Brr_io.Message.Ev.data (Brr.Ev.as_type event) : (r, Jv.Error.t) Result.t)
  )

let generate_in_worker path (lexique_url : Jstr.t) (dict1990_url : Jstr.t) rules (n : Jv.t) profile =
  (* We need to run this in a worker, otherwise the loading animation doesn't actually
   * animate, which we kind of want it to, since the a 2s of waiting is on the longer
   * side. *)
  let path = Jv.to_string path in
  let rules = (Stdlib.Obj.magic : Jv.t -> Dict_gen_common.Dict_gen.rules) rules in
  let n = Jv.to_int n in
  let profile = Jv.to_bool profile in
  Fut.to_promise
    ~ok:(fun (dict, duration) -> Jv.of_jv_list [ Jv.of_string dict ; Jv.of_string duration ])
    (rpc
       ~local:(n = 0)
       ~path
       (on_message ~k:Fn.id)
       (lexique_url, dict1990_url, rules, profile)
       (fun arg -> `On_message arg))

let () =
  if Brr_webworkers.Worker.ami ()
  then
    let open Fut.Syntax in
    Fut.await
      (let* event = Brr.Ev.next Brr_io.Message.Ev.message Brr.G.target in
       let data = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
       match data with
       | `On_message data -> on_message data ~k:Jv.repr)
      (fun res -> Brr_webworkers.Worker.G.post (res : (Jv.t, Jv.Error.t) Result.t))
  else
    Js_of_ocaml.Js.export "dict_gen"
      (Js_of_ocaml.Js.Unsafe.inject
         (Jv.obj [| "generate", Jv.callback ~arity:6 generate_in_worker
                  ; "rules", Jv.callback ~arity:1 rules
                  ; "currently_selected_rules", Jv.callback ~arity:1 currently_selected_rules
            |]))
