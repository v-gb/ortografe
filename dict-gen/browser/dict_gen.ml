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

let generate ?profile ?progress embedded rules =
  let t1 = Stdlib.Sys.time () in
  let buf = Buffer.create 1_000_000 in
  let `Stats stats =
    Dict_gen_common.Dict_gen.gen
      ?profile
      ?progress
      ~rules
      ~all:false
      ~output:(Buffer.add_string buf)
      ~json_to_string:Brrex.json_to_string
      (`Embedded embedded)
  in
  let t2 = Stdlib.Sys.time () in
  let sexp_str =
    let open Base in
    Sexplib.Sexp.to_string_hum [%sexp (stats : Sexp.t), (t2 -. t1 : float)]
  in
  (Buffer.contents buf, sexp_str)

type selected_rules = Dict_gen_common.Dict_gen.rules
let currently_selected_rules id_prefix =
  let builtin = Lazy.force Dict_gen_common.Dict_gen.all_builtin in
  let selected_builtins =
    List.filter builtin ~f:(fun rule ->
        Jv.get
          (Brrex.get_element_by_id
             (Jstr.of_string
                (id_prefix ^ Dict_gen_common.Dict_gen.name rule)))
          "checked"
        |> Jv.to_bool)
  in
  let custom_rule =
    Jv.get
      (Brrex.get_element_by_id (Jstr.of_string (id_prefix ^ "custom")))
      "value"
    |> Jv.to_string
    |> Dict_gen_common.Dict_gen.custom_rule
  in
  let selected_rules : selected_rules = Option.to_list custom_rule @ selected_builtins in
  let selection_text =
    if List.is_empty selected_rules
    then "rien de sélectionné"
    else List.map selected_rules ~f:Dict_gen_common.Dict_gen.name
         |> String.concat ~sep:" "
  in
  let selection_is_nonempty = not (List.is_empty selected_rules) in
  Jv.of_list Fn.id [ Jv.Id.to_jv selected_rules
                   ; Jv.of_string selection_text
                   ; Jv.of_bool selection_is_nonempty ]

let html_fragment () =
  Dict_gen_common.Dict_gen.all_selection_html
    ~url_prefix:"/" ~id_prefix:"checkbox-" ~name_prefix:"load-" ()
  |> Jv.of_string

let fetch url =
  let open Fut.Result_syntax in
  let* response = Brr_io.Fetch.url url in
  let* text = Brr_io.Fetch.Body.text (Brr_io.Fetch.Response.as_body response) in
  Fut.return (Ok (Jstr.to_string text))

let on_message ?progress ~k (lexique_url, dict1990_url, rules, profile) =
  let open Fut.Result_syntax in
  let* embedded =
    time_fut ~profile "fetch" (fun () ->
        let* data_lexique_Lexique383_gen_tsv = fetch lexique_url in
        Option.iter progress ~f:(fun f -> f 5);
        let* extension_dict1990_gen_csv = fetch dict1990_url in
        Option.iter progress ~f:(fun f -> f 10);
        Fut.ok { Dict_gen_common.Dict_gen.data_lexique_Lexique383_gen_tsv
               ; extension_dict1990_gen_csv
          })
  in
  match
    generate
      ?progress:(Option.map progress ~f:(fun f x -> f (10 + x * 9 / 10)))
      ~profile
      embedded
      rules
  with
  | exception e -> Fut.error (Jv.Error.v (Jstr.of_string (Exn.to_string e)))
  | v -> Fut.ok (k v)

let rpc : type q r.
          ?progress:_
          -> local:bool
          -> path:string
          -> (q -> (r, 'b) Result.t Fut.t)
          -> q
          -> (q -> _)
          -> (r, 'b) Result.t Fut.t
  = fun ?progress ~local ~path impl arg constr ->
  (* This function ensures well typedness, by tying the result in the worker
     case and in the non-worker case. *)
  let open Fut.Syntax in
  if local
  then impl arg
  else (
    let worker = Brr_webworkers.Worker.create (Jstr.of_string (path ^ "/dict_gen.bc.js")) in
    Brr_webworkers.Worker.post worker (constr arg);
    let rec loop () =
      let* event = Brr.Ev.next Brr_io.Message.Ev.message
                     (Brr_webworkers.Worker.as_target worker) in
      match Brr_io.Message.Ev.data (Brr.Ev.as_type event) with
      | ("magic-string", i) -> Option.iter progress ~f:(fun f -> f i); loop ()
      | _ -> Fut.return event
    in
    let* event = loop () in
    Brr_webworkers.Worker.terminate worker;
    Fut.return
      (Brrex.or_throw
         (Brr_io.Message.Ev.data
            (Brr.Ev.as_type event)
          : ((r, Jv.Error.t) Result.t, Jv.Error.t) Result.t))
  )

let generate_in_worker path (lexique_url : Jstr.t) (dict1990_url : Jstr.t) rules (n : Jv.t) profile progress =
  (* We need to run this in a worker, otherwise the loading animation doesn't actually
   * animate, which we kind of want it to, since the a 2s of waiting is on the longer
   * side. *)
  let path = Jv.to_string path in
  let rules = (Stdlib.Obj.magic : Jv.t -> selected_rules) rules in
  let progress =
    Jv.to_option
      (fun js_f i -> ignore (Jv.apply js_f [|Jv.of_int i|]))
      progress
  in
  let n = Jv.to_int n in
  let profile = Jv.to_bool profile in
  Brrex.fut_to_promise
    ~ok:(fun (dict, duration) -> Jv.of_jv_list [ Jv.of_string dict ; Jv.of_string duration ])
    (rpc
       ?progress
       ~local:(n = 0)
       ~path
       (on_message ?progress ~k:Fn.id)
       (lexique_url, dict1990_url, rules, profile)
       (fun arg -> `On_message arg))

let staged_generate =
  Jv.callback ~arity:2
    (fun lexique_url dict1990_url ->
      Brrex.fut_to_promise
        ~ok:Fn.id
        (let open Fut.Result_syntax in
         let* embedded =
           let* data_lexique_Lexique383_gen_tsv = fetch lexique_url in
           let* extension_dict1990_gen_csv = fetch dict1990_url in
           Fut.ok { Dict_gen_common.Dict_gen.data_lexique_Lexique383_gen_tsv
                  ; extension_dict1990_gen_csv
             }
         in
         let next_stage = Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded) in
         Fut.ok (
             Jv.callback ~arity:1 (fun (x : selected_rules) ->
                 let f, _meta = next_stage x in
                 Jv.callback ~arity:1 (fun jstr ->
                     Jv.of_option
                       ~none:Jv.null
                       Jv.of_string
                       (f (Jv.to_string jstr)))))))

let () =
  if Brr_webworkers.Worker.ami ()
  then
    let open Fut.Syntax in
    Brrex.fut_await
      (let* event = Brr.Ev.next Brr_io.Message.Ev.message Brr.G.target in
       let data = Brr_io.Message.Ev.data (Brr.Ev.as_type event) in
       match data with
       | `On_message data ->
          on_message data ~k:Jv.repr
            ~progress:(fun i -> Brr_webworkers.Worker.G.post ("magic-string", i)))
      (fun res ->
        Brr_webworkers.Worker.G.post
          (res : ((Jv.t, Jv.Error.t) Result.t, Jv.Error.t) Result.t))
  else
    Js_of_ocaml.Js.export "dict_gen"
      (Js_of_ocaml.Js.Unsafe.inject
         (Jv.obj [| "generate", Jv.callback ~arity:7 generate_in_worker
                  ; "staged_generate", staged_generate
                  ; "html_fragment", Jv.callback ~arity:1 html_fragment
                  ; "currently_selected_rules", Jv.callback ~arity:1 currently_selected_rules
            |]))
