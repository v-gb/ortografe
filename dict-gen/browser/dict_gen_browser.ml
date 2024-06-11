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

let compute_dict ?profile ?progress embedded rules =
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
  (selected_rules, selection_text, selection_is_nonempty)

let html_fragment () =
  Dict_gen_common.Dict_gen.all_selection_html
    ~url_prefix:"/" ~id_prefix:"checkbox-" ~name_prefix:"load-" ()

let embedded ~lexique_url ~dict1990_url =
  let open Fut.Result_syntax in
  let* data_lexique_Lexique383_gen_tsv = Brrex.fetch lexique_url in
  let* extension_dict1990_gen_csv = Brrex.fetch dict1990_url in
  Fut.ok { Dict_gen_common.Dict_gen.data_lexique_Lexique383_gen_tsv
         ; extension_dict1990_gen_csv
         }

let generate_ww_rpc, generate_ww =
  (* We need to run this in a worker, otherwise the loading animation doesn't actually
     animate, which we kind of want it to, since the a 2s of waiting is on the longer
     side. *)
  Brrex.rpc_with_progress (fun ?progress (lexique_url, dict1990_url, rules, profile) ->
      let open Fut.Result_syntax in
      let* embedded =
        time_fut ~profile "fetch" (fun () ->
            let* embedded = embedded ~lexique_url ~dict1990_url in
            Option.iter progress ~f:(fun f -> f 10);
            Fut.ok embedded)
      in
      match
        compute_dict
          ?progress:(Option.map progress ~f:(fun f x -> f (10 + x * 9 / 10)))
          ~profile
          embedded
          rules
      with
      | exception e -> Fut.error (Jv.Error.v (Jstr.of_string (Exn.to_string e)))
      | v -> Fut.ok v
    )
  
let generate =
  Brrex.B.(
    fun5'
      jstr
      jstr
      (magic : Jv.t -> selected_rules)
      bool
      (option (fun1 int' unit))
      (promise_or_error'
         (t2' string' string')))
    (fun lexique_url dict1990_url rules profile progress ->
      generate_ww ?progress (lexique_url, dict1990_url, rules, profile))

let cached cache (type a r) key ~eq (v : a) (f : int -> a -> r) =
  match Jv.to_option (Stdlib.Obj.magic : Jv.t -> a * r * int) (Jv.get cache key) with
  | Some (v', r, _) when eq v v' -> r
  | opt ->
     let i =
       match opt with
       | Some (_, _, i) -> i + 1
       | None -> 0
     in
     let r = f i v in
     Jv.set cache key (Stdlib.Obj.magic (v, r, i : a * r * int) : Jv.t);
     r

let staged_generate =
  Brrex.B.(fun2' jv (t3 string jstr jstr)
             (promise_or_error'
                (map' fst (fun1' string (option' string')))))
    (fun cache (prefix, csv1, csv2) ->
      let open Fut.Result_syntax in
      let* next_stage =
        cached cache "next_stage"
          ~eq:[%equal: Jstr.t * Jstr.t]
          (csv1, csv2)
          (fun i (lexique_url, dict1990_url) ->
            let* embedded = embedded ~lexique_url ~dict1990_url in
            Fut.ok (Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded), i))
      in
      let selection_rules, selection_text, _ = currently_selected_rules prefix in
      let dict =
        cached cache "dict"
          ~eq:[%equal: string * (_ * int)]
          (selection_text, next_stage)
          (fun _ (_selection_text, (next_stage, _)) -> next_stage selection_rules)
      in
      Fut.ok dict)

let main () =
  Brrex.main
    [ generate_ww_rpc ]
    (fun () ->
      Js_of_ocaml.Js.export "dict_gen_browser"
        (Js_of_ocaml.Js.Unsafe.inject
           (Jv.obj [| "generate", generate
                    ; "download_from_memory",
                      Brrex.B.(fun3' string jstr jstr unit')
                        (fun mime filename content ->
                          Brrex.download_from_memory
                            ~mime ~filename (`Jstr content))
                    ; "staged_generate", staged_generate
                    ; "html_fragment", Brrex.B.(fun1' unit string') html_fragment
                    ; "currently_selected_rules",
                        Brrex.B.(fun1' string (t3' Jv.Id.to_jv string' bool'))
                          currently_selected_rules
              |])))
