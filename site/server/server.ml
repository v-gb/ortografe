let read_until_close_or_max_size ~max_size stream =
  (* This is Dream's read_until_close function, with just an extra
     check when growing the buffer.
     With this, sending a 1GB makes the send reject 10x faster
     and use little memory instead of 3GB (which is more than the
     prod server has). *)
  let promise, resolver = Lwt.wait () in
  let length = ref 0 in
  let buffer = ref (Bigstringaf.create 4096) in
  let close _code =
    Bigstringaf.sub !buffer ~off:0 ~len:!length
    |> Bigstringaf.to_string
    |> Ok
    |> Lwt.wakeup_later resolver
  in
  let abort exn = Lwt.wakeup_later_exn resolver exn in
  let rec loop () =
    Dream.read_stream stream
      ~data:(fun chunk offset chunk_length _binary _fin ->
        let new_length = !length + chunk_length in
        if new_length > max_size
        then Lwt.wakeup_later resolver (Error `Too_big)
        else (
          if new_length > Bigstringaf.length !buffer
          then (
            let new_buffer = Bigstringaf.create (new_length * 2) in
            Bigstringaf.blit !buffer ~src_off:0 new_buffer ~dst_off:0 ~len:!length;
            buffer := new_buffer);
          Bigstringaf.blit chunk ~src_off:offset !buffer ~dst_off:!length
            ~len:chunk_length;
          length := new_length;
          loop ()))
      ~flush:loop
      ~ping:(fun buffer offset length ->
        Dream.pong_stream stream buffer offset length ~close ~exn:abort loop)
      ~pong:(fun _buffer _offset _length -> loop ())
      ~close ~exn:abort
  in
  loop ();
  promise

let limit_body_size ~max_size request =
  match%lwt read_until_close_or_max_size ~max_size (Dream.body_stream request) with
  | Error _ as e -> Lwt.return e
  | Ok body ->
      Dream.set_body request body;
      Lwt.return (Ok ())

let html_of_response ~title_unescaped ~body_unescaped =
  Printf.sprintf
    {|<html>
     <head>
     <style>
body {
  font-size: 1.1rem;
  line-height: 1.3;
  max-width: 50em;
}
pre {
  white-space: pre-wrap;
}
     </style>
     </head>
     <body><h3>%s</h3><pre>%s</pre></body>
     </html>|}
    title_unescaped body_unescaped

let error_has_body = Dream.new_field ~name:"error-has-body" ()

let my_error_template (error : Dream.error) debug_info suggested_response =
  (match Dream.field suggested_response error_has_body with
  | Some _ -> ()
  | None ->
      let status = Dream.status suggested_response in
      Dream.set_header suggested_response "Content-Type" Dream.text_html;
      Dream.set_body suggested_response
        (html_of_response
           ~title_unescaped:
             (Printf.sprintf "%d %s" (Dream.status_to_int status)
                (Dream.html_escape (Dream.status_to_string status)))
           ~body_unescaped:
             (match error.caused_by with
             | `Client -> ""
             | `Server -> Dream.html_escape debug_info)));
  Lwt.return suggested_response

let repo_root () =
  let root = ref (Filename.dirname Sys.executable_name) in
  while
    not
      (Sys.file_exists (Filename.concat !root ".git")
      || Sys.file_exists (Filename.concat !root ".jj"))
  do
    root := Filename.dirname !root;
    match !root with
    | "." | ".." | "/" -> failwith "server expects to run in container or in repo"
    | _ -> ()
  done;
  !root

let where_to_find_static_files () =
  if Sys.file_exists "/static" (* when in container *)
  then ("/static", `In_container true)
  else
    (* In dev, setup symlinks to the files in the repo, so we can just
       modify the files and reload without fiddling with the server. *)
    let repo_root = repo_root () in
    (* keep in sync with Dockerfile *)
    (Filename.concat repo_root "_build/default/site/static", `In_container false)

let respond_error_text status str =
  let code = Dream.status_to_int status and reason = Dream.status_to_string status in
  let response =
    Dream.response ~status
      ~headers:[ ("Content-Type", Dream.text_html) ]
      (html_of_response
         ~title_unescaped:
           (Printf.sprintf "Error %d: %s" code (Dream.html_escape reason))
         ~body_unescaped:(Dream.html_escape str))
  in
  Dream.set_field response error_has_body ();
  Lwt.return response

let hum_size_of_bytes n =
  Core.Byte_units.Short.to_string (Core.Byte_units.of_bytes_int n)

let stream_end = Dream.new_field ~name:"stream-end" ()

let actual_from_system root disk_path _request =
  (* Embarrasingly, Dream.from_filesystem reads the whole file into memory and then sends
     it. Two downloads of the 27MB ortografe_cli.exe in parallel were enough to crash the
     server into the 256MB limit. If we stream the data out, the memory usage is just 1MB per
     concurrent download (of files over 1MB). *)
  let file_path = Filename.concat root disk_path in
  Lwt.catch
    (fun () ->
      let%lwt stat =
        Lwt_unix.stat file_path (* Lwt_io.file_length is, uh, suboptimal *)
      in
      let%lwt ch = Lwt_io.open_file ~mode:Input file_path in
      if stat.st_size < 1_000_000
      then
        (* This branch is mostly to keep the logs clean. But it might be good as well to limit
           the allocation of bigstrings, because the gc tends to not be good at them. *)
        let%lwt contents =
          Lwt.finalize (fun () -> Lwt_io.read ch) (fun () -> Lwt_io.close ch)
        in
        Dream.respond ~headers:(Dream.mime_lookup file_path) contents
      else
        let promise, resolver = Lwt.wait () in
        let%lwt response =
          Dream.stream ~headers:(Dream.mime_lookup file_path) (fun stream ->
              let got_exn = Lwt.wakeup_later_exn resolver in
              let buf = Lwt_bytes.create 1_000_000 in
              let rec loop () =
                Lwt.on_any
                  (Lwt_io.read_into_bigstring ch buf 0 (Lwt_bytes.length buf))
                  (function
                    | 0 -> Lwt.wakeup_later resolver ()
                    | n ->
                        Dream.write_stream stream buf 0 n false false
                          ~close:(fun _ -> Lwt.wakeup_later resolver ())
                          ~exn:got_exn loop)
                  got_exn
              in
              Lwt.finalize
                (fun () ->
                  loop ();
                  promise)
                (fun () ->
                  let%lwt () = Lwt_io.close ch in
                  Dream.close stream))
        in
        Dream.set_field response stream_end promise;
        Lwt.return response)
    (fun _exn -> Dream.respond ~status:`Not_Found "")

let from_filesystem root path request =
  let disk_path, response_headers =
    let has_compressed_version =
      match path with
      | "Lexique383.gen.tsv" | "dict.js" -> true
      | _ -> String.ends_with path ~suffix:".bc.js"
    in
    if has_compressed_version
       && List.exists
            (fun s ->
              String.split_on_char ',' s
              |> List.exists (fun s -> String.trim s = "gzip"))
            (Dream.headers request "Accept-Encoding")
    then (path ^ ".gz", [ ("Content-Encoding", "gzip") ] @ Dream.mime_lookup path)
    else (path, [])
  in
  let%lwt response =
    (if true then actual_from_system else Dream.from_filesystem) root disk_path request
  in
  let cache =
    (* The big things we'd like to avoid repeating queries for are dict.js,
       the screenshot and the data (mostly for the clients to avoid downloading
       large stuff over and over in the last case). *)
    match path with
    | "dict.js" -> true
    | _ when String.ends_with path ~suffix:".bc.js" -> true
    | _ -> (
        match Filename.extension path with
        | ".png" | ".svg" | ".jpg" | ".csv" | ".tsv" -> true
        | _ -> false)
  in
  if cache then Dream.set_header response "Cache-Control" "max-age=3600";
  List.iter (fun (k, v) -> Dream.set_header response k v) response_headers;
  Lwt.return response

let embedded : Dict_gen_common.Dict_gen.embedded =
  { data_lexique_Lexique383_gen_tsv = Ortografe_embedded.data_lexique_Lexique383_gen_tsv
  ; extension_dict1990_gen_csv = Ortografe_embedded.extension_dict1990_gen_csv
  }

let define_client_ip : Dream.middleware =
 fun handler request ->
  let open Core in
  (match
     Dream.headers request "X-Forwarded-For"
     |> String.concat ~sep:","
     |> String.split ~on:','
     |> List.map ~f:String.strip
     |> List.filter ~f:(function "" -> false | _ -> true)
     |> List.hd
   with
  | None -> ()
  | Some addr -> Dream.set_client request addr);
  handler request

let logger = function
  | `Long -> Dream.logger
  | `Short ->
      let open Core in
      let ofday time_ns =
        time_ns
        |> Time_ns.to_int63_ns_since_epoch
        |> Int63.to_int_exn
        |> (fun i -> i mod (86400 * 1_000_000_000))
        |> Time_ns.Span.of_int_ns
        |> Time_ns.Ofday.of_span_since_start_of_day_exn
        |> Time_ns.Ofday.to_millisecond_string
      in
      let next_id =
        let r = ref (-1) in
        fun () ->
          r := !r + 1;
          !r
      in
      fun handler request ->
        let id = next_id () in
        (* format for request  : 13:04:49.553 GET ID IP PATH UA"
           format for response : 13:04:49.553 200 ID in INTus"
        *)
        if String.is_suffix (Dream.target request) ~suffix:".jpg"
           || String.is_suffix (Dream.target request) ~suffix:".svg"
           || String.is_suffix (Dream.target request) ~suffix:".png"
           || String.is_suffix (Dream.target request) ~suffix:".ico"
           || String.is_suffix (Dream.target request) ~suffix:".css"
        then handler request
        else
          let before = Time_ns.now () in
          let method_ = Dream.method_to_string (Dream.method_ request) in
          let user_agent =
            let user_agent = String.concat (Dream.headers request "User-Agent") in
            match String.rsplit2 user_agent ~on:' ' with
            | None -> user_agent
            | Some (_, s) -> (
                match String.rsplit2 s ~on:'/' with
                | Some (("Safari" | "Firefox" | "Chrome" | "Edg"), _) -> s
                | _ -> user_agent)
          in
          let client =
            let s = Dream.client request in
            match String.split s ~on:':' with
            | [ host; _port ] -> host
            | _ -> s (* ipv6 probably *)
          in
          Stdlib.prerr_endline
            [%string
              "%{ofday before} %{method_} %{id#Int} %{client} %{Dream.target request}  \
               %{user_agent}"];
          Stdlib.flush stderr;
          Lwt.try_bind
            (fun () -> handler request)
            (fun response ->
              let after1 = Time_ns.now () in
              let duration = Time_ns.Span.to_int_us (Time_ns.diff after1 before) in
              let status = Dream.status response in
              let bcolor, ecolor =
                if Dream.is_server_error status
                then ("[31m", "[39m")
                else if Dream.is_client_error status
                then ("[33m", "[39m")
                else ("[32m", "[39m")
              in
              Stdlib.prerr_endline
                [%string
                  "%{ofday after1} %{bcolor}%{Dream.status_to_int status#Int}%{ecolor} \
                   %{id#Int} %{client} in %{duration#Int}us"];
              Stdlib.flush stderr;
              (match Dream.field response stream_end with
              | None -> ()
              | Some promise ->
                  ignore
                    (Lwt.on_termination promise (fun () ->
                         let bcolor, ecolor = ("[35m", "[39m") in
                         let after2 = Time_ns.now () in
                         let duration =
                           Time_ns.Span.to_int_us (Time_ns.diff after2 before)
                         in
                         Stdlib.prerr_endline
                           [%string
                             "%{ofday after2} %{bcolor}DON%{ecolor} %{id#Int} \
                              %{client} in %{duration#Int}us"];
                         Stdlib.flush stderr)));
              Lwt.return response)
            (fun exn ->
              Stdlib.prerr_string [%string "Aborted by: %{Exn.to_string exn}"];
              Stdlib.flush stderr;
              Lwt.fail exn)

let redirect handler request =
  match Dream.header request "Host" with
  | Some ("ortografe-server.fly.dev" | "www.orthographe-rationnelle.info") ->
      (* Try to compel google search into showing the address below instead of the
         addresses above. *)
      Dream.redirect ~code:308 request
        ("https://orthographe-rationnelle.info" ^ Dream.target request)
  | _ -> handler request

let memory () =
  let rss = ref Core.Byte_units.zero in
  let pss = ref Core.Byte_units.zero in
  let parse s =
    Base.String.strip s
    |> Base.String.chop_suffix_exn ~suffix:" kB"
    |> int_of_string
    |> Float.of_int
    |> Core.Byte_units.of_kilobytes
  in
  Core.List.iter (Core.In_channel.read_lines "/proc/self/smaps") ~f:(fun s ->
      match Core.String.chop_prefix s ~prefix:"Rss:" with
      | Some s -> rss := Core.Byte_units.( + ) !rss (parse s)
      | None -> (
          match Core.String.chop_prefix s ~prefix:"Pss:" with
          | Some s -> pss := Core.Byte_units.( + ) !pss (parse s)
          | None -> ()));
  [%sexp `rss (!rss : Core.Byte_units.t), `pss (!pss : Core.Byte_units.t)]

let _print_memory () =
  Core.print_s (memory ());
  Gc.compact ();
  Core.print_s (memory ())

let time f =
  let open Core in
  let before = Time_ns.now () in
  let res = f () in
  (res, Time_ns.Span.to_int_ms (Time_ns.diff (Time_ns.now ()) before))

let get_dict () =
  let dict_search =
    lazy
      (let before = memory () in
       let t, ms =
         (* The persisted index makes loading much faster (100ms vs 750ms), and avoids
            the need for compaction after to reduce memory usage (27MB->100MB->58MB
            without persisting, 27MB->65MB with persisting). *)
         time (fun () ->
             Dict_search.Erofa.of_persist Data.data_homemade_dict_erofa_dict_search)
       in
       let after = memory () in
       (let open Core in
        print_s
          [%sexp "loading index", (before : Sexp.t), `ms (ms : int), (after : Sexp.t)]);
       t)
  in
  let oe_pattern = lazy (Core.String.Search_pattern.create "œ") in
  fun request ->
    match Dream.query request "q" with
    | None -> respond_error_text `Bad_Request "no ?q parameter"
    | Some term ->
        let responses =
          if String.length term > 50
          then []
          else Dict_search.Erofa.search (Lazy.force dict_search) term ~limit:10
        in
        let rows =
          Core.List.map
            ~f:(fun (as_, b, c, flags) ->
              let plural = if flags.implied_plural then "(s)" else "" in
              let as_display, b_display =
                if List.is_empty as_
                then (b ^ plural, "-")
                else
                  (String.concat "<br>" (List.map (fun a -> a ^ plural) as_), b ^ plural)
              in
              let c_display = if String.equal b c then "-" else c ^ plural in
              let link =
                let b_for_url =
                  (* lerobert ignores all diacritics (which is fine), but also drops œ on
                     the floor instead of turning it into oe, which is less fine, so do it
                     ourselves. *)
                  Dream.to_percent_encoded
                    (Core.String.Search_pattern.replace_all (Lazy.force oe_pattern)
                       ~in_:b ~with_:"oe")
                in
                [%string
                  {|<a href="https://dictionnaire.lerobert.com/definition/%{b_for_url}" style="background-color: #e22027; border-radius: 50%; color: white; font-weight: bold; text-align:center; display: inline-block; width: 1.3em; height: 1.3em; text-decoration: none;">R</a>|}]
              in
              [%string
                {|<tr><td>%{link}</td><td>%{as_display}</td><td>%{b_display}</td><td>%{c_display}</td></tr>|}])
            responses
          |> String.concat "\n"
        in
        let html =
          [%string
            {|
<table class="notranscribe">
<thead>
  <tr>
    <th></th>
    <th>Autre<br>orthographe</th>
    <th>Recommandé<br>depuis 1990</th>
    <th>Recommandé<br>par Érofa</th>
  </tr>
</thead>
<tbody>
%{rows}
</tbody>
</table>
|}]
        in
        Dream.html html

let post_conv ~max_input_size ~staged request =
  match%lwt limit_body_size ~max_size:max_input_size request with
  | Error `Too_big ->
      respond_error_text `Payload_Too_Large
        ("max size: " ^ hum_size_of_bytes max_input_size)
  | Ok () -> (
      match%lwt Dream.multipart ~csrf:false request with
      | `Ok l -> (
          let rules, rest =
            List.partition_map
              (fun ((name, values) as pair) ->
                match name with
                | "custom" -> (
                    match values with
                    | (_, value) :: _ ->
                        Left (Dict_gen_common.Dict_gen.custom_rule value)
                    | _ -> Left None)
                | _ -> (
                    match Dict_gen_common.Dict_gen.of_name_builtin name with
                    | Some rule -> Left (Some rule)
                    | None -> Right pair))
              l
          in
          let rules = List.filter_map Fun.id rules in
          match rest with
          | [ ("file", [ (fname, fcontents) ]) ] -> (
              let fname = fname ||? "unnamed.txt" in
              let ext = Filename.extension fname in
              Dream.log "upload ext:%S size:%s rules:%s" ext
                (hum_size_of_bytes (String.length fcontents))
                (String.concat "," (List.map Dict_gen_common.Dict_gen.name rules));
              let dict, plurals_in_s =
                match rules with
                | [] ->
                    (Stdlib.Hashtbl.find_opt (Lazy.force Ortografe_embedded.erofa), None)
                | [ rule ] when Dict_gen_common.Dict_gen.name rule = "1990" ->
                    ( Stdlib.Hashtbl.find_opt (Lazy.force Ortografe_embedded.rect1990)
                    , None )
                | _ ->
                    let dict, (metadata : Dict_gen_common.Dict_gen.metadata) =
                      (Lazy.force staged) rules
                    in
                    (dict, metadata.plurals_in_s)
              in
              match
                Ortografe.convert_string ~ext fcontents
                  ~options:
                    { convert_uppercase = true
                    ; dict
                    ; interleaved = true
                    ; plurals_in_s = plurals_in_s ||? Some "s"
                    }
              with
              | exception e ->
                  let str =
                    match e with Failure s -> s | _ -> Sexp_with_utf8.exn_to_string e
                  in
                  respond_error_text (`Status 422) str
              | None -> respond_error_text (`Status 422) ("unsupported file type " ^ ext)
              | Some (`ext new_ext, new_body) ->
                  let new_fname = Filename.remove_extension fname ^ "-conv" ^ new_ext in
                  Dream.respond
                    ~headers:
                      (Dream.mime_lookup new_fname
                      @ [ ( "Content-Disposition"
                          , Printf.sprintf "attachment; filename=\"%s\""
                              (Dream.to_percent_encoded new_fname) )
                        ])
                    new_body)
          | _ -> respond_error_text `Bad_Request "")
      | _ -> respond_error_text `Bad_Request "")

let run ?(log = true) ?port ?tls () =
  let staged = lazy (Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded)) in
  (* We have 250MB of memory in prod. If we say:
     - 60MB used for random things, like the OS or the exe
     - 70MB used for the persisted index
     - 25% of gc space overhead
     we'd have 96MB left. Give that we need to have both old and new data at once
     in memory, that would mean 48MB max size. I'll use 40MB to give a bit of headroom
     (like the size of the zip file before/after). In practice, maybe we should decrease
     this further, because processing 40MB of xml is going to block the server for maybe
     10s. *)
  let max_input_size = 50 * 1024 * 1024 in
  Ortografe.max_size := 45 * 1024 * 1024;
  let static_root, `In_container in_container = where_to_find_static_files () in
  Dream.run ?port ?tls
    ~interface:"0.0.0.0" (* apparently only listens on lo otherwise *)
    ~error_handler:(Dream.error_template my_error_template)
  @@ (if in_container (* approximates "in prod" *) then define_client_ip else Fun.id)
  @@ (if log then logger `Short else Fun.id)
  @@ redirect
  @@ Dream.router
       [ Dream.get "/dict" (get_dict ())
       ; Dream.post "/conv" (post_conv ~max_input_size ~staged)
       ; Dream.get "/" (from_filesystem static_root "index.html")
       ; Dream.get "/regles/perso" (from_filesystem static_root "regles_perso.html")
       ; Dream.get "/static/**" (Dream.static ~loader:from_filesystem static_root)
       ]

let run_for_bench ~log ?port ?tls () =
  (* benching with [ab -n 50000 -c 500] shows:
     - 25k requests/s with no logging and no tls
     - 22k requests/s with no logging
     - 320 requests/s with tls, logging or not
     for comparison, a trivial golang server with GOMAXPROCS=1
     - 24k requests/s with no logging and no tls
     - 630 requests/s with tls and no logging
     So tls is not efficient, but in our deployment, the infrastructure
     takes cares of it.
  *)
  Dream.run ?port ?tls
  @@ (if log then Dream.logger else Fun.id)
  @@ Dream.router [ Dream.get "/hello" (fun _req -> Lwt.return (Dream.response "hi")) ]

let main () =
  let module C = Cmdliner in
  let open Cmdliner.Term.Syntax in
  let cmd =
    C.Cmd.group (C.Cmd.info "ortografe")
      [ C.Cmd.v (C.Cmd.info "serve")
          (let+ port =
             C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.int) None (C.Arg.info [ "p" ]))
           and+ tls = C.Arg.value (C.Arg.flag (C.Arg.info [ "tls" ]))
           and+ no_log = C.Arg.value (C.Arg.flag (C.Arg.info [ "no-log" ])) in
           run ?port ~log:(not no_log) ~tls ())
      ; C.Cmd.v
          (C.Cmd.info "serve-for-bench")
          (let+ port =
             C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.int) None (C.Arg.info [ "p" ]))
           and+ tls = C.Arg.value (C.Arg.flag (C.Arg.info [ "tls" ]))
           and+ no_log = C.Arg.value (C.Arg.flag (C.Arg.info [ "no-log" ])) in
           run_for_bench ?port ~log:(not no_log) ~tls ())
      ]
  in
  exit (C.Cmd.eval cmd)

let () = main ()
