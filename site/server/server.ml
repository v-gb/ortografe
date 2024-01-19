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
          if new_length > Bigstringaf.length !buffer then begin
              let new_buffer = Bigstringaf.create (new_length * 2) in
              Bigstringaf.blit
                !buffer ~src_off:0 new_buffer ~dst_off:0 ~len:!length;
              buffer := new_buffer
            end;
          Bigstringaf.blit
            chunk ~src_off:offset !buffer ~dst_off:!length ~len:chunk_length;
          length := new_length;
          loop ()))
      ~flush:loop
      ~ping:(fun buffer offset length ->
        Dream.pong_stream stream buffer offset length ~close ~exn:abort loop)
      ~pong:(fun _buffer _offset _length ->
        loop ())
      ~close
      ~exn:abort
  in
  loop ();
  promise

let limit_body_size ~max_size request =
  match%lwt read_until_close_or_max_size ~max_size (Dream.body_stream request) with
  | Error _ as e -> Lwt.return e
  | Ok body ->
     Dream.set_body request body;
     Lwt.return (Ok ())


let error_has_body = Dream.new_field ~name:"error-has-body" ()

let my_error_template (error : Dream.error) debug_info suggested_response =
  (match Dream.field suggested_response error_has_body with
   | Some _ -> ()
   | None ->
      let status = Dream.status suggested_response in
      Dream.set_header suggested_response "Content-Type" Dream.text_html;
      Dream.set_body suggested_response
        (Printf.sprintf "<html><body> <h1>%d %s</h1><pre>%s</pre> </body></html>"
           (Dream.status_to_int status)
           (Dream.html_escape (Dream.status_to_string status))
           (match error.caused_by with
            | `Client -> ""
            | `Server -> Dream.html_escape debug_info)));
  Lwt.return suggested_response

let repo_root () =
  let root = ref (Filename.dirname Sys.executable_name) in
  while not (Sys.file_exists (Filename.concat !root ".git")
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
  then "/static"
  else
    (* In dev, setup symlinks to the files in the repo, so we can just
       modify the files and reload without fiddling with the server. *)
    let repo_root = repo_root () in
    (* keep in sync with Dockerfile *)
    Filename.concat repo_root "_build/default/site/static"

let respond_error_text status str =
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  let response =
    Dream.response
      ~status
      ~headers:["Content-Type", Dream.text_html]
      (Printf.sprintf
         "<html>
          <body>
          <h1>Error %d: %s</h1>
          <pre>%s</pre>
          </body>
          </html>"
         code (Dream.html_escape reason) (Dream.html_escape str))
  in
  Dream.set_field response error_has_body ();
  Lwt.return response

let hum_size_of_bytes n =
  Core.Byte_units.Short.to_string (Core.Byte_units.of_bytes_int n)

let from_filesystem root path request =
  let disk_path, response_headers =
    match path with
    | "dict.js" ->
       if
         List.exists (fun s ->
             String.split_on_char ',' s
             |> List.exists (fun s -> String.trim s = "gzip"))
           (Dream.headers request "Accept-Encoding")
       then "dict.js.gz", [("Content-Encoding", "gzip")]
       else path, []
    | _ -> path, []
  in
  let%lwt response = Dream.from_filesystem root disk_path request in
  let cache =
    (* the big things we'd like to avoid repeating queries for are dict.js
       and the screenshot *)
    match path with
    | "dict.js" -> true
    | _ ->
       match Filename.extension path with
       | ".png" | ".svg" | ".jpg" -> true
       | _ -> false
  in
  if cache
  then Dream.set_header response "Cache-Control" "max-age=3600";
  List.iter
    (fun (k, v) -> Dream.set_header response k v)
    response_headers;
  Lwt.return response

let run ?(log = true) ?port ?tls ?(max_input_size = 50 * 1024 * 1024) () =
  Ortografe.max_size := max_input_size * 2; (* xml can be quite large when decompressed *)
  let static_root = where_to_find_static_files () in
  Dream.run ?port ?tls
    ~interface:"0.0.0.0" (* apparently only listens on lo otherwise *)
    ~error_handler:(Dream.error_template my_error_template)
  @@ (if log then Dream.logger else Fun.id)
  @@ Dream.router
       [ Dream.post "/conv" (fun request ->
             match%lwt limit_body_size ~max_size:max_input_size request with
             | Error `Too_big ->
                respond_error_text `Payload_Too_Large
                  ("max size: " ^ hum_size_of_bytes max_input_size)
             | Ok () ->
                match%lwt Dream.multipart ~csrf:false request with
                | `Ok ["file", [ fname, fcontents ] ] ->
                  let fname = Option.value fname ~default:"unnamed.txt" in
                  let ext = Filename.extension fname in
                  Dream.log "upload ext:%S size:%s" ext
                    (hum_size_of_bytes (String.length fcontents));
                  (match Ortografe.convert_string ~ext fcontents
                           ~options:{ convert_uppercase = false
                                    ; dict = Lazy.force Ortografe.erofa }
                   with
                   | exception e -> respond_error_text (`Status 422) (Printexc.to_string e)
                   | None -> respond_error_text (`Status 422) ("unsupported file type " ^ ext)
                   | Some (new_ext, new_body) ->
                      let new_fname = Filename.remove_extension fname ^ "-conv" ^ new_ext in
                      Dream.respond
                        ~headers:
                        (Dream.mime_lookup new_fname
                         @ [ "Content-Disposition",
                             Printf.sprintf "attachment; filename=\"%s\"" (Dream.to_percent_encoded new_fname) ])
                        new_body
                  )
             | _ -> respond_error_text `Bad_Request ""
           )
       ; Dream.get "/" (from_filesystem static_root "index.html")
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
  let return = C.Term.const in
  let map x f = C.Term.app (return f) x in
  let both a b = C.Term.app (C.Term.app (return (fun a b -> (a, b))) a) b in
  let (let+) = map
  and (and+) = both
  in
  let cmd =
    C.Cmd.group (C.Cmd.info "ortografe")
      [ C.Cmd.v (C.Cmd.info "serve")
          (let+ port = C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.int) None (C.Arg.info ["p"]))
           and+ tls = C.Arg.value (C.Arg.flag (C.Arg.info ["tls"]))
           and+ no_log = C.Arg.value (C.Arg.flag (C.Arg.info ["no-log"]))
           in
           run ?port ~log:(not no_log) ~tls ())
      ; C.Cmd.v (C.Cmd.info "serve-for-bench")
          (let+ port = C.Arg.value (C.Arg.opt (C.Arg.some C.Arg.int) None (C.Arg.info ["p"]))
           and+ tls = C.Arg.value (C.Arg.flag (C.Arg.info ["tls"]))
           and+ no_log = C.Arg.value (C.Arg.flag (C.Arg.info ["no-log"]))
           in
           run_for_bench ?port ~log:(not no_log) ~tls ())
      ; C.Cmd.v (C.Cmd.info "download-all")
          (let+ () = return () in
           Book_import.download_all ~root:(repo_root ()))
      ; C.Cmd.v (C.Cmd.info "convert-all")
          (let+ () = return () in
           Book_import.convert_all ~root:(Sys.getenv "INSIDE_DUNE"))
      ; C.Cmd.v (C.Cmd.info "rewrite-index")
          (let+ arg1 =
             C.Arg.required (C.Arg.pos 0 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"index.html" []))
            and+ arg2 =
             C.Arg.required (C.Arg.pos 1 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"books.html" []))
           in
           Rewrite_index.rewrite arg1 arg2)
      ]
  in
  exit (C.Cmd.eval cmd)

let () = main ()
