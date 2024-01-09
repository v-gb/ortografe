let my_error_template _error debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in

  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response (Printf.sprintf "
    <html>
    <body>
      <h1>%d %s</h1>
      <pre>%s</pre>
    </body>
    </html>
  " code (Dream.html_escape reason) (Dream.html_escape debug_info));
  Lwt.return suggested_response

let run ?(log = true) ?port ?tls () =
  let static_root =
    if Sys.file_exists "/static" (* when in container *)
    then "/static"
    else "/tmp/static"
  in
  Dream.run ?port ?tls
    ~interface:"0.0.0.0" (* apparently only listens on lo otherwise *)
    ~error_handler:(Dream.error_template my_error_template)
  @@ (if log then Dream.logger else Fun.id)
  @@ Dream.router
       [ Dream.post "/conv" (fun request ->
             match%lwt Dream.multipart ~csrf:false request with
             | `Ok ["file", [ fname, fcontents ] ] ->
                let fname = Option.value fname ~default:"unnamed.txt" in
                let ext = Filename.extension fname in
                (* reads arbitrarily large stuff. There's no changing that without
                   submitting a pull request. The streaming interface still reads
                   everything in memory. So either way, we'll keep the current interface,
                   but at some point, we'll have a throttle. *)
                (match Ortografe.convert_string ~ext fcontents with
                 | None -> failwith ("unsupported file type " ^ ext)
                 | Some (new_ext, new_body) ->
                    let new_fname = Filename.remove_extension fname ^ "-conv" ^ new_ext in
                    Dream.respond
                      ~headers:
                      (Dream.mime_lookup new_fname
                       @ [ "Content-Disposition",
                           Printf.sprintf "attachment; filename=\"%s\"" (Dream.to_percent_encoded new_fname) ])
                      new_body
                )
             | _ -> Dream.empty `Bad_Request
           )
       ; Dream.get "/" (Dream.from_filesystem static_root "index.html")
       ; Dream.get "/static/**" (Dream.static static_root)
       ]

let run_for_bench ~log ?port ?tls () =
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
      [ C.Cmd.v (C.Cmd.info "conv")
          (let+ args = C.Arg.value (C.Arg.pos_all C.Arg.string [] (C.Arg.info []))
           in
           Ortografe.convert_files args)
      ; C.Cmd.v (C.Cmd.info "serve")
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
      ]
  in
  exit (C.Cmd.eval cmd)

let () = main ()
