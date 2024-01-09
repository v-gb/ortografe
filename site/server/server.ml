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

let where_to_find_static_files () =
  if Sys.file_exists "/static" (* when in container *)
  then "/static"
  else
    (* In dev, setup symlinks to the files in the repo, so we can just
       modify the files and reload without fiddling with the server. *)
    let repo_root =
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
    in
    (* maybe we should read the COPY line from the docker file, to
       avoid duplicating? *)
    let static_root = "/tmp/static" in
    ListLabels.iter
      [ "site/client/index.html"
      ; "site/client/page.js"
      ; "extension/src/rewrite.js"
      ; "extension/src/dict.js"
      ; "_build/default/doc-conversion/bin/ortografe_cli.exe"
      ] ~f:(fun f ->
        let src = Filename.concat repo_root f in
        let dst = Filename.concat static_root (Filename.basename f) in
        if Sys.file_exists dst then Unix.unlink dst;
        Unix.symlink src dst
      );
    static_root

let run ?(log = true) ?port ?tls () =
  let static_root = where_to_find_static_files () in
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
      ]
  in
  exit (C.Cmd.eval cmd)

let () = main ()
