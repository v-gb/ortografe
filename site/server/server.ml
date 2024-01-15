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
    (* maybe we should read the COPY line from the docker file, to
       avoid duplicating? *)
    let static_root = "/tmp/static" in
    ListLabels.iter
      [ "site/client/index.html", None
      ; "site/client/page.js", None
      ; "extension/src/rewrite.js", None
      ; "extension/src/dict.js", None
      ; "_build/default/doc-conversion/bin/ortografe_cli.exe", None
      ; Book_import.directory ~root:repo_root, Some "books"
      ] ~f:(fun (f, dst) ->
        let src = if Filename.is_relative f then Filename.concat repo_root f else f in
        let dst = Filename.concat static_root (Option.value dst ~default:(Filename.basename f)) in
        if Sys.file_exists dst then Unix.unlink dst;
        Unix.symlink src dst
      );
    static_root

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

let run ?(log = true) ?port ?tls ?(max_input_size = 100_000_000) () =
  Ortografe.max_size := max_input_size * 2; (* xml can be quite large when decompressed *)
  let static_root = where_to_find_static_files () in
  Dream.run ?port ?tls
    ~interface:"0.0.0.0" (* apparently only listens on lo otherwise *)
    ~error_handler:(Dream.error_template my_error_template)
  @@ (if log then Dream.logger else Fun.id)
  @@ Dream.router
       [ Dream.post "/conv" (fun request ->
             (* reads arbitrarily large stuff. There's no changing that without submitting
                a pull request. The streaming interface still reads everything in
                memory. So either way, we'll keep the current interface, but at some
                point, we'll have a throttle. *)
             match%lwt Dream.multipart ~csrf:false request with
             | `Ok ["file", [ fname, fcontents ] ] ->
                if String.length fcontents > max_input_size
                then respond_error_text `Payload_Too_Large
                       ("max size: " ^ Int.to_string max_input_size)
                else
                  let fname = Option.value fname ~default:"unnamed.txt" in
                  let ext = Filename.extension fname in
                  Dream.log "upload ext:%S size:%s" ext
                    (Core.Byte_units.Short.to_string
                       (Core.Byte_units.of_bytes_int (String.length fcontents)));
                  (match Ortografe.convert_string ~convert_uppercase:false ~ext fcontents with
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
      ; C.Cmd.v (C.Cmd.info "download-all")
          (let+ () = return () in
           Book_import.download_all ~root:(repo_root ()))
      ; C.Cmd.v (C.Cmd.info "convert-all")
          (let+ () = return () in
           Book_import.convert_all ~root:(repo_root ()))
      ; C.Cmd.v (C.Cmd.info "books-html")
          (let+ () = return () in
           print_string (Book_import.html ~root:(repo_root ())))
      ]
  in
  exit (C.Cmd.eval cmd)

let () = main ()
