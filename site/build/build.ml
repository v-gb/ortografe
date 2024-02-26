let main () =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let cmd =
    C.Cmd.group (C.Cmd.info "build")
      [ C.Cmd.v (C.Cmd.info "download-all")
          (let+ () = return () in
           Book_import.download_all ~root:(Sys.getenv "DUNEROOT"))
      ; C.Cmd.v (C.Cmd.info "convert-all")
          (let+ () = return () in
           Book_import.convert_all ~root:(Sys.getenv "DUNEROOT"))
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
