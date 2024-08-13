let main () =
  let module C = Cmdliner in
  let open Cmdliner.Term.Syntax in
  let cmd =
    C.Cmd.group (C.Cmd.info "build")
      [ C.Cmd.v (C.Cmd.info "download-all")
          (let+ () = C.Term.const () in
           Book_import.download_all ~root:(Core.Sys.getenv_exn "DUNEROOT"))
      ; C.Cmd.v (C.Cmd.info "convert-all")
          (let+ books_tar =
             C.Arg.required
               (C.Arg.pos 0 (C.Arg.some C.Arg.string) None
                  (C.Arg.info ~docv:"path to books.tar" []))
           in
           Book_import.convert_all ~books_tar)
      ]
  in
  exit (C.Cmd.eval cmd)

let () = main ()
