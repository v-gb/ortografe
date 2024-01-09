let () =
  let module C = Cmdliner in
  let return = C.Term.const in
  let map x f = C.Term.app (return f) x in
  let both a b = C.Term.app (C.Term.app (return (fun a b -> (a, b))) a) b in
  let (let+) = map
  and (and+) = both
  in
  let cmd =
    C.Cmd.v
      (C.Cmd.info
         ~doc:"conversion de documents vers l'orthographe Érofa"
         (Filename.basename Sys.executable_name))
      (let+ arg1 =
         C.Arg.value (C.Arg.pos 0 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"INPUT_FILE" []))
       and+ arg2 =
         C.Arg.value (C.Arg.pos 1 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"OUTPUT_FILE" []))
       in
       Ortografe.convert_files arg1 arg2)
  in
  exit (C.Cmd.eval cmd)
