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
       and+ convert_uppercase = C.Arg.value (C.Arg.flag (C.Arg.info ["convert-uppercase"]))
       and+ base_dict =
         C.Arg.value
           (C.Arg.opt
              (C.Arg.enum [ "erofa", `Erofa; "1990", `Rect1990; "empty", `Empty ])
              `Erofa
              (C.Arg.info ["base-dict"]))
       in
       let dict =
         match base_dict with
         | `Erofa -> Lazy.force Ortografe.erofa
         | `Rect1990 -> Lazy.force Ortografe.rect1990
         | `Empty -> Hashtbl.create 1
       in
       Ortografe.convert_files
         ~options:{ convert_uppercase
                  ; dict
                  ; interleaved =
                      (match Sys.getenv "INTERLEAVED" with
                       | "false" -> false
                       | _ | exception Not_found -> true)
                  }
         arg1 arg2)
  in
  exit (C.Cmd.eval cmd)
