let () =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let cmd =
    C.Cmd.group
      (C.Cmd.info (Filename.basename Sys.executable_name))
      [ C.Cmd.v
          (C.Cmd.info
             ~doc:"conversion de documents vers l'orthographe Érofa"
             "conv")
          (let+ arg1 =
             C.Arg.value (C.Arg.pos 0 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"INPUT_FILE" []))
           and+ arg2 =
             C.Arg.value (C.Arg.pos 1 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"OUTPUT_FILE" []))
           and+ convert_uppercase = C.Arg.value (C.Arg.flag (C.Arg.info ["convert-uppercase"]))
           and+ dict =
             let enum = [ "erofa", `Erofa; "1990", `Rect1990; "empty", `Empty ] in
             C.Arg.value
               (C.Arg.opt
                  (C.Arg.some (C.Arg.enum enum))
                  None
                  (C.Arg.info ["dict"] ~doc:(C.Arg.doc_alts_enum enum)))
           and+ dict_file =
             C.Arg.value
               (C.Arg.opt
                  (C.Arg.some C.Arg.file)
                  None
                  (C.Arg.info ["dict-file"]
                     ~doc:"a filename, for instance as output by $(mname) dict"))
           in
           let dict =
             match dict, dict_file with
             | None, None -> Lazy.force Ortografe.erofa
             | Some _, Some _ -> failwith "cannot specify both --dict and --dict-file"
             | Some `Erofa, None -> Lazy.force Ortografe.erofa
             | Some `Rect1990, None -> Lazy.force Ortografe.rect1990
             | Some `Empty, None -> Hashtbl.create 1
             | None, Some file ->
                let h = Hashtbl.create 10000 in
                Core.String.split_lines
                  (Core.In_channel.read_all file)
                |> List.iter (fun l ->
                       match String.split_on_char ',' l with
                       | [ a; b ] -> Hashtbl.replace h a b
                       | _ -> failwith (Printf.sprintf
                                          "didn't get exactly 2 items in line %S" l));
                h
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
      ; Dict_gen.gen_cmd "dict"
          ~doc:"génération de dictionnaires de réécriture personnalisés pour la conversion \
                de document, ou pour l'extension de navigateur internet"
      ]
  in
  exit (C.Cmd.eval cmd)
