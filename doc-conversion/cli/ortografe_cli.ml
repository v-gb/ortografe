let embedded : Dict_gen_common.Dict_gen.embedded =
  { data_lexique_Lexique383_gen_tsv = Ortografe_embedded.data_lexique_Lexique383_gen_tsv
  ; extension_dict1990_gen_csv = Ortografe_embedded.extension_dict1990_gen_csv
  }

let parse_dict str =
  let lookup, metadata =
    Dict_gen_common.Dict_gen.parse str ~json_of_string:Yojson.Basic.from_string
  in
  fun () -> (lookup, metadata)

let no_metadata = Dict_gen_common.Dict_gen.no_metadata

let load_rules rules ~prebuild =
  match rules with
  | [] ->
      fun () ->
        (Stdlib.Hashtbl.find_opt (Lazy.force Ortografe_embedded.erofa), no_metadata)
  | [ rule ] when Dict_gen_common.Dict_gen.name rule = "1990" ->
      fun () ->
        (Stdlib.Hashtbl.find_opt (Lazy.force Ortografe_embedded.rect1990), no_metadata)
  | _ ->
      if not prebuild
      then
        let staged = Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded) in
        fun () -> staged rules
      else
        let b = Buffer.create 100 in
        let (`Stats _) =
          Dict_gen_common.Dict_gen.gen ~rules ~all:false ~output:(Buffer.add_string b)
            ~json_to_string:Yojson.to_string (`Embedded embedded)
        in
        parse_dict (Buffer.contents b)

let bench =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  C.Cmd.v (C.Cmd.info ~doc:"" "bench")
    (let+ arg1 =
       C.Arg.value
         (C.Arg.pos 0 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"INPUT_FILE" []))
     and+ rules = Dict_gen_nonbrowser.rules_cli ()
     and+ prebuild = C.Arg.value (C.Arg.flag (C.Arg.info ~doc:"" [ "prebuild" ])) in
     let staged = load_rules rules ~prebuild in
     for i = 1 to 5 do
       let t1 = Sys.time () in
       let dict, metadata = staged () in
       Ortografe.convert_files
         ~options:
           { convert_uppercase = true
           ; dict
           ; interleaved =
               (match Sys.getenv "INTERLEAVED" with
               | "false" -> false
               | _ | (exception Not_found) -> true)
           ; plurals_in_s = metadata.plurals_in_s ||? true
           }
         arg1 (Some "/dev/null");
       let t2 = Sys.time () in
       Printf.printf "%d: %f\n" i (t2 -. t1)
     done)

let ext_conv ?src_type src dst which = Ortografe.ext_conv ?src_type src dst which

let main more_cmd =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let arg_option arg info = C.Arg.opt (C.Arg.some arg) None info in
  let output_file =
    C.Arg.value
      (arg_option C.Arg.string
         (C.Arg.info ~docv:"OUTPUT_FILE" [ "o" ] ~doc:"le fichier de sortie"))
  in
  let output_file_maybe_in_place =
    let+ output = output_file
    and+ output_inplace =
      C.Arg.value
        (C.Arg.flag
           (C.Arg.info
              ~doc:
                "supprime le fichier d'entrée puis le réécrit en sortie. ATTENTION à ne \
                 pas perdre vos données"
              [ "in-place" ]))
    in
    if Option.is_some output
    then
      if output_inplace
      then failwith "can't specify both -o and --in-place"
      else `File output
    else if output_inplace
    then `In_place
    else `File output
  in
  let input_type =
    C.Arg.value
      (arg_option C.Arg.string
         (C.Arg.info ~docv:"TYPE" [ "type" ]
            ~doc:
              "comment traiter le fichier. --type txt veut dire traiter le fichier comme \
               un .txt même s'il n'est pas nommé .txt"))
  in
  let one_input_file =
    C.Arg.value
      (C.Arg.pos 0 (C.Arg.some C.Arg.string) None (C.Arg.info ~docv:"INPUT_FILE" []))
  in
  let cmd =
    C.Cmd.group
      (C.Cmd.info (Filename.basename Sys.executable_name))
      [ C.Cmd.v
          (C.Cmd.info ~doc:"extrait le texte d'un document"
             ~man:
               [ `S C.Manpage.s_description
               ; `P
                   "Extrait le texte du document INPUT_FILE (ou stdin si non\n\
                   \                        spécifié) sous forme d'une liste de chaines, \
                    et l'imprime dans\n\
                   \                        OUTPUT_FILE. Les chaines de OUTPUT_FILE \
                    peuvent ensuite être modifiées\n\
                   \                        de façon quelconque (mais en préservant leur \
                    nombres), puis être réinsérées\n\
                   \                        dans INPUT_FILE à l'aide de $(iname) insert.\n\n\
                   \                        Cela permet de réécrire le texte d'un \
                    document de façon arbitraire."
               ; `P "Exemple :"
               ; `Pre "$(iname) foo.docx \\ "
               ; `Noblank
               ; `Pre "  | sed \"s/aujourd'hui/hui/g\" \\ "
               ; `Noblank
               ; `Pre "  | $(mname) insert foo.doc -o foo2.docx"
               ; `Noblank
               ; `P
                   "(copie foo.docx en foo2.docx en changeant « aujourd'hui » en « hui »)"
               ]
             "extract")
          (let+ input = one_input_file
           and+ input_type = input_type
           and+ output = output_file in
           ext_conv ?src_type:input_type input output `Extract)
      ; C.Cmd.v
          (C.Cmd.info ~doc:"insère du texte dans un document"
             ~man:
               [ `S C.Manpage.s_description
               ; `P
                   "Remplace le texte du document INPUT_FILE (ou stdin si non\n\
                   \                        spécifié) par le texte de TEXT_INPUT_FILE \
                    (ou stdin si non\n\
                   \                        spécifié) et écrit le résultat dans \
                    OUTPUT_FILE.\n\
                   \                        Voir l'aide de $(mname) extract pour un \
                    exemple d'utilisation."
               ]
             "insert")
          (let+ input = one_input_file
           and+ input_type = input_type
           and+ output = output_file_maybe_in_place
           and+ text_input =
             C.Arg.value
               (arg_option C.Arg.string
                  (C.Arg.info ~docv:"TEXT_INPUT_FILE" [ "i" ] ~doc:"le fichier de texte"))
           in
           if Option.is_none input && Option.is_none text_input
           then
             failwith
               "must specify either INPUT_FILE or TEXT_INPUT_FILE, otherwise they'd both \
                be read from stdin";
           ext_conv ?src_type:input_type input
             (match output with
             | `File opt -> opt
             | `In_place ->
                 Some (input ||? failwith "--in-place only makes sense with an INPUT_FILE"))
             (`Insert text_input))
      ; C.Cmd.v
          (C.Cmd.info ~doc:"transcrit des documents vers l'orthographe Érofa (ou autre)"
             ~man:
               [ `S C.Manpage.s_description
               ; `P
                   "Transcrit le texte du document INPUT_FILE (ou stdin si non\n\
                   \                        spécifié) en OUTPUT_FILE (ou un nom dérivé \
                    de INPUT_FILE si non\n\
                   \                        spécifié, ou stdout). La transcription \
                    utilise l'orthographe Érofa\n\
                   \                        à moins qu'une autre orthographe ne soit \
                    sélectionnée."
               ; `P "Exemples :"
               ; `Pre "$(iname) foo.docx"
               ; `Noblank
               ; `P "(produit foo-conv.docx en orthographe Érofa)"
               ; `Pre "$(iname) foo.docx -o foo-erofa.docx"
               ; `Pre "$(iname) < ~/fichier.txt"
               ; `Pre "$(iname) foo.docx -o foo-modif.docx --1990 --qu/qou --qu/q"
               ; `Noblank
               ; `P "(transcrit avec les règles spécifiées au lieu des règles Érofa)"
               ]
             "conv")
          (let+ args =
             C.Arg.value
               (C.Arg.pos_all C.Arg.string [] (C.Arg.info ~docv:"INPUT_FILE" []))
           and+ input_type = input_type
           and+ output = output_file_maybe_in_place
           and+ convert_uppercase =
             let+ b =
               (* We don't do this in the extension, but some books seems to regularly
                  have uppercase text hardcoded rather than as styling, and so we miss
                  bits of text.  It seems less annoying if we convert these. *)
               C.Arg.value
                 (C.Arg.flag
                    (C.Arg.info ~doc:"ne pas réécrire les mots tout en majuscule"
                       [ "no-convert-uppercase" ]))
             in
             not b
           and+ rules = Dict_gen_nonbrowser.rules_cli ()
           and+ dict_file =
             C.Arg.value
               (C.Arg.opt (C.Arg.some C.Arg.file) None
                  (C.Arg.info ~docv:"FILENAME" [ "dict-file" ]
                     ~doc:"a filename, for instance as output by $(mname) dict"))
           in
           let dict, metadata =
             (* Maybe we should provide a --builtin-erofa flag or something, to be able to
                combine the upstream erofa dict with --dict-file. *)
             let from_rules =
               match rules with
               | [] -> None
               | _ :: _ -> Some (load_rules rules ~prebuild:false ())
             in
             let from_dict_file =
               Option.map
                 (fun file -> parse_dict (Core.In_channel.read_all file) ())
                 dict_file
             in
             match (from_rules, from_dict_file) with
             | None, None ->
                 ( Stdlib.Hashtbl.find_opt (Lazy.force Ortografe_embedded.erofa)
                 , no_metadata )
             | Some v, None | None, Some v -> v
             | Some p_rules, Some p_dict ->
                 Dict_gen_common.Dict_gen.merge_right_biased p_rules p_dict
           in
           let convert ~in_ ~out =
             Ortografe.convert_files
               ~options:
                 { convert_uppercase
                 ; dict
                 ; interleaved =
                     (match Sys.getenv "INTERLEAVED" with
                     | "false" -> false
                     | _ | (exception Not_found) -> true)
                 ; plurals_in_s = metadata.plurals_in_s ||? true
                 }
               ?src_type:input_type in_ out
           in
           match args with
           | [] ->
               convert ~in_:None
                 ~out:
                   (match output with
                   | `In_place -> failwith "--in-place only makes sense with INPUT_FILEs"
                   | `File opt -> opt)
           | _ :: _ ->
               if List.length args > 1
                  && match output with `File (Some _) -> true | _ -> false
               then failwith "can't specify -o with multiple input files";
               List.iter
                 (fun arg ->
                   convert ~in_:(Some arg)
                     ~out:(match output with `File opt -> opt | `In_place -> Some arg))
                 args)
      ; bench
      ; +more_cmd
      ]
  in
  exit (C.Cmd.eval cmd)
