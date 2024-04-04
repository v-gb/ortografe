let embedded : Dict_gen_common.Dict_gen.embedded =
  { data_lexique_Lexique383_gen_tsv = Ortografe_embedded.data_lexique_Lexique383_gen_tsv
  ; extension_dict1990_gen_csv = Ortografe.extension_dict1990_gen_csv }  

let parse_dict str =
  let lookup, metadata =
    Dict_gen_common.Dict_gen.parse str
      ~json_of_string:Yojson.Basic.from_string
  in
  fun () -> lookup, metadata

let no_metadata = Dict_gen_common.Dict_gen.no_metadata
let load_rules rules ~prebuild =
  match rules with
  | [] -> (fun () -> Stdlib.Hashtbl.find_opt (Lazy.force Ortografe.erofa), no_metadata)
  | [ rule ] when Dict_gen_common.Dict_gen.name rule = "1990" ->
     (fun () -> Stdlib.Hashtbl.find_opt (Lazy.force Ortografe.rect1990), no_metadata)
  | _ ->
     if not prebuild
     then
       let staged = Dict_gen_common.Dict_gen.staged_gen (`Embedded embedded) in
       fun () -> staged rules
     else (
       let b = Buffer.create 100 in
       let `Stats _ =
         Dict_gen_common.Dict_gen.gen
           ~rules
           ~all:false
           ~output:(Buffer.add_string b)
           ~json_to_string:(Yojson.to_string)
           (`Embedded embedded)
       in
       parse_dict (Buffer.contents b)
     )  

let bench =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  C.Cmd.v
    (C.Cmd.info ~doc:"" "bench")
    (let+ arg1 =
       C.Arg.value (C.Arg.pos 0 (C.Arg.some C.Arg.string) None
                      (C.Arg.info ~docv:"INPUT_FILE" []))
     and+ rules = Dict_gen.rules_cli ()
     and+ prebuild = C.Arg.value (C.Arg.flag (C.Arg.info ~doc:"" ["prebuild"]))
     in
     let staged = load_rules rules ~prebuild in
     for i = 1 to 5 do
       let t1 = Sys.time () in
       let dict, metadata = staged () in
       Ortografe.convert_files
         ~options:{ convert_uppercase = true
                  ; dict
                  ; interleaved =
                      (match Sys.getenv "INTERLEAVED" with
                       | "false" -> false
                       | _ | exception Not_found -> true)
                  ; plurals_in_s = metadata.plurals_in_s ||? true
         }
         arg1 (Some "/dev/null");
       let t2 = Sys.time () in
       Printf.printf "%d: %f\n" i (t2 -. t1)
     done
    )  

let () =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let cmd =
    C.Cmd.group
      (C.Cmd.info (Filename.basename Sys.executable_name))
      [ C.Cmd.v
          (C.Cmd.info
             ~doc:"transcrit des documents vers l'orthographe Érofa (ou autre)"
             ~man:[ `S C.Manpage.s_description
                  ; `P "Transcrit le texte du document INPUT_FILE (ou stdin si non
                        spécifié) en OUTPUT_FILE (ou un nom dérivé de INPUT_FILE si non
                        spécifié, ou stdout). La transcription utilise l'orthographe à moins
                        qu'une autre orthographe ne soit sélectionnée."
                  ; `P "Exemples :"
                  ; `Pre "$(iname) foo.docx"
                  ; `Noblank; `P "(produit foo-conv.docx en orthographe Érofa)"
                  ; `Pre "$(iname) foo.docx foo-erofa.docx"
                  ; `Pre "$(iname) < ~/fichier.txt"
                  ; `Pre "$(iname) foo.docx foo-modif.docx --1990 --qu/qou --qu/q"
                  ; `Noblank; `P "(transcrit avec les règles spécifiées au lieu des règles Érofa)"
                  ]
             "conv")
          (let+ arg1 =
             C.Arg.value (C.Arg.pos 0 (C.Arg.some C.Arg.string) None
                            (C.Arg.info ~docv:"INPUT_FILE" []))
           and+ arg2 =
             C.Arg.value (C.Arg.pos 1 (C.Arg.some C.Arg.string) None
                            (C.Arg.info ~docv:"OUTPUT_FILE" []))
           and+ convert_uppercase =
             let+ b =
               (* We don't do this in the extension, but some books seems to regularly
                  have uppercase text hardcoded rather than as styling, and so we miss
                  bits of text.  It seems less annoying if we convert these. *)
               C.Arg.value
                 (C.Arg.flag
                    (C.Arg.info
                       ~doc:"ne pas réécrire les mots tout en majuscule" ["no-convert-uppercase"]))
             in
             not b
           and+ rules = Dict_gen.rules_cli ()
           and+ dict_file =
             C.Arg.value
               (C.Arg.opt
                  (C.Arg.some C.Arg.file)
                  None
                  (C.Arg.info ~docv:"FILENAME" ["dict-file"]
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
               Option.map (fun file -> parse_dict (Core.In_channel.read_all file) ())
                 dict_file
             in
             match from_rules, from_dict_file with
             | None, None -> Stdlib.Hashtbl.find_opt (Lazy.force Ortografe.erofa), no_metadata
             | Some v, None | None, Some v -> v
             | Some (f_rules, m_rules), Some (f_dict, _m_dict) ->
                (fun word ->
                  match f_dict word with
                  | Some _ as opt -> opt
                  | None -> f_rules word), m_rules
           in
           Ortografe.convert_files
             ~options:{ convert_uppercase
                      ; dict
                      ; interleaved =
                          (match Sys.getenv "INTERLEAVED" with
                           | "false" -> false
                           | _ | exception Not_found -> true)
                      ; plurals_in_s = metadata.plurals_in_s ||? true
                      }
             arg1 arg2)
      ; bench
      ; Dict_gen.gen_cmd "dict"
          ~embedded
          ~doc:"génération de dictionnaires de réécriture personnalisés pour la conversion \
                de document, ou pour l'extension de navigateur internet"
      ]
  in
  exit (C.Cmd.eval cmd)
