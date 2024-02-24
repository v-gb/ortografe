let static () : Dict_gen_common.Dict_gen.static =
  { data_lexique_Lexique383_gen_tsv = Ortografe_embedded.data_lexique_Lexique383_gen_tsv
  ; extension_dict1990_gen_csv = Ortografe.extension_dict1990_gen_csv }  

let parse_dict str =
  let lines = Core.String.split_lines str in
  let lines, plurals_in_s =
    match lines with
    | first :: lines when String.starts_with first ~prefix:"{" ->
       let json = Yojson.Basic.from_string first in
       let metadata = Dict_gen_common.Dict_gen.metadata_of_json json in
       lines, metadata.plurals_in_s
    | _ -> lines, None
  in
  let h = Hashtbl.create (List.length lines) in
  List.iter (fun l ->
      match String.split_on_char ',' l with
      | [ a; b ] -> Hashtbl.replace h a b
      | _ -> failwith (Printf.sprintf
                         "didn't get exactly 2 items in line %S" l))
    lines;
  h, plurals_in_s

let () =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let cmd =
    C.Cmd.group
      (C.Cmd.info (Filename.basename Sys.executable_name))
      [ C.Cmd.v
          (C.Cmd.info
             ~doc:"conversion de documents vers l'orthographe Érofa (ou autre)"
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
           let dict, plurals_in_s =
             match rules, dict_file with
             | _ :: _, Some _ -> failwith "cannot specify both dictionary rules and --dict-file"
             | [], Some file -> parse_dict (Core.In_channel.read_all file)
             | [], None -> Lazy.force Ortografe.erofa, None
             | (_ :: _ as rules), None ->
                (match rules with
                 | [ rule ] when Dict_gen_common.Dict_gen.name rule = "erofa" ->
                    Lazy.force Ortografe.erofa, None
                 | [ rule ] when Dict_gen_common.Dict_gen.name rule = "1990" ->
                    Lazy.force Ortografe.rect1990, None
                 | _ ->
                    let b = Buffer.create 100 in
                    let `Stats _ =
                      Dict_gen_common.Dict_gen.gen
                        ~rules
                        ~all:false
                        ~output:(Buffer.add_string b)
                        ~json_to_string:(Yojson.to_string)
                        (`Static (static ()))
                    in
                    parse_dict (Buffer.contents b)
                )
           in
           Ortografe.convert_files
             ~options:{ convert_uppercase
                      ; dict = Stdlib.Hashtbl.find_opt dict
                      ; interleaved =
                          (match Sys.getenv "INTERLEAVED" with
                           | "false" -> false
                           | _ | exception Not_found -> true)
                      ; plurals_in_s = plurals_in_s ||? true
                      }
             arg1 arg2)
      ; Dict_gen.gen_cmd "dict"
          ~static:(static ())
          ~doc:"génération de dictionnaires de réécriture personnalisés pour la conversion \
                de document, ou pour l'extension de navigateur internet"
      ]
  in
  exit (C.Cmd.eval cmd)
