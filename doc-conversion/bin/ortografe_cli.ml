let () =
  Ortografe_cli.main
    [ Dict_gen.gen_cmd "dict"
        ~embedded:Ortografe_cli.embedded
        ~doc:"génération de dictionnaires de réécriture personnalisés pour la conversion \
              de document, ou pour l'extension de navigateur internet"
    ]
