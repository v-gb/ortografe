open Core
module Dict_gen = Dict_gen_common.Dict_gen

let rules_cli () =
  let module C = Cmdliner in
  let open Cmdliner_bindops in
  let docs = "Sélection du dictionnaire de réécriture" in
  let+ rules =
    List.fold_right ~init:(return []) (force Dict_gen.all_builtin) ~f:(fun rule acc ->
        let+ present =
          C.Arg.value
            (C.Arg.flag
               (C.Arg.info ~docs ~doc:(Dict_gen.doc rule) [ Dict_gen.name rule ]))
        and+ acc = acc in
        if present then rule :: acc else acc)
  and+ custom_rule =
    let+ opt =
      C.Arg.value
        (C.Arg.opt (C.Arg.some C.Arg.string) None
           (C.Arg.info ~docs [ "custom" ]
              ~doc:"une règle de réécriture perso, comme 'eaux/ôs eau/ô'"))
    in
    Dict_gen.custom_rule (opt ||? "")
  in
  Option.to_list custom_rule @ rules
