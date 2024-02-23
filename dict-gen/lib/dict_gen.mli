val rules_cli : unit -> Dict_gen_common.Dict_gen.rule list Cmdliner.Term.t
val gen_cmd : ?static:Dict_gen_common.Dict_gen.static -> ?doc:string -> string -> unit Cmdliner.Cmd.t
val main : unit -> unit
