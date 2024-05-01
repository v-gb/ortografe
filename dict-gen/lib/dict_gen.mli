val rules_cli : unit -> Dict_gen_common.Dict_gen.rules Cmdliner.Term.t
val gen_cmd : ?embedded:Dict_gen_common.Dict_gen.embedded -> ?doc:string -> string -> unit Cmdliner.Cmd.t
val main : unit -> unit
