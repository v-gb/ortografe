type static =
  { data_lexique_Lexique383_gen_tsv : string
  ; extension_dict1990_gen_csv : string }

val gen_cmd : ?static:static -> ?doc:string -> string -> unit Cmdliner.Cmd.t
val main : unit -> unit
