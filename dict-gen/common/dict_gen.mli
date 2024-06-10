open Base

val build_lexique_post90
    : Data.Lexique.t
   -> (string, string) Hashtbl.t
   -> rect1990:bool
   -> Data.Lexique.t

val build_erofa_ext
    : erofa:(string, string) Hashtbl.t
   -> post90:(string, string) Hashtbl.t
   -> lexique:Data.Lexique.t
   -> (string * string) list

type embedded =
  { data_lexique_Lexique383_gen_tsv : string
  ; extension_dict1990_gen_csv : string
  }
type values =
  { post90 : (string, string) Hashtbl.t
  ; lexique : Data.Lexique.t
  }

type rule
type rules = rule list

val all_builtin : rule list Lazy.t
val name : rule -> string
val of_name_builtin : string -> rule option
val html_doc : rule -> string
val doc : rule -> string
val problems : rule -> string list
val all_selection_html
    : url_prefix:string
    -> id_prefix:string
    -> name_prefix:string
    -> ?checked:(rule -> bool)
    -> unit
    -> string
val custom_rule : string -> rule option

type 'a json =
  [> `Assoc of (string * 'a json) list
  | `Bool of bool
  | `String of string ] as 'a

val gen
    :  ?profile:bool
    -> ?progress:(int -> unit)
    -> rules:rules
    -> all:bool
    -> output:(string -> unit)
    -> json_to_string: (_ json -> string)
    -> [ `Embedded of embedded | `Values of values ]
    -> [ `Stats of Sexp.t ]

type metadata =
  { desc : string option
  ; lang : string option
  ; supports_repeated_rewrites : bool option
  ; plurals_in_s : bool option
  }
val metadata_of_json : _ json -> metadata
val no_metadata : metadata

val parse : string -> json_of_string:(string -> _ json) -> (string -> string option) * metadata

val staged_gen
    : [< `Embedded of embedded | `Values of values ]
  -> (rules
      -> (string -> string option) * metadata)

val time : profile:bool -> string -> (unit -> 'a) -> 'a

val merge_right_biased
   : (string -> string option) * metadata
  -> (string -> string option) * metadata
  -> (string -> string option) * metadata

val merge_right_biased_opt
   : (string -> string option) * metadata
  -> ((string -> string option) * metadata) option
  -> (string -> string option) * metadata
