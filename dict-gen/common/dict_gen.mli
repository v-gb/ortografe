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
val all : rule list Lazy.t
val name : rule -> string
val doc : rule -> string
val html : id_prefix:string -> name_prefix:string -> rule -> string

type 'a json =
  [> `Assoc of (string * 'a json) list
  | `Bool of bool
  | `String of string ] as 'a

val gen
    :  ?profile:bool
    -> rules:rule list
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

val staged_gen
    : [< `Embedded of embedded | `Values of values ]
  -> (rule list
      -> metadata * (string -> string option))

val time : profile:bool -> string -> (unit -> 'a) -> 'a
