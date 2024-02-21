open Base

val build_lexique_post90
    : Data.Lexique.t list
   -> (string, string) Base.Hashtbl.t
   -> fix_90:bool
   -> Data.Lexique.t list

val build_erofa_ext
    : erofa:(string, string) Hashtbl.t
   -> post90:(string, string) Hashtbl.t
   -> lexique:Data.Lexique.t list
   -> (string * string) list

type static =
  { data_lexique_Lexique383_gen_tsv : string
  ; extension_dict1990_gen_csv : string
  }

val gen
    :  rules:Rewrite.rule list
    -> rect90:bool
    -> all:bool
    -> oe:bool
    -> output:(string -> unit)
    -> json_to_string:
         ([> `Assoc of
               (string *
                  [> `Bool of bool | `String of string ])
                 list ] ->
          string)
    -> [ `Static of static
       | `Values of [ `Post90 of (string, string) Hashtbl.t ]
                    * [ `Lexique of Data.Lexique.t list ] ]
    -> [ `Stats of Sexplib.Sexp.t ]