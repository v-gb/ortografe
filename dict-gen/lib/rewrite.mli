(** Functionality to respell words in the given lexique *)

module Data := Dict_gen_common.Data

(** Matches words that shouldn't change, according to the érofa rules,
    like proper nouns, or manual exceptions. *)
val erofa_preserve : string -> bool

val load_skip : unit -> (Data.Lexique.t -> bool)

type rule
val doc : rule -> string
val name : rule -> string
val all : rule list Lazy.t
val supports_repeated_rewrites : rule -> bool
val plurals_in_s : rule -> bool

type stats [@@deriving sexp_of]
val gen
    :  ?fix_oe:bool
    -> ?not_understood:[ `Raise | `Print | `Ignore ]
    -> ?rules: rule list
    -> Data.Lexique.t list
    -> (string -> string -> unit)
    -> stats
