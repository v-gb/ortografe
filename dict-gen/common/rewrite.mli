(** Functionality to respell words in the given lexique *)

(** Matches words that shouldn't change, according to the érofa rules,
    like proper nouns, or manual exceptions. *)
val erofa_preserve : string -> bool

val load_skip : unit -> (Data.Lexique.row -> bool)

type rule
type rules = rule list
val doc : rule -> string
val name : rule -> string
val erofa : rule
val all : rule list Lazy.t
val supports_repeated_rewrites : rule -> bool
val plurals_in_s : rule -> bool

type stats [@@deriving sexp_of]
val gen
    :  ?fix_oe:bool
    -> ?not_understood:[ `Raise | `Call of (Sexplib.Sexp.t -> unit) | `Ignore ]
    -> rules: rules
    -> Data.Lexique.t
    -> (string -> string -> unit)
    -> stats

val staged_gen : ?fix_oe:bool -> rules: rules -> unit -> (Data.Lexique.row -> string)
