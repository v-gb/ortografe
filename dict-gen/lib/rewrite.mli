(** Functionality to respell words in the given lexique *)

(** Matches words that shouldn't change, according to the érofa rules,
    like proper nouns, or manual exceptions. *)
val erofa_preserve : string -> bool

val load_skip : unit -> (Data_src.Lexique.t -> bool)

type rule
val doc : rule -> string
val name : rule -> string
val all : rule list Lazy.t
val supports_repeated_rewrites : rule -> bool
val plurals_in_s : rule -> bool

type stats [@@deriving sexp_of]
val gen
    : root:[> Eio.Fs.dir_ty ] Eio.Path.t
    -> ?fix_oe:bool
    -> ?not_understood:[ `Raise | `Print | `Ignore ]
    -> ?lexique:Data_src.Lexique.t list
    -> ?rules: rule list
    -> (string -> string -> unit)
    -> stats
