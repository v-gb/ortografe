(** Functionality to respell words in the given lexique *)

(** Matches words that shouldn't change, according to the érofa rules,
    like proper nouns, or manual exceptions. *)
val erofa_preserve : string -> bool

val load_skip : unit -> (Data_src.Lexique.t -> bool)

val gen
    : root:[> Eio.Fs.dir_ty ] Eio.Path.t
    -> ?skip_not_understood:bool
    -> ?lexique:Data_src.Lexique.t list
    -> ?rules:[< `Erofa | `Qu | `Ti | `Il ] list
    -> (string -> string -> unit)
    -> unit
