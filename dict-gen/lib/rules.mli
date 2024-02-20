(** This module allows one to break down the pronunciation of French words into
    a list of (graphem, phonem), and provides an indication of how surprising the
    pronunciation is. *)

type t
val create : unit -> t

type importance =
  | Core_optional
  | Core
  | Surprising

type path_elt =
  { graphem : string
  ; phonem : string
  ; i : int
  ; j : int
  ; this_surprise : int
  ; importance : importance
  }
type search_res = path_elt list * int

val search : t -> string -> string -> (search_res, Sexplib.Sexp.t) Result.t
val check : Data.Lexique.t list -> skip:(Data.Lexique.t -> bool) -> unit

(** A few utility functions *)

(** Given the phonems that follow the phonem for an «e» letter, compute whether e's syllable
    is closed or open, that is, which of é or è you should expect is to be pronounced
    as (although it would probably work the same for other pairs of open/closed vowels). *)
val accent_aigu : string -> bool

val (#:) : string -> (int * int) -> string
val (#::) : string -> (int * int) -> Uchar.t
val in_ortho_weak_vowels : Uchar.t -> bool
val in_ortho_vowels : Uchar.t -> bool
val str_of_uchar : Uchar.t -> string
