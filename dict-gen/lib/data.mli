open Core

module Lexique : sig
  type t =
    { ortho : string
    ; phon : string
    ; lemme : string
    ; h_aspire : bool
    }
  [@@deriving sexp_of]
  val parse : string -> t list

  val not_usable_words : unit -> string Hash_set.t
end

val parse_erofa : string -> (string, string) Hashtbl.t
val parse_post90 : string -> (string, string) Hashtbl.t
