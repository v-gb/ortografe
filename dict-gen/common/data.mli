open Base

module Lexique : sig
  type row =
    { ortho : string
    ; phon : string
    ; lemme : string
    ; h_aspire : bool
    }
  [@@deriving sexp_of]

  type t = row list
  val parse : string -> t

  val not_usable_words : unit -> string Hash_set.t
end

val parse_erofa : string -> (string, string) Hashtbl.t
val parse_post90 : string -> (string, string) Hashtbl.t
