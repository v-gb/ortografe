open Core

type 'a src =
  [ `Str of string | `Root of ([> Eio.Fs.dir_ty ] as 'a) Eio.Path.t ]

module Lexique : sig
  type t =
    { ortho : string
    ; phon : string
    ; lemme : string
    ; h_aspire : bool
    }
  [@@deriving sexp_of]
  val load : _ src -> t list

  val not_usable_words : unit -> string Hash_set.t
end

val load_erofa : _ src -> (string, string) Hashtbl.t
val load_post90 : _ src -> (string, string) Hashtbl.t
