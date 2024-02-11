open Core

module Lexique : sig
  type t =
    { ortho : string
    ; phon : string
    ; cgram : string
    ; lemme : string
    ; h_aspire : bool
    }
  [@@deriving sexp_of]
  val load : root:[> Eio.Fs.dir_ty ] Eio.Path.t -> ?filename:string -> unit -> t list

  val not_usable_words : unit -> string Hash_set.t
end

val load_erofa : root:[> Eio.Fs.dir_ty ] Eio.Path.t -> (string, string) Hashtbl.t
val load_post90 : root:[> Eio.Fs.dir_ty ] Eio.Path.t -> (string, string) Hashtbl.t
