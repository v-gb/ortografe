open Core

module Lexique : sig
  type t =
    { ortho : string
    ; phon : string
    ; cgram : string
    ; lemme : string
    }
  [@@deriving sexp_of]
  val load : root:[> Eio.Fs.dir_ty ] Eio.Path.t -> ?filename:string -> unit -> t list

  val not_usable_words : unit -> string Hash_set.t
end

module Wiki : sig
  type t =
    { word : string
    ; phon : string
    ; h_aspire : bool
    }
      [@@deriving sexp_of]

  val load : root:[> Eio.Fs.dir_ty ] Eio.Path.t -> t list
end

module Wiki_h : sig
  type t =
    { word : string
    ; h_aspire : bool
    }
      [@@deriving sexp_of]

  val load : root:[> Eio.Fs.dir_ty ] Eio.Path.t -> t list
end
