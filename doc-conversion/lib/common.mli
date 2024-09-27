module Markup := Markup_t
(** A few misc utility functions and types *)

module Substring : sig
  type t =
    { string : string
    ; start : int
    ; len : int
    }

  val of_string : string -> t
end

module Buf : sig
  (** Reimplement a fragment of Buffer.t, so we can expose substring__consume.

      We want this function when we transform a docx or other file: we start from a zip
      file, decompress some xml from it, process it, write the result in a buffer, and
      compress it. Buffer.t forces to have 3 copies of the xml in memory: after
      decompression, in the buffer, and the Buffer.contents. This buffer allows us to
      have only two.

      Given that the processing is done in a streaming fashion, it may be possible to
      extend the streaming to the compression and decompression step, so we never have
      the full data in memory. But zipc doesn't expose any stream api either for
      compression or decompression, so we do the easier thing.
   *)

  type t

  val create : int -> t
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val substring__consume : t -> Substring.t
end

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out
  | Substring : int -> Substring.t out
  | Ignore : unit out

val substring_out : string -> Substring.t out
val write_out : 'a out -> string -> 'a
val markup_output : 'a out -> (char, Markup.sync) Markup.stream -> 'a
val buffer : ?n:int -> Buffer.t option -> Buffer.t

type options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : string option
  }

type 'a convert =
  ?progress:(int -> unit) -> options:options -> string -> dst:'a out -> 'a
