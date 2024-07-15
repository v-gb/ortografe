(** Converts text between spelling based on the given options *)

val convert : ?buf:Buffer.t -> _ Common.convert

module Interleaved : sig
  type 'structure t
  (** This is a generalization of convert that allows one to provide interleaved text and
      structure (xml tags in practice), and tries to rewrite all words and put back all 
      the document around the converted words. *)

  val create :
    embed:(string -> 'structure) -> convert:(string -> string) -> 'structure t

  val emit_structure : 's t -> 's -> [< `Flush | `Space | `Not_special ] -> 's list
  val emit_text : 's t -> string -> 's list
end
