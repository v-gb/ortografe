(** Converts text between spelling based on the given options *)

val convert : _ Common.convert

module Interleaved : sig
  (** This is a generalization of convert that allows one to provide interleaved text and
      structure (xml tags in practice), and tries to rewrite all words and put back all 
      the document around the converted words. *)
  type 'structure t
  val create : embed:(string -> 'structure) -> convert:(string -> string) -> 'structure t
  val emit_structure : 's t -> 's -> 's list
  val emit_text : 's t -> [< `Flush | `Space | `Text of string ] -> 's list
end
