(** This module provides alternatives to Sexp.to_string_him that leaves
    some unicode in atoms unescaped, for readability. *)

val linkme : unit
val to_string_hum : ?indent:int -> Sexplib.Sexp.t -> string
