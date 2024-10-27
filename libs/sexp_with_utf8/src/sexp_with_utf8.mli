(** This module provides alternatives to Sexp.to_string_him that leaves some unicode in
    atoms unescaped, for readability. *)

val linkme : unit

val to_string_hum :
  ?which:[ `Alphabetic | `All ] -> ?indent:int -> Sexplib.Sexp.t -> string

val exn_to_string : exn -> string
