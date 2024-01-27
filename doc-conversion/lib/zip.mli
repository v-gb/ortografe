(** High-level wrapper around zipc, that's both easier to use, and caps memory usage, to
   limit the risk of DOS. *)

val max_size : int ref
val map
    : string
   -> (Zipc.Member.t -> (unit -> string) -> string option)
   -> string
