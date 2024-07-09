(** High-level wrapper around zipc, that's both easier to use, and caps memory usage, to
   limit the risk of DOS. *)

val max_size : int ref

val map :
     ?progress:(int -> unit)
  -> string
  -> (path:string -> (contents:string -> string) option)
  -> string
