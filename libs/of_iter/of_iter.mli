(** A few functions to work on iterators of 'a with type [('a -> unit) -> unit] like
   (fun f -> List.iter f [ ... ]). *)

val list : (('a -> unit) -> unit) -> 'a list
