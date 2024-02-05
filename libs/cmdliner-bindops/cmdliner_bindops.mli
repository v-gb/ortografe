(** Maybe one day https://github.com/dbuenzli/cmdliner/issues/173 will get fixed. Until then. *)

val return : 'a -> 'a Cmdliner.Term.t
val (let+) : 'a Cmdliner.Term.t -> ('a -> 'b) -> 'b Cmdliner.Term.t
val (and+) : 'a Cmdliner.Term.t -> 'b Cmdliner.Term.t -> ('a * 'b) Cmdliner.Term.t
