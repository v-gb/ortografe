(** The parser of Markup is not usable in javascript (it's written in CPS, and so blows
   up the stack). We provide a parser with the interface we need based on
   browser-provided functionality. *)

val parse : flavor:[< `Html | `Xml ] -> string -> (Markup.signal -> unit) -> unit

