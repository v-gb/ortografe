(** Extension of the [Markup] library *)

val map : ('a -> 'b) -> 'a Common.stream -> 'b Common.stream
val filter : ('a -> bool) -> 'a Common.stream -> 'a Common.stream
val concat_map : ('a -> 'b list) -> 'a Common.stream -> 'b Common.stream
val duplicate : 'a Common.stream -> 'a Common.stream * 'a Common.stream

val docx_ns : string

type name = Markup.name [@@deriving equal, sexp_of]
type 'a node = 'a Markup.node [@@deriving sexp_of]

type tree = tree node [@@deriving sexp_of]
type signal = Markup.signal [@@deriving equal, sexp_of]
val trees : (Markup.signal, 'a) Markup.stream -> (tree, 'a) Markup.stream

val transform
   : impl:Common.impl
  -> transform:(Markup.signal Common.stream -> Markup.signal Common.stream)
  -> flavor:[ `Xml | `Html ]
  -> string
  -> dst:'a Common.out
  -> 'a

val text_elt : convert_text:(string -> string) -> signal -> signal
