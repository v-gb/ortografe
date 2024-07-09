module Markup := Markup_t
(** Extension of the [Markup] library *)

type 'a stream := ('a, Markup.sync) Markup.stream

val concat_map : ('a -> 'b list) -> 'a stream -> 'b stream
val docx_ns : string

type name = Markup.name [@@deriving equal, sexp_of]
type 'a node = 'a Markup.node [@@deriving sexp_of]
type tree = tree node [@@deriving sexp_of]
type signal = Markup.signal [@@deriving equal, sexp_of]

val trees : (Markup.signal, 'a) Markup.stream -> (tree, 'a) Markup.stream

val transform :
     transform:(Markup.signal stream -> Markup.signal stream)
  -> flavor:[ `Xml | `Html ]
  -> string
  -> dst:'a Common.out
  -> 'a

val text_elt : convert_text:(string -> string) -> signal -> signal
