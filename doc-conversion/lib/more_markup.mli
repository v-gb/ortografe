(** Extension of the [Markup] library *)

val docx_ns : string

type name = Markup.name [@@deriving equal, sexp_of]
type 'a node = 'a Markup.node [@@deriving sexp_of]

type tree = tree node [@@deriving sexp_of]
type signal = Markup.signal [@@deriving equal, sexp_of]
val trees : (Markup.signal, 'a) Markup.stream -> (tree, 'a) Markup.stream

type 'a convert_xml = ?debug:bool -> ?pp:bool -> 'a Common.convert

val transform
   : ?debug:bool
  -> ?pp:bool
  -> transform:((Markup.signal, Markup.sync) Markup.stream ->
                (Markup.signal, Markup.sync) Markup.stream)
  -> flavor:[ `Xml | `Html ]
  -> string
  -> dst:'a Common.out
  -> 'a

val text_elt : convert_text:(string -> string) -> signal -> signal
