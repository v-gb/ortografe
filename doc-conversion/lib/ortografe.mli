module Markup := Markup_t
type 'a stream = 'a Common.stream =
  | Markup of ('a, Markup.sync) Markup.stream
  | Fun of (('a -> unit) -> unit)

type impl =
  { parse : flavor:[ `Xml | `Html ] -> string -> Markup.signal stream
  ; print : flavor:[ `Xml | `Html ] -> Markup.signal stream -> (char -> unit) -> (string -> unit) -> unit
  }
val markup_impl : impl
val markup_print_impl : impl

type options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : bool
  ; impl : impl
  }

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out
  | Ignore : unit out
type 'a convert = options:options -> string -> dst:'a out -> 'a
val max_size : int ref

val pure_text : ?convert_text:(string -> string) -> ?buf:Buffer.t -> _ convert
val html : ?convert_text:(string -> string) -> _ convert
val htmlz : ?convert_text:(string -> string) -> _ convert
val officeopenxml : [< `Docx | `Pptx ] -> ?convert_text:(string -> string) -> _ convert
val epub : ?convert_text:(string -> string) -> _ convert
val officeopenxml_old : [< `Doc | `Ppt ] -> ?convert_text:(string -> string) -> _ convert
val opendocument : ?convert_text:(string -> string) -> _ convert

val convert_string : ext:string -> options:options -> string -> ([ `ext of string ] * string) option
val convert_files : ?src_type:string -> options:options -> string option -> string option -> unit
val ext_conv : ?src_type:string -> string option -> string option -> [< `Extract | `Insert of string option ] -> unit

val mimetype_by_ext : string -> string option

val map_zip
    : string
      -> (Zipc.Member.t -> (unit -> string) -> string option)
      -> string

module More_markup = More_markup
module Common = Common
  
(**/**)
module Private : sig
  val grab_from_zip : string -> string -> string
  val convert_officeopenxml : [< `Docx | `Pptx ] -> ?convert_text:(string -> string) -> _ convert
end
