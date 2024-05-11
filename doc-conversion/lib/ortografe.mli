type options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : bool
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

val map_zip
    : string
      -> (Zipc.Member.t -> (unit -> string) -> string option)
      -> string

module More_markup = More_markup
  
(**/**)
module Private : sig
  val grab_from_zip : string -> string -> string
  val convert_officeopenxml : [< `Docx | `Pptx ] -> ?convert_text:(string -> string) -> _ convert
end
