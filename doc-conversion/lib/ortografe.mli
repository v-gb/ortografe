val erofa : (string, string) Hashtbl.t Lazy.t
val rect1990 : (string, string) Hashtbl.t Lazy.t
type options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : bool
  }

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out
type 'a convert = ?buf:Buffer.t -> options:options -> string -> dst:'a out -> 'a
val max_size : int ref

val pure_text : _ convert
val html : _ convert
val htmlz : _ convert
val docx : _ convert
val epub : _ convert
val doc : _ convert
val opendocument : _ convert

val convert_string : ext:string -> options:options -> string -> (string * string) option
val convert_files : options:options -> string option -> string option -> unit

val map_zip
    : string
      -> (Zipc.Member.t -> (unit -> string) -> string option)
      -> string

module More_markup = More_markup
  
(**/**)
val extension_dict1990_gen_csv : string
module Private : sig
  val grab_from_zip : string -> string -> string
  val convert_docx_xml : _ convert
end
