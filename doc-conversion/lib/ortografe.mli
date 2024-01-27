type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out

val erofa : (string, string) Hashtbl.t Lazy.t
val rect1990 : (string, string) Hashtbl.t Lazy.t
type options =
  { convert_uppercase : bool
  ; dict : (string, string) Hashtbl.t
  }

type 'a convert = ?buf:Buffer.t -> options:options -> string -> dst:'a out -> 'a
type 'a convert_xml = ?debug:bool -> ?pp:bool -> 'a convert
val pure_text : _ convert
val html : 'a convert_xml
val htmlz : 'a convert_xml
val docx : 'a convert_xml
val epub : 'a convert_xml
val doc : 'a convert_xml
val max_size : int ref

val convert_string : ext:string -> options:options -> string -> (string * string) option
val convert_files : options:options -> string option -> string option -> unit

val map_zip
    : string
      -> (Zipc.Member.t -> (unit -> string) -> string option)
      -> string

module More_markup = More_markup
  
(**/**)
module Private : sig
  val grab_from_zip : string -> string -> string
  val join_consecutive_ish_text_nodes : (Markup.signal, 'a) Markup.stream -> (Markup.signal, 'a) Markup.stream
end
