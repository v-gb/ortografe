type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out

val pure_text : ?buf:Buffer.t -> string -> dst:'a out -> 'a
val html : ?buf:Buffer.t -> ?debug:bool -> ?pp:bool -> string -> dst:'a out -> 'a
val docx : ?buf:Buffer.t -> ?debug:bool -> ?pp:bool -> string -> dst:'a out -> 'a
val epub : ?buf:Buffer.t -> ?debug:bool -> ?pp:bool -> string -> dst:'a out -> 'a
val doc : ?buf:Buffer.t -> ?debug:bool -> ?pp:bool -> string -> dst:'a out -> 'a
val main : unit -> unit
val max_size : int ref

(**/**)
module Private : sig
  val docx_document : string -> string
  val read_whole_zip : string -> string
end
