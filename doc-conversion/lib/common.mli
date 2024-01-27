(** A few misc utility functions and types *)

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out
val write_out : 'a out -> string -> 'a
val markup_output : 'a out -> (char, Markup.sync) Markup.stream -> 'a

val buffer : ?n:int -> Buffer.t option -> Buffer.t

type options =
  { convert_uppercase : bool
  ; dict : (string, string) Hashtbl.t
  ; interleaved : bool
  }
type 'a convert = ?buf:Buffer.t -> options:options -> string -> dst:'a out -> 'a
