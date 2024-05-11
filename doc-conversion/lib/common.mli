(** A few misc utility functions and types *)

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out
  | Ignore : unit out
val write_out : 'a out -> string -> 'a
val markup_output : 'a out -> (char, Markup.sync) Markup.stream -> 'a

val buffer : ?n:int -> Buffer.t option -> Buffer.t

type options =
  { convert_uppercase : bool
  ; dict : (string -> string option)
  ; interleaved : bool
  ; plurals_in_s : bool
  }
type 'a convert = options:options -> string -> dst:'a out -> 'a
