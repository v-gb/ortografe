type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out

let write_out (type a) (out : a out) (string : string) : a =
  match out with
  | String -> string
  | Channel ch -> Out_channel.output_string ch string

let markup_output (type a) (out : a out) : ((char, Markup.sync) Markup.stream -> a) =
  match out with
  | String -> Markup.to_string
  | Channel ch -> Markup.to_channel ch

let buffer ?(n = 123) buf =
  match buf with
  | Some buf -> Buffer.clear buf; buf
  | None -> Buffer.create n

type options =
  { convert_uppercase : bool
  ; dict : (string, string) Hashtbl.t
  ; interleaved : bool
  ; plurals_in_s : bool
  }
type 'a convert = ?buf:Buffer.t -> options:options -> string -> dst:'a out -> 'a
