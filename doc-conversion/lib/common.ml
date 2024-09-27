open struct
  module Markup = Markup_t
end

module Substring = struct
  type t =
    { string : string
    ; start : int
    ; len : int
    }

  let of_string s = { string = s; start = 0; len = String.length s }
end

module Buf = struct
  type t =
    { mutable b : bytes
    ; mutable len : int
    }

  let create n = { b = Bytes.create n; len = 0 }

  let resize t extra =
    let new_len = Int.max (t.len + extra) ((Bytes.length t.b * 2) + 1) in
    let new_b = Bytes.create new_len in
    Bytes.blit t.b 0 new_b 0 t.len;
    t.b <- new_b

  let add_char t c =
    if t.len + 1 > Bytes.length t.b then resize t 1;
    Bytes.unsafe_set t.b t.len c;
    t.len <- t.len + 1

  let add_string t s =
    let n = String.length s in
    if t.len + n > Bytes.length t.b then resize t n;
    Bytes.unsafe_blit_string s 0 t.b t.len n;
    t.len <- t.len + n

  let substring__consume t : Substring.t =
    { string = Bytes.unsafe_to_string t.b; start = 0; len = t.len }
end

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out
  | Substring : int -> Substring.t out
  | Ignore : unit out

let substring_out s = Substring (String.length s * 11 / 10)

let write_out (type a) (out : a out) (string : string) : a =
  match out with
  | Ignore -> ()
  | String -> string
  | Substring _ -> Substring.of_string string
  | Channel ch -> Out_channel.output_string ch string

let markup_output (type a) (out : a out) : (char, Markup.sync) Markup.stream -> a =
  match out with
  | Ignore -> ignore
  | String -> Markup.to_string
  | Substring n ->
      fun stream ->
        let buffer = Buf.create n in
        Markup.iter (fun b -> Buf.add_char buffer b) stream;
        Buf.substring__consume buffer
  | Channel ch -> Markup.to_channel ch

let buffer ?(n = 123) buf =
  match buf with
  | Some buf ->
      Buffer.clear buf;
      buf
  | None -> Buffer.create n

type options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : string option
  }

type 'a convert =
  ?progress:(int -> unit) -> options:options -> string -> dst:'a out -> 'a
