open Base

type 'a src =
  [ `Str of string | `Root of ([> Eio.Fs.dir_ty ] as 'a) Eio.Path.t ]

val load_erofa : _ src -> (string, string) Hashtbl.t
val load_post90 : _ src -> (string, string) Hashtbl.t
val load_lexique : _ src -> Data.Lexique.t list
