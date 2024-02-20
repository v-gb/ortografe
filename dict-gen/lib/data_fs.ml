open Base
module Data = Dict_gen_common.Data

type 'a src =
  [ `Str of string | `Root of ([> Eio.Fs.dir_ty ] as 'a) Eio.Path.t ]

let (^/) = Eio.Path.(/)

let in_build_dir ~root filename =
  if Eio.Path.is_file (root ^/ filename)
  then root ^/ filename
  else (root ^/ "_build/default") ^/ filename

let read src filename =
  match src with
  | `Str str -> str
  | `Root root -> Eio.Path.load (in_build_dir ~root filename)

let load_erofa src =
  Data.parse_erofa
    (read src "extension/dict.external-sources.gen.csv")

let load_post90 src =
  Data.parse_post90
    (read src "extension/dict1990.gen.csv")

let load_lexique src =
  Data.Lexique.parse
    (read src "data/lexique/Lexique383.gen.tsv")
