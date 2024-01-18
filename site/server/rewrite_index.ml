open Core

module Tmp = struct
  type name = string * string
  [@@deriving sexp_of]

  type 'a node =
    [ `Element of name * (name * string) list * 'a list
    | `Text of string
    | `Doctype of (Markup.doctype [@sexp.opaque])
    | `Xml of (Markup.xml_declaration [@sexp.opaque])
    | `PI of string * string
    | `Comment of string ]
  [@@deriving sexp_of]

  type tree = tree node
  [@@deriving sexp_of]
end

type tree = Tmp.tree [@@deriving sexp_of]
           
let trees signals =
  (* should try to upstream this, this is boilerplate that should be provided upstream *)
  Markup.trees signals
    ~text:(fun s -> `Text (String.concat s))
    ~element:(fun a b c -> `Element (a, b, c))
    ~comment:(fun a -> `Comment a)
    ~pi:(fun a b -> `PI (a, b))
    ~xml:(fun a -> `Xml a)
    ~doctype:(fun a -> `Doctype a)

let trees_of_string src : tree list =
  Markup.parse_html
    (Markup.string src)
  |> Markup.signals
  |> Markup.trim
  |> trees
  |> Markup.to_list

let string_of_trees trees =
  List.map trees ~f:(fun tree ->
      tree
      |> Markup.from_tree Fun.id
      |> Markup.pretty_print
      |> Markup.write_html
      |> Markup.to_string)
  |> String.concat_lines

let rec rewrite ~books_html = function
  | `Element (name, attrs, children) as initial
       when List.exists attrs ~f:(function
                | ((_, "class"), value) ->
                   List.mem ~equal:String.equal
                     (String.split value ~on:' ')
                     "to-transcribe"
                | _ -> false)
    ->
     let new_children =
       if String.(=) (snd name) "p"
       then children
       else
         match List.last_exn children with
         | `Element (_, _, children) -> children
         | _ -> raise_s [%sexp "hmm", (children : tree list)]
     in
     let new_node =
       ("<p class=transcribe-result>" ^ string_of_trees new_children ^ "</p>")
       |> Ortografe.html
            ~options:{ convert_uppercase = false; dict = Lazy.force Ortografe.erofa }
            ~dst:String
       |> trees_of_string
       |> List.hd_exn
     in
     [ initial; new_node ]
  | `Element ((_, "books"), _, _) ->
     trees_of_string books_html
  | `Element (name, attrs, children) ->
     [ `Element (name, attrs, List.concat_map children ~f:(rewrite ~books_html)) ]
  | elt -> [elt]

let rewrite index_html books_html =
  (* We insert the converted text into the index.html as compile time, so
     - it works without javascript
     - the layout doesn't move
     - in principle, we could stop sending the 500kB of dict, although we
       still do that currently. Although we wouldn't anymore if the moved
       the transcription to another page, so progress.
   *)
  index_html
  |> In_channel.read_all
  |> trees_of_string
  |> List.concat_map ~f:(rewrite ~books_html:(In_channel.read_all books_html))
  |> string_of_trees
  |> (fun data -> Out_channel.write_all "index.html" ~data)
