open Core

open struct
  module Markup = Markup_t
end

let concat_map f s = Markup.transform (fun () a -> (f a, Some ())) () s
let docx_ns = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

type name = string * string [@@deriving equal]

let sexp_of_name (ns, name) =
  let ns =
    if String.( = ) ns Markup.Ns.xml
    then "xml"
    else if String.( = ) ns docx_ns
    then "docx"
    else ns
  in
  sexp_of_string (ns ^ ":" ^ name)

type xml_declaration = Markup.xml_declaration =
  { version : string
  ; encoding : string option
  ; standalone : bool option
  }
[@@deriving equal, sexp_of]

type doctype = Markup.doctype =
  { doctype_name : string option
  ; public_identifier : string option
  ; system_identifier : string option
  ; raw_text : string option
  ; force_quirks : bool
  }
[@@deriving equal, sexp_of]

type signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string
  ]
[@@deriving equal, sexp_of]

type 'a node =
  [ `Element of name * (name * string) list * 'a list
  | `Text of string
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string
  ]
[@@deriving sexp_of]

let sexp_of_node sexp_of_a node =
  match node with
  | `Element (name, [], children) ->
      [%sexp_of: [ `Element of name * a list ]] (`Element (name, children))
  | _ -> sexp_of_node sexp_of_a node

type tree = tree node [@@deriving sexp_of]

let trees signals =
  (* should try to upstream this, this is boilerplate that should be provided upstream *)
  Markup.trees signals
    ~text:(fun s -> `Text (String.concat s))
    ~element:(fun a b c -> `Element (a, b, c))
    ~comment:(fun a -> `Comment a)
    ~pi:(fun a b -> `PI (a, b))
    ~xml:(fun a -> `Xml a)
    ~doctype:(fun a -> `Doctype a)

open Common

let transform (type a) ~transform ~flavor src ~(dst : a out) : a =
  (* https://v3.ocaml.org/p/markup/latest/doc/Markup/index.html
     Note: no implicit closing of tags *)
  Markup.string src
  |> (match flavor with
     | `Xml -> Markup.parse_xml ~entity:Markup.xhtml_entity __
     | `Html -> Markup.parse_html __)
  |> Markup.signals
  |> transform
  |> (match flavor with `Xml -> Markup.write_xml __ | `Html -> Markup.write_html __)
  |> Common.markup_output dst

let text_elt ~convert_text = function
  | `Text strs -> `Text (List.map ~f:convert_text strs)
  | elt -> elt
