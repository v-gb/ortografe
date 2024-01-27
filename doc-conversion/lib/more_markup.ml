open Core

let docx_ns = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

type name = string * string [@@deriving equal]
let sexp_of_name (ns, name) =
  let ns =
    if String.(=) ns Markup.Ns.xml
    then "xml"
    else if String.(=) ns docx_ns
    then "docx"
    else ns
  in
  sexp_of_string (ns ^ ":" ^ name)

type xml_declaration = Markup.xml_declaration =
  {version    : string;
   encoding   : string option;
   standalone : bool option}
    [@@deriving equal, sexp_of]
type doctype = Markup.doctype =
  {doctype_name      : string option;
   public_identifier : string option;
   system_identifier : string option;
   raw_text          : string option;
   force_quirks      : bool}
    [@@deriving equal, sexp_of]
type signal =
  [ `Start_element of name * (name * string) list
  | `End_element
  | `Text of string list
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string ]
    [@@deriving equal, sexp_of]

type 'a node =
  [ `Element of name * (name * string) list * 'a list
  | `Text of string
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string ]
    [@@deriving sexp_of]
let sexp_of_node sexp_of_a node =
  match node with
  | `Element (name, [], children) ->
     [%sexp_of: [ `Element of (name * a list) ]] (`Element (name, children))
  | _ -> sexp_of_node sexp_of_a node

type tree = tree node
[@@deriving sexp_of]

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

type 'a convert_xml = ?debug:bool -> ?pp:bool -> 'a convert

let maybe_debug_signal ?(debug = false) signals =
  if debug
  then Markup.map (fun elt ->
           print_endline (Markup.signal_to_string elt);
           elt) signals
  else signals

let maybe_pp_signal ?(pp = false) signals =
  if pp then Markup.pretty_print signals else signals

let transform ?debug ?pp ~transform ~flavor src ~dst =
  (* https://v3.ocaml.org/p/markup/latest/doc/Markup/index.html
     Note: no implicit closing of tags *)
  let z =
    (match flavor with
     | `Xml -> Markup.parse_xml (Markup.string src)
     | `Html -> Markup.parse_html (Markup.string src))
    |> Markup.signals
    |> maybe_debug_signal ?debug
    |> transform
    |> maybe_pp_signal ?pp
  in
  (match flavor with
   | `Xml -> Markup.write_xml z
   | `Html -> Markup.write_html z)
  |> markup_output dst

let text_elt ~convert_text = function
  | `Text strs -> `Text (List.map ~f:convert_text strs)
  | elt -> elt
