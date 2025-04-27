open Core

open struct
  module Markup = Markup_t
end

module Css = struct
  type t =
    [ `Kv of string * string
    | `Raw of string
    | `Sel of string * string
    ]

  type length = float * [ `em | `rem | `px | `percent ]

  let to_string l =
    List.map l ~f:(function
      | `Raw r -> r
      | `Sel (sel, v) -> [%string "%{sel} %{v}\n"]
      | `Kv (k, v) -> [%string "%{k}: %{v};\n"])
    |> String.concat

  let zero = (0., `em)
  let em f = (f, `em)
  let rem f = (f, `rem)
  let px f = (f, `px)
  let percent f = (f, `percent)

  let size (f, unit) =
    match unit with
    | `em -> (
        match f with 0. -> "0" | _ -> Float.to_string_hum ~strip_zero:true f ^ "em")
    | `rem -> Float.to_string_hum ~strip_zero:true f ^ "rem"
    | `px -> Float.to_string_hum ~strip_zero:true f ^ "px"
    | `percent -> Float.to_string_hum ~strip_zero:true f ^ "%"

  let display = function
    | `block -> `Kv ("display", "block")
    | `inline -> `Kv ("display", "inline")
    | `inline_block -> `Kv ("display", "inline-block")
    | `none -> `Kv ("display", "none")

  let prop_lrtb name ?all ?lr ?tb ?l ?r ?t ?b () =
    [ +(match all with Some v -> [ `Kv (name, size v) ] | None -> [])
    ; +(match Option.first_some l lr with
       | None -> []
       | Some v -> [ `Kv (name ^ "-left", size v) ])
    ; +(match Option.first_some r lr with
       | None -> []
       | Some v -> [ `Kv (name ^ "-right", size v) ])
    ; +(match Option.first_some t tb with
       | None -> []
       | Some v -> [ `Kv (name ^ "-top", size v) ])
    ; +(match Option.first_some b tb with
       | None -> []
       | Some v -> [ `Kv (name ^ "-bottom", size v) ])
    ]

  let margin ?all = prop_lrtb "margin" ?all
  let margin_auto = `Kv ("margin", "auto")
  let width' raw = `Kv ("width", raw)
  let width f = width' (size f)
  let width_auto = `Kv ("width", "auto")
  let width_fit_content = `Kv ("width", "fit-content")
  let height' raw = `Kv ("height", raw)
  let height f = height' (size f)
  let height_auto = `Kv ("height", "auto")
  let padding ?all = prop_lrtb "padding" ?all
  let font_family l = `Kv ("font-family", String.concat ~sep:"," l)
  let pointer_events `none = `Kv ("pointer-events", "none")
  let color s = `Kv ("color", s)
  let text_decoration s = `Kv ("text-decoration", s)
  let text_decoration_line `underline = `Kv ("text-decoration-line", "underline")
  let background_color s = `Kv ("background-color", s)
  let max_width' raw = `Kv ("max-width", raw)
  let max_width f = max_width' (size f)
  let min_width' raw = `Kv ("min-width", raw)
  let min_width f = min_width' (size f)
  let min_height' raw = `Kv ("min-height", raw)
  let min_height f = min_height' (size f)
  let list_style_type `None = `Kv ("list-style-type", "none")
  let font_size' raw = `Kv ("font-size", raw)
  let font_size f = font_size' (size f)
  let font_weight `Bold = `Kv ("font-weight", "bold")

  let border ?width ?style ?color ?radius () =
    [ +(match width with None -> [] | Some f -> [ `Kv ("border-width", size f) ])
    ; +(match style with
       | None -> []
       | Some `solid -> [ `Kv ("border-style", "solid") ])
    ; +(match color with None -> [] | Some f -> [ `Kv ("border-color", f) ])
    ; +(match radius with None -> [] | Some f -> [ `Kv ("border-radius", size f) ])
    ]

  let whitespace `pre_wrap = `Kv ("white-space", "pre-wrap")

  let position ?all ?lr ?tb ?l ?r ?t ?b which =
    let lr = Option.first_some lr all in
    let tb = Option.first_some tb all in
    [ (match which with
      | `relative -> `Kv ("position", "relative")
      | `absolute -> `Kv ("position", "absolute"))
    ; +(match Option.first_some l lr with
       | None -> []
       | Some v -> [ `Kv ("left", size v) ])
    ; +(match Option.first_some r lr with
       | None -> []
       | Some v -> [ `Kv ("right", size v) ])
    ; +(match Option.first_some t tb with
       | None -> []
       | Some v -> [ `Kv ("top", size v) ])
    ; +(match Option.first_some b tb with
       | None -> []
       | Some v -> [ `Kv ("bottom", size v) ])
    ]

  let aspect_ratio a b = `Kv ("aspect-ratio", sprintf "%d/%d" a b)

  let float = function
    | `Right -> `Kv ("float", "right")
    | `Left -> `Kv ("float", "left")

  let hyphens `auto = `Kv ("hyphens", "auto")

  let flex_child ?grow ?shrink ?basis () =
    [ +(match grow with Some f -> [ `Kv ("flex-grow", Int.to_string f) ] | None -> [])
    ; +(match shrink with
       | Some f -> [ `Kv ("flex-shrink", Int.to_string f) ]
       | None -> [])
    ; +(match basis with Some f -> [ `Kv ("flex-basis", size f) ] | None -> [])
    ]

  let flex ?wrap ?gap ?grow ?shrink ?basis ?row_gap ?column_gap direction =
    [ `Kv ("display", "flex")
    ; (match direction with
      | `Row -> `Kv ("flex-direction", "row")
      | `Column -> `Kv ("flex-direction", "column"))
    ; +(match wrap with Some `Wrap -> [ `Kv ("flex-wrap", "wrap") ] | None -> [])
    ; +(match gap with Some f -> [ `Kv ("gap", size f) ] | None -> [])
    ; +(match row_gap with Some f -> [ `Kv ("row-gap", size f) ] | None -> [])
    ; +(match column_gap with Some f -> [ `Kv ("column-gap", size f) ] | None -> [])
    ; +(match grow with Some f -> [ `Kv ("flex-grow", Int.to_string f) ] | None -> [])
    ; +(match shrink with
       | Some f -> [ `Kv ("flex-shrink", Int.to_string f) ]
       | None -> [])
    ; +(match basis with Some f -> [ `Kv ("flex-basis", size f) ] | None -> [])
    ]

  let justify_content `center = `Kv ("justify-content", "center")
  let text_align `center = `Kv ("text-align", "center")

  let clear = function
    | `left -> `Kv ("clear", "left")
    | `right -> `Kv ("clear", "right")
    | `both -> `Kv ("clear", "both")

  let selector sel l =
    match l with [] -> [] | _ :: _ -> [ `Sel (sel, "{\n" ^ to_string l ^ "}") ]

  let raw1 s = `Raw s
  let raw s = [ `Raw s ]
  let _ = (width_auto, height)
end

type node =
  { classes : (string * string) list
  ; scripts : string list
  ; html : node Markup.node
  }

let trees_of_string ?(attrs = []) src : _ list =
  let attrs = List.map attrs ~f:(fun (k, v) -> ((Markup.Ns.html, k), v)) in
  let trees =
    Markup.parse_html (Markup.string src)
    |> Markup.signals
    |> Markup.trim
    |> Markup.trees
         ~text:(fun s -> `Text (String.concat s))
         ~element:(fun a b c -> `Element (a, b, c))
         ~comment:(fun a -> `Comment a)
         ~pi:(fun a b -> `PI (a, b))
         ~xml:(fun a -> `Xml a)
         ~doctype:(fun a -> `Doctype a)
    |> Markup.to_list
  in
  match trees with
  | `Element (a, attrs', children) :: rest ->
      `Element (a, attrs @ attrs', children) :: rest
  | _ -> trees

let map_node_nonrec map_a = function
  | `Element (name, attrs, children) ->
      `Element (name, attrs, List.map ~f:map_a children)
  | (`Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _) as t -> t

let fold_node (n : node) init f =
  let rec fold acc n =
    let acc = f n acc in
    match n.html with
    | `Element (_, _, children) -> List.fold ~init:acc ~f:fold children
    | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> acc
  in
  fold init n

let html h = { classes = []; scripts = []; html = h }
let rec node_of_trees tree : node = html (map_node_nonrec node_of_trees tree)
let nodes_of_string ?attrs str = List.map ~f:node_of_trees (trees_of_string ?attrs str)

let elt name ?cl ?(attrs = []) children : node =
  let attrs, classes =
    match cl with
    | None -> (attrs, [])
    | Some style ->
        let this_class =
          name ^ "_" ^ String.prefix (Md5.to_hex (Md5.digest_string style)) 6
        in
        let attrs =
          let rewrote = ref false in
          let attrs =
            List.map attrs ~f:(function
              | "class", v ->
                  rewrote := true;
                  ("class", this_class ^ " " ^ v)
              | p -> p)
          in
          if !rewrote then attrs else ("class", this_class) :: attrs
        in
        (attrs, [ (this_class, style) ])
  in
  let attrs = List.map attrs ~f:(fun (k, v) -> ((Markup.Ns.html, k), v)) in
  { html = `Element ((Markup.Ns.html, name), attrs, children); classes; scripts = [] }

let leafelt name ?cl attrs : node = elt name ?cl ~attrs []
let comment s : node = html (`Comment s)

let text =
  let replace =
    lazy
      (let thin_nbws = "\u{202F}" in
       let alist =
         [ +List.map [ " :"; " ?"; " !"; "« "; " »" ] ~f:(fun s ->
                (s, String.substr_replace_all s ~pattern:" " ~with_:thin_nbws))
         ; ("->", "→")
         ]
       in
       let table = Hashtbl.of_alist_exn (module String) alist in
       let re = Re.(compile (alt (List.map alist ~f:(fun (s, _) -> str s)))) in
       fun s ->
         Re.replace re s ~f:(fun group -> Hashtbl.find_exn table (Re.Group.get group 0)))
  in
  fun s -> html (`Text (force replace s))

let list' which ?cl ?attrs attrs_children =
  elt ?cl
    (match which with `ul -> "ul" | `ol -> "ol")
    ?attrs
    (List.map attrs_children ~f:(fun (attrs, children) -> elt "li" ~attrs children))

let list which ?cl ?attrs children =
  list' which ?cl ?attrs (List.map children ~f:(fun x -> ([], x)))

let br = elt "br" []
let hr ?cl () = elt ?cl "hr" []

let a ~href ?cl ?(attrs = []) children =
  elt "a" ?cl ~attrs:([ ("href", href) ] @ attrs) children

let div ?cl ?attrs children = elt "div" ?cl ?attrs children
let h1 ?cl ?attrs children = elt "h1" ?cl ?attrs children
let h2 ?cl ?attrs children = elt "h2" ?cl ?attrs children
let h3 ?cl ?attrs children = elt "h3" ?cl ?attrs children
let span ?cl ?attrs children = elt "span" ?cl ?attrs children
let p ?cl ?attrs children = elt "p" ?cl ?attrs children
let strong ?cl ?attrs children = elt "strong" ?cl ?attrs children
let textarea ?cl ?attrs children = elt "textarea" ?cl ?attrs children
let section ?cl ?attrs children = elt "section" ?cl ?attrs children
let img ?cl src attrs = leafelt "img" ?cl (("src", src) :: attrs)
let code ?cl ?attrs children = elt "code" ?cl ?attrs children

let table ~header ?(cl = "") l =
  elt "table" ~cl:("text-align:center;" ^ cl)
    [ +(match header with
       | None -> []
       | Some h -> [ elt "thead" [ elt "tr" (List.map h ~f:(fun z -> elt "th" z)) ] ])
    ; elt "tbody"
        (List.map l ~f:(fun z -> elt "tr" (List.map z ~f:(fun zz -> elt "td" zz))))
    ]

let flex_cl ?(wrap = false) dir =
  let dir = match dir with `Row -> "row" | `Column -> "column" in
  let wrap = if wrap then "wrap" else "nowrap" in
  [%string "display: flex; flex-dir: %{dir}; flex-wrap: %{wrap}; "]

let pseudo_list ?cl ?attrs children : node list =
  let cl =
    (cl ||? "")
    ^ "\ndisplay:flex;\nflex-direction: row;\ngap:0.3em;\nmargin-left: 0.7em;\n"
  in
  List.concat_mapi children ~f:(fun i (c, wrap) ->
      wrap
        (div ~cl ?attrs
           [ p ~cl:"margin:0;"
               [ span
                   ~attrs:
                     [ ( "style"
                       , if i = 0
                         then "letter-spacing: -0.1em"
                         else "letter-spacing: -0.05em" )
                     ]
                   [ text [%string "%{i+1#Int}."] ]
               ]
           ; c
           ]))

let id name = ("#" ^ name, ("id", name))

let script src ~defer =
  leafelt "script" [ ("src", src); +(if defer then [ ("defer", "") ] else []) ]

let style txt = elt "style" [ text txt ]

let style_tag node =
  let classes =
    fold_node node [] (fun node acc -> List.rev_append node.classes acc)
    |> List.rev
    |> List.stable_dedup ~compare:(fun (c1, v1) (c2, v2) ->
           let c = String.compare c1 c2 in
           if c = 0 && String.( <> ) v1 v2
           then raise_s [%sexp "clash between classes", (v1 : string), (v2 : string)];
           c)
  in
  match classes with
  | [] -> []
  | _ ->
      [ style
          (classes
          |> List.map ~f:(fun (class_, value) -> [%string ".%{class_} { %{value} }"])
          |> String.concat ~sep:"\n")
      ]

let html ~lang ~head ?body_style ~body () =
  let body = elt ?cl:body_style "body" body in
  let scripts =
    fold_node body [] (fun node acc -> List.rev_append node.scripts acc)
    |> List.rev
    |> List.stable_dedup ~compare:String.compare
  in
  elt "html"
    ~attrs:[ ("lang", lang) ]
    [ elt "head"
        [ +head
        ; +List.map scripts ~f:(fun src -> script src ~defer:true)
        ; +style_tag body
        ]
    ; body
    ]

let details ?cl ?summary_cl ?(attrs = []) ?(open_ = false) summary body =
  elt ?cl "details"
    ~attrs:(if open_ then ("open", "") :: attrs else attrs)
    (elt ?cl:summary_cl "summary" summary :: body)

let cite ?cl body = elt ?cl "cite" body
