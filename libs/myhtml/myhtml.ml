open Core
open struct module Markup = Markup_t end

type node =
  { classes : (string * string) list
  ; scripts : string list
  ; html : node Markup.node
  }

let trees_of_string ?(attrs = []) src : _ list =
  let attrs =
    List.map attrs ~f:(fun (k, v) -> ((Markup.Ns.html, k), v))
  in
  let trees =
    Markup.parse_html
      (Markup.string src)
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
  | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ as t -> t

let fold_node (n : node) ~init ~f =
  let rec fold acc n =
    let acc = f acc n in
    match n.html with
    | `Element (_, _, children) -> List.fold ~init:acc ~f:fold children
    | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> acc
  in
  fold init n

let html h = { classes = []; scripts = []; html = h }

let rec node_of_trees tree : node =
  html (map_node_nonrec node_of_trees tree)
let nodes_of_string ?attrs str =
  List.map ~f:node_of_trees (trees_of_string ?attrs str)

let elt name ?cl ?(attrs = []) children : node =
  let attrs, classes =
    match cl with
    | None -> attrs, []
    | Some style ->
       let this_class =
         name ^ "_" ^ String.prefix (Md5.to_hex (Md5.digest_string style)) 6
       in
       let attrs =
         let rewrote = ref false in
         let attrs =
           List.map attrs ~f:(function
               | ("class", v) ->
                  rewrote := true;
                  ("class", this_class ^ " " ^ v)
               | p -> p)
         in
         if !rewrote
         then attrs
         else ("class", this_class) :: attrs
       in
       attrs, [ this_class, style ]
  in
  let attrs =
    List.map attrs ~f:(fun (k, v) -> ((Markup.Ns.html, k), v))
  in
  { html = `Element ((Markup.Ns.html, name), attrs, children)
  ; classes
  ; scripts = []
  }
let leafelt name ?cl attrs : node = elt name ?cl ~attrs []
let comment s : node = html (`Comment s)
let text =
  let replace =
    lazy (
        let thin_nbws = "\u{202F}" in
        let alist =
          [ +List.map
               [ " :"
               ; " ?"
               ; " !"
               ; "« "
               ; " »"
               ] ~f:(fun s ->
                 s, String.substr_replace_all s ~pattern:" " ~with_:thin_nbws)
          ; "->", "→"
          ]
        in
        let table = Hashtbl.of_alist_exn (module String) alist in
        let re = Re.(compile (alt (List.map alist ~f:(fun (s, _) -> str s)))) in
        fun s -> Re.replace re s ~f:(fun group ->
                     Hashtbl.find_exn table (Re.Group.get group 0)))
  in
  fun s -> html (`Text (force replace s))

let list' which ?cl ?attrs attrs_children =
  elt ?cl
    (match which with `ul -> "ul" | `ol -> "ol")
    ?attrs
    (List.map attrs_children
       ~f:(fun (attrs, children) ->
         elt "li" ~attrs children))
let list which ?cl ?attrs children =
  list' which ?cl ?attrs (List.map children ~f:(fun x -> [], x))

let br = elt "br" []
let hr ?cl () = elt ?cl "hr" []
let a ~href ?cl ?(attrs = []) children = elt "a" ?cl ~attrs:(["href", href] @ attrs) children
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
let table ~header ?(cl="") l =
  elt "table"
    ~cl:("text-align:center;"^ cl)
    [ +(match header with
        | None -> []
        | Some h ->
           [ elt "thead"
               [ elt "tr" (List.map h ~f:(fun z -> elt "th" z)) ] ])
    ; elt "tbody"
        (List.map l ~f:(fun z -> elt "tr" (List.map z ~f:(fun zz -> elt "td" zz))))
    ]
let flex_cl ?(wrap = false) dir =
  let dir = match dir with `Row -> "row" | `Column -> "column" in
  let wrap = if wrap then "wrap" else "nowrap" in
  [%string "display: flex; flex-dir: %{dir}; flex-wrap: %{wrap}; "]
let pseudo_list ?cl ?attrs children : node list =
  let cl = (cl ||? "") ^ "
display:flex;
flex-direction: row;
gap:0.3em;
margin-left: 0.7em;
"
  in
  List.concat_mapi children ~f:(fun i (c, wrap) ->
      wrap (div ~cl ?attrs
              [ p ~cl:"margin:0;"
                  [ span ~attrs:["style",
                                 if i = 0
                                 then "letter-spacing: -0.1em"
                                 else "letter-spacing: -0.05em"]
                      [ text [%string "%{i+1#Int}."] ] ]
              ; c
    ]))
let id name =
  "#" ^ name, ("id", name)
let script src ~defer =
  leafelt "script" [ "src", src; +(if defer then [ "defer", ""] else []) ]
let style txt = elt "style" [ text txt ]

let html ~lang ~head ?body_style ~body () =
  let body = elt ?cl:body_style "body" body in
  let scripts =
    fold_node body ~init:[] ~f:(fun acc node -> List.rev_append node.scripts acc)
    |> List.rev
    |> Set.stable_dedup_list (module String)
  in
  let classes =
    let module M = struct
        type t = string * string
        let sexp_of_t _ = failwith "asd"
        let compare (c1, v1) (c2, v2) =
          let c = String.compare c1 c2 in
          if c = 0 && String.(<>) v1 v2
          then raise_s [%sexp "clash between classes",
                        (v1 : string), (v2 : string)];
          c
        include (val Comparator.make ~sexp_of_t ~compare)
      end in
    fold_node body ~init:[] ~f:(fun acc node -> List.rev_append node.classes acc)
    |> List.rev
    |> Set.stable_dedup_list (module M)
  in
  elt "html" ~attrs:["lang", lang ]
    [ elt "head"
        [ +head
        ; +List.map scripts ~f:(fun src -> script src ~defer:true)
        ; +match classes with
           | [] -> []
           | _ -> [ style (classes
                           |> List.map ~f:(fun (class_, value) ->
                                  [%string ".%{class_} { %{value} }"])
                           |> String.concat ~sep:"\n") ]
        ]
    ; body
    ]

let details ?cl ?summary_cl ?(open_ = false) summary body =
  elt ?cl "details"
    ~attrs:(if open_ then [ "open", "" ] else [])
    (elt ?cl:summary_cl "summary" summary :: body)
let cite ?cl body = elt ?cl "cite" body
