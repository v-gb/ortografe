open Core
open struct module Markup = Markup_t end

module General_purpose = struct
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
      |> Ortografe.More_markup.trees
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

  let rec node_of_trees (tree : Ortografe.More_markup.tree) : node =
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
  let a ?cl ?(attrs = []) ~href children = elt "a" ?cl ~attrs:(["href", href] @ attrs) children
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

  let _ = comment
end

open General_purpose

let green_pale = "#e5fbe5"
let green = "#b9f4b9"
let _ = green_pale

let html ~head ?body_style ~body () =
  html ~lang:"fr" ~head ?body_style ~body ()

let head ~root ~attrs ~title ~description () =
  [ leafelt "meta" ["name", "viewport"; "content", "width=device-width, initial-scale=1"]
  ; leafelt "meta" ["charset", "utf-8"]
  (* proving ownership for search engine consoles *)
  ; +if root
     then
       [ leafelt "meta" (* bing *)
           [ "name", "msvalidate.01"
           ; "content", "528A9A3C7E6F9E5C349FB47AB8447469" ]
       ; leafelt "meta" (* gsearch for https://ortografe-server.fly.dev *)
           [ "name", "google-site-verification"
           ; "content", "cium7Nf85Z4I0Wj9O3Ck5ZwwkUzUQ_h_cwcwJHEUug8" ]
       ; leafelt "meta" (* gsearch for https://orthographe-rationnelle.info *)
           [ "name", "google-site-verification"
           ; "content", "Bv_wuz7zTmy7xlz2yedr5Zsjub8_AIQKH_HpSNmcqSU" ]
       ]
     else []
  ; elt "title" [ text title ]
  ; leafelt "meta" [ "name", "description"; "content", description ]
  (* It would be good to have the open-graph stuff
   *  https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML/The_head_metadata_in_HTML *)
  ; leafelt "link" [ "rel", "icon"; "href", "/static/favicon.png" ]
  ; +attrs
  ; style {|
/* https://www.joshwcomeau.com/css/custom-css-reset/ */
input, button, textarea, select {
  font: inherit;
}

a:link, a:visited {
  color: #106410;
  text-underline-offset: 1.5px;
}

.for-active-browser {
  background-color: #e5fbe5;
  padding: 3px;
  border-radius: 6px;
}

.exp-hidden {
  display: none;
}
.exp-shown {
  background-color: #ffecce;
  border-radius: 4px;
}
|}
  ]
let exp_hidden = "exp-hidden"
let exp_hidden_class = "class", exp_hidden

let navbar l =
  let a = a ~cl:"display: block;
                 margin: 0;
                 padding: 20px 20px 20px;
                 &:link, &:visited {
                 font-family: sans-serif;
                 color: inherit;
                 text-decoration: unset;
                 }
                 &:hover {
                 text-decoration-line: underline;
                 }
                 "
  in
  elt "nav" ~cl:"background-color: #F5F5F5;"
    [ div ~cl:"max-width: 55em; margin: auto; "
        [ list `ul ~cl:"margin: 0;
                        padding: 0;
                        list-style-type: none;
                        font-size: 1.1em;
                        font-weight: bold;
                        display:flex;
                        flex-direction: row;"
            (List.map l ~f:(fun (href, content) -> [ a ~href content ]))
        ]
    ]

let h1 ?cl children = h1 ~cl:(
  "margin: 1.67em 0;
   font-family: sans-serif;
   font-size: calc( 1.4em + (5 - 1.4) * ( (100cqw - 400px) / ( 800 - 400) ));
" ^ (cl ||? "")) children

let h2 children = h2 ~cl:"
  margin-top: 1em;
  margin-bottom: 0.3em;
  font-size: 2.5rem;
  font-family: sans-serif;
" children

let h3 children = h3 ~cl:"
  margin-top: 2.2em;
  margin-bottom: 0.8em;
  font-size: 1.3rem;
" children

module Index = struct
  let regles_ref, regles_def = id "regles"
  let outils_ref, outils_def = id "outils"
  let aller_plus_loin_ref, aller_plus_loin_def = id "aller-plus-loin"

  let navbar () =
    navbar
      [ regles_ref, [ text "Règles" ]
      ; outils_ref, [ text "Outils" ]
      ; aller_plus_loin_ref, [ text "Aller plus loin" ]
      ]

  let introduction () =
    let books ~attrs =
      (* ça, on peut le calculer direct en mémoire, pas besoin de passer par un fichier *)
      nodes_of_string ~attrs (In_channel.read_all "../static/books.html")
    in
    [
      div ~cl:"hyphens: auto"
        [ div ~cl:"float: right; max-width: calc(min(50%, 10em*320/180)); flex: 1 10em; \
                   margin:0; margin-left: 0.5em; position: relative;"
            [ a ~href:"https://www.youtube.com/embed/5YO7Vg1ByA8?autoplay=1"
                ~attrs:["target", "_blank"]
                [ img
                    "/static/hoedt_piron.jpg"
                    ~cl:"aspect-ratio:320/180; max-width: 100%; height: auto;"
                    [ "alt", "La faute de l'orthographe | Arnaud Hoedt, Jérôme Piron \
                              | TEDxRennes"
                    ; "width", "320"
                    ; "height", "180" ]
                ]
            (* Svg and some of the styling taken from youtube's embed iframes instead of
             using the actual iframe, because the iframe itself loads 1MB of stuff and
             slows the loading of the page. *)
            ; div ~cl:"
                       position: absolute;
                       left: 50%;
                       top: 50%;
                       width: 25%;
                       height: calc(68/48*25%);
                       margin-left: -12.5%;
                       margin-top: -11.5%;
                       pointer-events: none;
                       "
                (nodes_of_string {|<svg height="100%" version="1.1" viewBox="0 0 68 48" width="100%"><path class="ytp-large-play-button-bg" d="M66.52,7.74c-0.78-2.93-2.49-5.41-5.42-6.19C55.79,.13,34,0,34,0S12.21,.13,6.9,1.55 C3.97,2.33,2.27,4.81,1.48,7.74C0.06,13.05,0,24,0,24s0.06,10.95,1.48,16.26c0.78,2.93,2.49,5.41,5.42,6.19 C12.21,47.87,34,48,34,48s21.79-0.13,27.1-1.55c2.93-0.78,4.64-3.26,5.42-6.19C67.94,34.95,68,24,68,24S67.94,13.05,66.52,7.74z" fill="#f00"></path><path d="M 45,24 27,14 27,34" fill="#fff"></path></svg>|})
            ]
        ; p
            [ text "Les linguistes de l'"
            ; a ~href:"http://erofa.free.fr/" [ text "association Érofa" ]
            ; text " proposent trois règles qui rationalisent quelques points \
                    difficiles de l'orthographe du français."
            ]
        ; p
            [ text "Si vous trouvez notre orthographe déjà parfaitement \
                    rationnelle, visionnez cette édifiante vidéo !"
            ]
        ; p
            [ text "Sinon, "
            ; strong [ text "allez voir à quoi ça ressemble dans les \
                             textes suivants avant de lire les règles !" ]
            ]
        ]
    ; +books ~attrs:["style", "
    margin-left: auto;
    margin-right: auto;
    max-width: 40em;
    margin-top: 1.5em;
    padding-left: 0;
    list-style-type: none;
    display: flex;
    flex-flow: row wrap;
    column-gap: 1.5em;
    row-gap: 0.8em;
    justify-content: center;
    text-align:center;
    clear: both
  "]
    ; p ~cl:"font-size: 0.8em;"
        [ text "Notez que la transcription est automatique, et peut faire des erreurs.\n"
        ; text "Notez également qu'Érofa est bâtie sur les rectifications de \
                1990, qui sont donc appliquées."
        ]
    ]

  let interactive_transcription ~initial_text ~id_textarea ~id_converted_text =
    (* https://css-tricks.com/snippets/css/a-guide-to-flexbox/ *)
    let textarea_body, textarea_placeholder =
      match initial_text with
      | `Body str -> [ text str ], []
      | `Placeholder str -> [], [ "placeholder", str ]
    in
    div ~cl:"
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
    gap: 1em;
  "
      [ textarea
          ~cl:"
    background-color: #FAF9F6;
    flex: 1 1 0px;
    min-width: 15em;
    min-height: 6em;
    max-width: 30em;
  "
          ~attrs:["id", id_textarea; +textarea_placeholder]
          textarea_body
      ; div
          ~cl:"
    white-space: pre-wrap;
    flex: 1 1 0px;
    min-width: 15em;
    max-width: 30em;
    min-height: 6em;
    border: 2px solid #b9f4b9;
    background-color: #e5fbe5;
    padding: 3px;
    border-radius: 6px;
  "
          ~attrs:[ "id", id_converted_text
                 ; "class", "notranscribe"] []
      ]

  let transcribe_word w =
    Ortografe.pure_text
      ~options:{ convert_uppercase = false
               ; dict = Stdlib.Hashtbl.find_opt
                          (Lazy.force Ortografe_embedded.erofa)
               ; interleaved = true
               ; plurals_in_s = true
      }
      ~dst:String
      w

  let section_regles () =
    let transcription color =
      let border_color, background_color =
        match color with
        | `green -> "#b9f4b9", "#e5fbe5"
        | `grey -> "grey", "#FAF9F6"
      in
      [%string "
    border: 2px solid %{border_color};
    background-color: %{background_color};
    padding-top: 1px;
    padding-bottom: 2px;
    padding-left: 4px;
    padding-right: 4px;
    border-radius: 4px;
    margin-top: 0.1em;
    margin-bottom: 0.1em;
  "] in
    let words txt ws1 ws2 =
      text txt,
      (fun x ->
        [ x
        ; div ~cl:(flex_cl `Row ~wrap:true
                   ^ "justify-content: center; gap: 0.7em; margin-top: 0.2em;")
            [ table ~header:(Some [[text "Exemples"]])
                ~cl:(transcription `green)
                (List.map ws1 ~f:(fun w ->
                     let w' = transcribe_word w in
                     [ [ text w; text " → ";  text w' ] ]))
            ; table ~header:(Some [[text "Mais pas"]])
                ~cl:(transcription `grey)
                (List.map ws2 ~f:(fun w ->
                     [ [ text w ] ]))
            ]
        ])
    in
    section ~attrs:[regles_def]
      [ h2 [ text "Les règles" ]
      ; div ~cl:"display: flex; flex-direction: column;"
          ~attrs:[ "class", "notranscribe" ]
          [ p ~cl:"margin: 0;"
              [ text "Vous avez dû constater que les textes se lisent bien, même \
                      sans explication, et vous avez peut-être deviné une partie des \
                      règles. Excluant les noms propres :" ]
          ; div ~cl:"max-width: 25em;"
              (pseudo_list ~cl:"margin-top: 1em;"
                 [ words
                     "Les consonnes doubles qui n'ont pas d'effet sur la \
                      prononciation sont simplifiées."
                     [ "sonnerie"; "appelle"; "accord"; "lutte"; "verre" ]
                     [ "messe"; "accident"; "ennui"; "surréel"; "Rennes" ]
                 ; words
                     "Les x finaux muets deviennent des s."
                     [ "bijoux"; "choix"; "veux"; "deux" ]
                     [ "dix"; "duplex"; "intox" ]
                 ; words "Les ph, h et y d'origines grecques ou similaires \
                          sont simplifiés."
                     [ "photo"; "rythme"; "humain"
                       ; "chaos"; "théâtre"; "huile" ]
                     [ "hache"; "ahuri"; "pays"; "babyfoot" ]
                 ])
           ]
      ; p ~cl:"font-size: 0.8em; margin-top: 1.5em;"
          [ text "Érofa propose également une "
          ; a ~href:"http://www.participepasse.info/"
              [ text "règle sur l'accord du participe passé" ]
          ; text ", qui est recommandée par plusieurs instances francophones \
                  compétentes, mais les outils de ce site ne peuvent pas l'appliquer."
          ]
      ]

  let section_dictionnaire () =
    section
      [ h3 [ text "Dictionnaire de transcription" ]
      ; div
          ~cl:"text-align: center;"
          [ leafelt "input"
              [ "type", "text"
              ; "placeholder", "Cherchez un mot !"
              ; "id", "dict-search-input"
              ]
          ]
      ; div
          ~attrs:[ "id", "dict-search-output" ]
          ~cl:"table { margin: auto }
               td { text-align: center }
               tbody tr:nth-child(odd) {
                 background-color: #FAF9F6;
               }
               "
          []
      ; p ~cl:"font-size: 0.8em;"
          [ text "Il est possible qu'il y ait quelques erreurs sur des mots peu communs." ]
      ]

  let section_transcription_interactive () =
    section
      [ h3 [ text "Transcription interactive" ]
      ; elt "noscript"
          [ p [ strong [ text "Cette fonctionalité requiert du javascript." ] ] ]
      ; interactive_transcription
          ~id_textarea:"user-text"
          ~id_converted_text:"converted-text"
          ~initial_text:(`Body "Babar a une mémoire exceptionnelle, car c'est un éléph…")
      ]

  let email_link children = elt "a" ~attrs:["class", "mailelt"] children

  let submit_file ?replace_onchange ?label_attrs ?(attrs = []) f =
    let upload_css = [%string {|
    --progress: 0%;
    background: linear-gradient(to right,
                      white 0 var(--progress),
                      %{green_pale} var(--progress) 100%);
    border: 1.5px solid;
    border-radius: 4px;
    cursor: pointer;
    font-family: Graphik,-apple-system,system-ui,"Segoe UI",Roboto,Oxygen,Ubuntu,Cantarell,"Fira Sans","Droid Sans","Helvetica Neue",sans-serif;
    font-size: 1.10rem;
    padding: 6px 8px;
    margin-right: 0.5em;
    text-align: center;
    text-transform: none;
    width: fit-content;
    display: inline-block;
    |}] in
    elt "form"
      ~attrs:[ "action", "/conv"
             ; "method", "post"
             ; "enctype", "multipart/form-data"
             ; +attrs
             ]
      (f [ elt "label"
             ~cl:upload_css
             ?attrs:label_attrs
             [ leafelt "input"
                 ~cl:"display:none"
                 [ "type", "file"
                 ; "name", "file"
                 ; "accept", ".doc,.docx,.odt,.odp,.ppt,.pptx,.html,.xhtml,.htmlz,\
                              .epub,.txt,.mkd,.md"
                 ; +match replace_onchange with
                    | None -> [ "onchange", "form.submit()" ]
                    | Some l -> l
                 ]
             ; text "choisir un fichier"
             ]
         ; span ~cl:"font-size: 0.7em; "
             [ text "(ça ne marche pas ? "
             ; email_link [ text "dites-le nous" ]
             ; text ")"
             ]
         ])

  let image_list' l =
    let large_icon = "
    margin-right: calc(0.2 * min(5em, 15%));
    margin: 3px calc(0.2 * min(5em, 15%)) 3px 3px;
    float: left;
    width: calc(min(5em, 15%));
    height: auto;
  " in
    list' `ul
      ~cl:"
    list-style: none;
    display: flex;
    flex-flow: row wrap;
    gap: 0.5em;
    li {
        max-width: 25em;
        width: 100%;
    }
  "
      (List.map l ~f:(fun (attrs, img, desc) ->
         attrs, (img ~cl:large_icon :: desc)))

  let image_list l =
    image_list' (List.map l ~f:(fun (img, desc) -> ([], img, desc)))

  let format_acceptes () =
    image_list
      [ (fun ~cl ->
          img "/static/word.svg" ~cl
            [ "alt", "Microsoft Office Word (2019–present).svg"
            ; "width", "1881" (* can we go and compute this? *)
            ; "height", "1750"
        ])
      , [ text "Word et similaires (.doc, .docx, .odt)" ]
      ; (fun ~cl ->
          img "/static/powerpoint.svg" ~cl
            [ "alt", "Microsoft Office PowerPoint (2019–present).svg"
            ; "width", "512"
            ; "height", "476"
        ])
      , [ text "PowerPoint et similaires (.ppt, .pptx, .odp)" ]
      ; (fun ~cl ->
        img "/static/text_file.svg" ~cl
          [ "alt", "Text file icon"; "width", "822"; "height", "754" ])
      , [ text "fichiers textes (.txt, .mkd, .md)" ]
      ; (fun ~cl ->
        img "/static/pdf.svg" ~cl
          [ "alt", "PDF file icon"; "width", "544"; "height", "580" ])
      , [ text "Pas supporté directement. Soumettez soit le fichier dont est tiré le \
                PDF (préférable), ou un fichier Word recréé depuis le PDF (voir "
        ; a ~href:"https://www.adobe.com/acrobat/online/pdf-to-word.html"
            [ text "Adobe" ]
        ; text " ou "
        ; a ~href:"https://tools.pdf24.org/en/pdf-to-word"
            [ text "Pdf24" ]
        ; text ")."
        ]
      ; (fun ~cl ->
        img "/static/epub.svg" ~cl
          [ "alt", "Epub logo color.svg"; "width", "600"; "height", "800"])
      , [ text "livres numériques (.epub), comme ceux de "
        ; a ~href:"https://fr.wikisource.org/wiki/Wikisource:Accueil"
            [ text "wikisource" ]
        ; text " plus haut"
        ]
      ; (fun ~cl ->
        img "/static/internet_globe.svg" ~cl
          ["alt", "Internet Symbol"; "width", "407"; "height", "407" ])
      , [ text "sources de pages web (.html, .xhtml, .htmlz)" ]
      ]

  let section_transcription_en_ligne () =
    section
      [ h3 [ text "Transcription de documents, en ligne" ]
      ; submit_file Fn.id
      ; p [ text "Les formats acceptés sont :" ]
      ; format_acceptes ()
      ]

  let section_transcription_locale () =
    section
      [ h3 [ text "Transcription de documents, sur votre ordinateur" ]
      ; p [ text "Ce n'est utilisable que par les développeurs, \
                  actuellement. Voici un "
          ; a ~href:"/static/ortografe_cli.exe"
              ~attrs:["download", "ortografe-cli"]
              [ text "exécutable Linux natif" ]
          ; text ", ou "
          ; a ~href:"/static/ortografe.bc.js"
              ~attrs:["download", "ortografe-cli.js"]
              [ text "javascript" ]
          ; text " (nécessite Node.js) pour transcrire à la \
                  ligne de commande (pas d'interface graphique)."
          ]
      ]

  let icon = "height: 1em; width: auto;"
  let clickable_icon_round =
    "padding: 2px; border-radius:50%; box-shadow: 0 0 5px 4px #b9f4b9;"
  let clickable_icon_box =
    "padding: 2px; border-radius: 3px; box-shadow: 0 0 5px 4px #b9f4b9;"

  let section_transcription_pages () =
    let ext_in_chrome_link =
      "https://chromewebstore.google.com/detail/orthographe-rationnelle/jdicbfmgcajnpealjodkghahiakdafcl?hl=fr"
    in
    let ext_in_firefox_link =
      "https://addons.mozilla.org/fr/firefox/addon/orthographe-rationnelle/"
    in
    let ext_in_safari_link =
      "https://apps.apple.com/us/app/orthographe-rationnelle/id6482850164"
    in
    let firefox_on_android_link =
      "https://play.google.com/store/apps/details?id=org.mozilla.firefox"
    in
    let edge_instructions_link =
      "https://www.01net.com/astuces/comment-installer-des-extensions-google-chrome-sur-microsoft-edge-1844564.html"
    in
    section
      [ h3 [ text "Transcription des pages internet que vous visitez" ]
      ; (let src = "/static/screenshot-1280-800.png" in
         a ~href:src
           [ img src
               ~cl:"max-width:min(35em,100%); height:auto"
               [ "alt", "Capture d'écran de page Éléphant de Wikipédia en orthographe \
                         Érofa"
               ; "width", "1280"
               ; "height", "800"
               ]
        ])
      ; p [ text "Lisez internet en orthographe rationalisée avec une extension pour \
                  votre navigateur :" ]
      ; image_list'
          [ [ "class", "for-chrome" ]
          , (fun ~cl ->
              a ~href:ext_in_chrome_link
                [ img ~cl:(cl ^ clickable_icon_round) "/static/chrome.svg"
                    [ "alt", ""; "width", "48"; "height", "48" ]
                ])
          , [ text "Que sur ordinateur. Sur téléphone, Chrome ne supporte pas les \
                    extensions, nous vous conseillons donc "
            ; a ~href:firefox_on_android_link
                [ text "Firefox "
                ; img ~cl:icon "/static/firefox.svg"
                    [ "alt", ""; "width", "77"; "height", "79" ]
                ]
            ; text ". "
            ]
          ; [ "class", "for-firefox" ]
          , (fun ~cl ->
            a ~href:ext_in_firefox_link
                [ img ~cl:(cl ^ clickable_icon_round) "/static/firefox.svg"
                    [ "alt", ""; "width", "77"; "height", "79" ]
                ])
          , [ text "Ordinateur et téléphone." ]
          ; [ "class", "for-safari" ]
          , (fun ~cl ->
            a ~href:ext_in_safari_link
              [ img ~cl:(cl ^ clickable_icon_round) "/static/safari.svg"
                  [ "alt", ""; "width", "187"; "height", "186" ]
              ])
          , [ text "MacOS et iOS. Si l'extension n'a pas d'effet, vérifiez qu'il n'y a \
                    pas de triangle d'avertissement orange en haut près de la barre d'"
            ; a ~href:"/static/extension.zip"
                ~attrs:["style", "cursor:default; text-decoration:none; color:black; "]
                [ text "adresse" ]
            ; text "."
            ]
          ; [ "class", "for-edge" ]
          , (fun ~cl ->
            a ~href:ext_in_chrome_link
              [ img ~cl:(cl ^ clickable_icon_round) "/static/edge.svg"
                  [ "alt", ""; "width", "256"; "height", "256" ]
              ])
          , [ text "Ordinateur, et peut-être téléphone. Suivez "
            ; a ~href:edge_instructions_link
                [ text "les deux premiers points de ces instructions" ]
            ; text ", puis cliquez sur l'icône."
            ]
          ]
      ]

  let section_verificateurs () =
    let firefox_link =
      "https://addons.mozilla.org/fr/firefox/addon/corecteur-ortografe-simplifiee/"
    in
    let thunderbird_link =
      "https://addons.thunderbird.net/fr/thunderbird/addon/corecteur-ortografe-simpl-thun/"
    in
    let libreoffice_link =
      "https://extensions.openoffice.org/de/projectrelease/correction-en-orthographe-simplifiee-du-francais-100"
    in
    section
      [ h3 [ text "Vérificateurs d'orthographe" ]
      ; p [ text "Pour éviter les soulignages rouges intempestifs, nous fournissons des \
                  extensions pour que les outils suivants reconnaissent l'orthographe \
                  Érofa :" ]
      ; image_list'
          [ []
          , (fun ~cl ->
              a ~href:firefox_link
                [ img ~cl:(cl ^ clickable_icon_round) "/static/firefox.svg"
                    [ "alt", ""; "width", "77"; "height", "79" ]])
          , [ text "navigateur Firefox (non supporté sur téléphones)" ]
          ; []
          , (fun ~cl ->
            a ~href:thunderbird_link
                [ img ~cl:(cl ^ clickable_icon_round) "/static/thunderbird.svg"
                    [ "alt", ""; "width", "512"; "height", "512" ]])
          , [ text "messagerie Thunderbird" ]
          (* libreoffice pas encore prêt *)
          ; [ "style", "display:none" ]
          , (fun ~cl ->
            a ~href:libreoffice_link
              [ img ~cl:(cl ^ clickable_icon_box) "/static/libreoffice_writer.png"
                  [ "alt", ""; "width", "128"; "height", "128" ]])
          , [ text "suite bureautique LibreOffice" ]
          ]
      ]

  let section_claviers () =
    [ section
        [ h3 [ text "Clavier pour téléphone" ]
        ; p [ text "La plupart des claviers virtuels pour téléphone s'adaptent à votre \
                    orthographe : après avoir tapé les mots quelques fois en \
                    orthographe Érofa, ces orthographes vont seront proposés. Mais pour \
                    ne pas avoir à apprendre toute l'orthographe à votre clavier, et \
                    aussi pour que vous puissiez choisir quand écrire en Érofa, nous \
                    vous proposons un dictionnaire pour le clavier HeliBoard pour \
                    Android."
            ; details
                [ text "Instructions pour l'utiliser." ]
                [ list `ol
                    [ [ text "Installez "
                      ; a ~href:"https://f-droid.org/"
                          [ text "F-Droid" ]
                      ; text " depuis son site (pas disponible dans le Play \
                              Store). C'est un catalogue d'applications comme le Play \
                              Store."
                      ]
                    ; [ text "Installez "
                      ; a ~href:"https://f-droid.org/fr/packages/helium314.keyboard"
                          [ text "HeliBoard" ]
                      ; text " (le clavier virtuel), puis suivez ses instructions pour \
                              l'activer."
                      ]
                    ; [ a ~href:"/static/heliboard_erofa.dict"
                          [ text "Téléchargez" ]
                      ; text " le dictionnaire français Érofa pour HeliBoard."
                      ]
                    ; [ text "Utilisez ce dictionnaire :"
                      ; list `ol
                          [ [ text "Allez dans les paramètres d'HeliBoard" ]
                          ; [ text "-> « Langues & Dispositions »" ]
                          ; [ text "-> « français »" ]
                          ; [ text "-> « Dictionnaires + »"]
                          ; [ text "-> « Ajouter »" ]
                          ; [ text "tapez « erofa » dans la barre de recherche" ]
                          ; [ text "sélectionnez « heliboard_erofa.dict »" ]
                          ]
                      ]
                    ; [ text "Testez le clavier où vous voulez (par exemple Messages, \
                              WhatsApp, GMail)." ]
                    ; [ text "(optionnel) Pour éviter le soulignage rouge des mots en \
                              orthographe Érofa :"
                      ; list `ol
                          [ [ text "allez dans les « Paramètres » d'Android" ]
                          ; [ text "-> « Système »" ]
                          ; [ text "-> « Clavier »" ]
                          ; [ text "-> « Correcteur orthographique »" ]
                          ; [ text "-> « Correcteur par défaut »" ]
                          ; [ text "puis choisissez HeliBoard." ]
                          ]
                      ]
                    ]
                ; text "Pour repasser en orthographe usuelle, quand le clavier est \
                        visible, cliquez sur l'icône de clavier en bas à droite et \
                        sélectionnez votre ancien clavier (probablement Gboard). Et \
                        pour repasser en Érofa, même manipulation, mais sélectionnez \
                        HeliBoard."
                ]
            ]
        (* Avec gboard, le fait qu'on ne puisse pas supprimer les entrées de façon
           simple limite l'utilisation aux orthographes qu'on veut utiliser
           permanennement, ce n'est pas vraiment utilisable pour Érofa où au mieux un
           utilisateur voudrait passer d'une orthographe à l'autre. *)
        ; div ~attrs:[exp_hidden_class]
            [ list `ul [
              [ a ~href:"/static/gboard-1990-1000.zip"
                  [ text "Dictionnaire 1990 pour gboard" ] ]
                ; [ a ~href:"/static/gboard-erofa-1000.zip"
                      [ text "Dictionnaire Érofa pour gboard" ]
                  ]
                ; [ a ~href:"https://www.reddit.com/r/gboard/comments/z89a3f/how_to_empty_user_dictionary/" [ text "seule méthode pour supprimer les entrées :(" ] ]
                ]
            ]
        ]
    ; section
        [ h3 [ text "Clavier pour ordinateur" ]
        ; p [ text "Les accents sont plus fréquents avec l'orthographe Érofa. Pour les \
                    rares personnes qui utilisent un clavier QWERTY américain sans \
                    accès aux accents, sous Ubuntu nous vous suggérons la disposition \
                    de clavier « French (US) » (seule AltGr change, et permet d'écrire \
                    tous les signes du français, sans touches mortes)." ]
        ]
    ]

  let details__summary_unstyled ?open_ summary body =
    details ?open_
      ~summary_cl:"
&::marker { content: none; }
&::-webkit-details-marker { display: none; }
"
      summary
      body

  let section_autres_orthographes () =
    details__summary_unstyled (* ~open_:true *)
      [ h3 [ text "Support pour d'autres orthographes"
           ; span ~cl:"text-decoration-line: underline" [ text "…" ]
          ]
      ]
      [ p [ text "Parmi nos outils, voici ceux qui supportent d'autres orthographes \
                  que l'orthographe Érofa:" ]
      ; list' `ul
          (let works = [ "style", "list-style-type:\"\\2705\u{A0}\"" ] in
           let doesnt_work = [ "style", "list-style-type:\"\\274C\u{A0}\"" ] in
           [ doesnt_work, [ text "Dictionnaire de transcription" ]
           ; works, [ text "transcription interactive (voir plus bas)" ]
           ; works, [ text "transcription de documents, en ligne (voir plus bas)" ]
           ; works, [ text "transcription de documents, sur votre ordinateur (voir "
                    ; code [ text "--help" ]
                    ; text ")"
                    ]
           ; works, [ text "transcription de pages que vous visitez (voir les options \
                            de l'extension)" ]
           ; doesnt_work, [ text "vérificateurs d'orthographe" ]
           ; doesnt_work, [ text "clavier pour téléphone" ]
           ; works, [ text "données (voir plus bas)" ]
           ]
          )
      ; p [ text "Notez que ces autres orthographes ne sont pas des recommendations \
                  (ni des futures recommendations), simplement des possibilités \
                  d'expérimentation." ]
       ; submit_file (fun button ->
             [ +(Dict_gen_common.Dict_gen.all_selection_html
                   ~url_prefix:"/static/"
                   ~name_prefix:""
                   ~id_prefix:"conv-"
                   ~checked:(fun r ->
                     String.(=) (Dict_gen_common.Dict_gen.name r) "1990")
                   ()
                 |> nodes_of_string)
             ; div
                 [ text "Autres "
                 ; a ~attrs:[ "style", "background-color: #1e90ff; border-radius: 50%; color: white; font-weight: bold; text-align:center; display: inline-block; width: 1.3em; height: 1.3em; text-decoration: none;"
                            ; "target", "_blank" ]
                     ~href:"/static/dict-format.html"
                     [ text "?" ]
                 ; text " : "
                 ; span
                     ~cl:"cursor: pointer;"
                     ~attrs:[ "id", "form-conv-dict-deselect"
                            ; "style", "display: none"
                            ; "onclick", "const elt = document.getElementById('form-conv-dict'); elt.value = null; elt.dispatchEvent(new Event('change', { bubbles: true }));"
                               ]
                     [ text "\u{2BBE} " ]
                 ; elt "label"
                     [ leafelt "input"
                         [ "id", "form-conv-dict"
                         ; "type", "file"
                         ; "accept", ".txt,.csv,.dict"
                         ]
                     ]
                 ]
             ; br
             ; div ~attrs:["id", "form-conv-button-error"] []
             ; +button
           ])
           ~label_attrs:["id", "form-conv-label"]
           ~replace_onchange:["id", "form-conv-button"]
           ~attrs:["id", "form-conv"]
       ; br
       ; interactive_transcription
           ~id_textarea:"user-text2"
           ~id_converted_text:"converted-text2"
           ~initial_text:(`Placeholder "Tapez le texte à transcrire ici.")
       ; p
           [ text "Données : "
           ; a ~href:"dict.csv"
               ~attrs:[ "id", "download-dict" ]
               ~cl:[%string "&.loading {
                               border-radius: 6px;
                               background: linear-gradient(to right,
                                 white 0 var(--progress),
                                 %{green_pale} var(--progress) 100%);
                               border: 2px solid %{green};
                               padding: 0 3px 3px 3px;
                             }"]
               [ text "csv" ]
           ; text " contenant l'orthographe avant/après."
           ]
       ]

  let section_donnees () =
    section
      [ h3 [ text "Données" ]
      ; p [ text "Nous fournissons des données brutes qui décrivent le passage de \
                  l'orthographe usuelle à d'autres orthographes :" ]
      ; list `ul ~cl:"list-style: none"
          [ [ a ~href:"/static/erofa.csv" ~attrs:["download", "erofa.csv"]
                [ text "csv Érofa" ]
            ; text " contenant l'orthographe avant/après, ce qui inclut l'essentiel des rectifications de 1990"
            ]
          ; [ a ~href:"/static/rect1990.csv" ~attrs:["download", "rect1990.csv"]
                [ text "csv rectifications de 1990" ]
            ; text ", pareil mais seulement les rectifications de 1990 "
            ]
          ]
      ]

  let section_aller_plus_loin () =
    let hr = hr ~cl:"width: 70%; border: 0.1px solid #f4f4f4; margin: 0.8em;" () in
    section
      ~attrs:[aller_plus_loin_def]
      [ h2 [ text "Aller plus loin" ]
      ; list `ul ~cl:"list-style-type: none; padding-left: 0;"
          [ [ p ~cl:"margin-top: 0; margin-bottom: 0.2em"
                [ text "Pour plus d'information sur l'orthographe proposée (très \
                        lisible sans être expert) :" ]
            ; list `ul
                [ [ a ~href:"/static/erofa-texte.pdf#page=47"
                      [ text "Les consonnes doubles" ]
                  ; text " (sept pages)"
                  ]
                ; [ a ~href:"/static/erofa-texte.pdf#page=55"
                      [ text "Les x finaux" ]
                  ; text " (quatre pages)"
                  ]
                ; [ a ~href:"/static/erofa-texte.pdf#page=59"
                      [ text "Les lettres grecques et similaires" ]
                  ; text " (onze pages)"
                  ]
                ; [ a ~href:"/static/erofa-texte.pdf#page=17"
                      [ text "Explications de l'histoire de l'orthographe et pourquoi cette réforme" ]
                  ; text " (26 pages)"
                  ]
                ; [ a ~href:"http://www.lambert-lucas.com/wp-content/uploads/2022/11/OA-dictionnaire-EROFA.pdf"
                      [ text "Tous les articles précédents, plus une liste des mots modifiés" ]
                  ]
                ]
            ; hr
            ]
          ; [ a ~cl:"&:visited, &:link { text-decoration-line: none; }
                     border-radius: 4px;
                     background-color: #e5fbe5;
                     border: 1.5px solid;
                     padding: 1px 4px;"
                ~href:"http://erofa.free.fr/index.php?option=com_content&view=article&id=56&Itemid=67"
                [ text "Rejoignez" ]
            ; text " l'"
            ; a ~href:"http://erofa.free.fr/"
                [ text "association Érofa" ]
            ; text " qui propose cette réforme !"
            ; hr
            ]
          ; [ text "Le livre numérique gratuit "
            ; a ~href:"/static/orthographe-qui-a-peur-de-la-reforme.epub"
                [ cite [ text "Orthographe : qui a peur de la réforme ?" ] ]
            ; text " (de Marie-Louise Moreau et Georges Legros) explique un tas de \
                    choses sur l'orthographe : pourquoi l'orthographe est comme elle \
                    est, comment elle a évolué dans le passé, les idées fausses qu'on \
                    s'en fait, les craintes que génèrent une réforme, etc."
            ; hr
            ]
          ; [ p ~cl:"margin-top: 0; margin-bottom: 0.2em" [ text "Sur le site et les outils :" ]
            ; list `ul
                [ [ email_link [ text "Contactez-nous" ]
                  ; text " si vous avez des problèmes, questions ou suggestions !"
                  ]
                ; [ details
                      [ text "Activité" ]
                      [ list `ul
                          [ [ text "2024-06 : support pour écrire en Érofa avec les \
                                    claviers de téléphones, support de transcription \
                                    avec des dictionnaires personnalisés." ]
                          ; [ text "À ce stade, les outils sont essentiellement finis." ]
                          ; [ text "2024-05 : support de transcriptions de document \
                                    côté client, amélioration de l'apparence du site" ]
                          ; [ text "2024-01 - 2024-04 : support des rectifications de \
                                    1990, ajout de la transcription de documents, \
                                    extension pour Safari, support d'orthographes \
                                    autres que l'orthographe Érofa dans le site et dans \
                                    les extensions, impression d'un livre classique en \
                                    orthographe Érofa (Alice au pays des merveilles)." ]
                          ; [ text "2023-10 - 2024-01 : extension pour Chrome, la \
                                    transcription inclut maintenant les mots dérivés \
                                    comme les conjugaisons (grâce l'application \
                                    informatisée des règles Érofa à un lexique), \
                                    création du site avec la transcription interactive" ]
                          ; [ text "2023-10 : extension pour Firefox" ]
                          ]
                      ]
                  ]
                ; [ details
                      [ text "Licences et mentions légales" ]
                      [ div ~cl:"padding-left: 2em"
                          [ p [ text "Les outils de cette page utilisent des dictionnaires \
                                      construits à partir d'Érofa ("
                              ; a ~href:"http://erofa.free.fr/index.php?option=com_content&view=article&id=59&Itemid=68"
                                  [ text "utilisation non-commerciale" ]
                              ; text "), de "
                              ; a ~href:"http://www.lexique.org"
                                  [ text "Lexique" ]
                              ; text " (la licence dit utilisation non-commerciale, mais les \
                                      auteurs permettent l'utilisation dans ce site), du "
                              ; a ~href:"https://dumps.wikimedia.org/legal.html"
                                  [ text "wiktionnaire" ]
                              ; text " (toute utilisation)."
                              ]
                          ; p [ text "Vous pouvez retrouver l'origine des icônes utilisées dans \
                                      cette page sur la page wikipédia dudit programme ou \
                                      format. Les quelques cas plus dures sont "
                              ; a ~href:"https://commons.wikimedia.org/wiki/File:Text-txt.svg"
                                  [ text "fichier texte" ]
                              ; text ", "
                              ; a ~href:"https://commons.wikimedia.org/wiki/File:Pdf_by_mimooh.svg"
                                  [text "pdf" ]
                              ; text ", "
                              ; a ~href:"https://en.wikipedia.org/wiki/File:Crystal_Clear_app_linneighborhood.svg"
                                  [text "globe internet"]
                              ; text ". Aucunes n'ont été modifiées."
                              ]
                          ; p [ text "Le favicon provient de "
                              ; a ~href:"https://icons8.com/icon/hGBTBdUownyO/quill-pen" [ text "icons8" ]
                              ; text "."
                              ]
                          ]
                      ]
                  ]
                ; [ text "Pour les développeurs, voici le "
                  ; a ~href:"https://github.com/v-gb/ortografe"
                      [ text "code source" ]
                  ; text " de ce site et de ses outils."
                  ]
                ; [a ~href:"https://hal.science/hal-04579389"
                     [ text "Article scientifique" ]
                  ; text " sur une partie du travail ci-dessus au "
                  ; a ~href:"https://cmlf2024.sciencesconf.org/"
                      [ text "CMLF 2024" ]
                  ; text "."
                  ]
                ]
            ]
          ]
      ]

  let head () =
    head
      ~root:true
      ~title:"Orthographe rationnelle"
      ~description:"Outils pour utiliser l'orthographe rationalisée du \
                    français Érofa. Ou d'autres orthographes, comme celle \
                    des rectifications de 1990."
      ~attrs:[ script "/static/dict.js" ~defer:true
             ; script "/static/rewrite.js" ~defer:true
             ; script "/static/page.js" ~defer:true
             ]
      ()

  let main () : node =
    html
      ~head:(head ())
      ~body_style:"margin: 0; font-size: 1.1rem; line-height: 1.3;"
      ~body:
      [ navbar ()
      ; elt "main" ~cl:"max-width: 55em; margin: auto;"
          [ div ~cl:"margin: 0 8px 8px 8px;"
              [ h1 ~cl:"text-align:center;"
                  [ text "Une orthographe rationnelle"
                  ; br
                  ; text "ici et maintenant"
                  ]
              ; +introduction ()
              ; section_regles ()
              ; section ~attrs:[outils_def]
                  [ h2 [ text "Outils de mise en pratique" ]
                  ; p ~cl:"margin-top:0"
                      [ text "Pour rédiger de nouveaux textes en cette orthographe, \
                              vous trouverez ici un vérificateur d'orthographe, un \
                              clavier pour télephone, et un dictionnaire pour consulter \
                              l'orthographe de n'importe quel mot." ]
                  ; p ~cl:"margin-top:0"
                      [ text "Pour transcrire des textes existants, vous pourrez \
                              soumettre plus bas du texte, des documents, ou récupérer \
                              un outil pour transcrire les pages internet à la volée." ]
                  ]
              ; section_dictionnaire ()
              ; section_transcription_interactive ()
              ; section_transcription_en_ligne ()
              ; section_transcription_locale ()
              ; section_transcription_pages ()
              ; section_verificateurs ()
              ; +section_claviers ()
              ; section_donnees ()
              ; section_autres_orthographes ()
              ; section_aller_plus_loin ()
              ]
          ]
      ]
      ()
end

module Regles_alfonic = struct
  let main () : node =
    let rules_set = Set.of_list (module String) [ "alfonic" ] in
    let rules =
      force Dict_gen_common.Dict_gen.all_builtin
      |> List.filter ~f:(fun rule ->
           Set.mem rules_set (Dict_gen_common.Dict_gen.name rule))
    in
    assert (List.length rules = 1);
    let rules_str =
      List.map rules ~f:Dict_gen_common.Dict_gen.name
      |> String.concat ~sep:" "
    in
    html
      ~head:(head
               ~root:false
               ~title:("Orthographe rationnelle -- " ^ rules_str)
               ~description:("Outils de conversion en " ^ rules_str)
               ~attrs:[ script "/static/rewrite.js" ~defer:true
                      ; script "/static/page.js" ~defer:true
                      ]
               ())
      ~body_style:"margin: 0; font-size: 1.1rem; line-height: 1.3;"
      ~body:
      [ navbar
          [ "/", [ text "Accueil" ] ]
      ; elt "main" ~cl:"max-width: 55em; margin: auto;"
          [ div ~cl:"margin: 0 8px 8px 8px;"
              [ h1 ~cl:"text-align:center; margin-top: 1em; margin-bottom: 1em;"
                  [ text "Orthographe rationnelle" ]
              ; p [ text "Les outils de cette page appliquent les règles :" ]
              ; list `ul
                  (List.map (if false then [] else rules) ~f:(fun rule ->
                       [ strong [ text (Dict_gen_common.Dict_gen.name rule) ]
                       ; text ", "
                       ; +nodes_of_string (Dict_gen_common.Dict_gen.html_doc rule)
                       ; br
                       ; +match Dict_gen_common.Dict_gen.problems rule with
                          | [] -> []
                          | _ :: _ as problems ->
                             [ text "Problèmes connus :"
                             ; list `ul (List.map problems ~f:(fun s -> [ text s ]))
                             ]
                  ]))
              ; section
                  [ h3 [ text "Transcription interactive" ]
                  ; Index.interactive_transcription
                      ~id_textarea:"user-text2"
                      ~id_converted_text:"converted-text2"
                      ~initial_text:(`Placeholder "Tapez le texte à transcrire ici.")
                  ]
              ; section
                  [ h3 [ text "Transcription de documents, en ligne" ]
                  ; Index.submit_file (fun button ->
                        [ div ~cl:"display: none"
                            (Dict_gen_common.Dict_gen.all_selection_html
                               ~url_prefix:"/static/"
                               ~name_prefix:""
                               ~id_prefix:"conv-"
                               ~checked:(fun r ->
                                 Set.mem rules_set (Dict_gen_common.Dict_gen.name r))
                               ()
                             |> nodes_of_string)
                        ; +button
                      ])
                  ; p [ text "Les formats acceptés sont :" ]
                  ; Index.format_acceptes ()
                  ]
              ]
          ]
      ]
      ()
end

let () =
  List.iter
    [ "index.html", Index.main ()
    ; "regles_alfonic.html", Regles_alfonic.main ()
    ]
    ~f:(fun (filename, node) ->
      Out_channel.write_all
        filename
        ~data:(
          "<!DOCTYPE html>\n" ^
            (node
             |> Markup.from_tree __.html
             |> Markup.pretty_print
             |> Markup.write_html
             |> Markup.to_string)))
