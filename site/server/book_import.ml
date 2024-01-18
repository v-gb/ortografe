module Stdlib_sys = Sys
module Z = Unix
open Core
module Unix = Z

let which_source ~url =
  match
    Uri.host url
    |> Core.Option.value_exn
    |> Core.String.split ~on:'.'
    |> List.rev
  with
  | _ :: "wikisource" :: _ -> `Wikisource
  | _ :: "gutenberg" :: _ -> `Gutenberg
  | _ -> failwith ("unknown source site for " ^ Uri.to_string url)

let suf = function
  | `Wikisource -> ".epub"
  | `Gutenberg -> ".zip"

let dl_path ~root ~title ~source =
  root ^/ "dl.gen" ^/ (title ^ suf source)
let conv_path ~root ~title ~source:_ =
  root ^/ "site" ^/ "static" ^/ "books" ^/ title

let directory ~root = root ^/ "dl-conv.gen"
let mkdir_and_write_all path ~data =
  if not (Stdlib_sys.file_exists (Filename.dirname path))
  then Unix.mkdir (Filename.dirname path) 0o777;
  Out_channel.write_all path ~data

let download_from_wikisource url =
  (* url looks like "https://fr.wikisource.org/wiki/Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
     epub url looks like "https://ws-export.wmcloud.org/?format=epub&lang=fr&page=Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
   *)
  let title = String.chop_prefix_exn ~prefix:"/wiki/" (Uri.path url) in
  Hyper.get ("https://ws-export.wmcloud.org/?format=epub&lang=fr&page=" ^ title)

let download_from_gutenberg url =
  let id = Filename.basename (Uri.path url) in
  Hyper.get [%string "https://www.gutenberg.org/cache/epub/%{id}/pg%{id}-h.zip"]

let download url ~root ~title =
  let url = Uri.of_string url in
  let source = which_source ~url in
  let path = dl_path ~root ~title ~source in
  if not (Sys_unix.file_exists_exn path) then (
    (* make it easier to do queries one by one, because they throttle so much *)
    let data =
      match source with
      | `Wikisource -> download_from_wikisource url
      | `Gutenberg -> download_from_gutenberg url
    in
    mkdir_and_write_all (dl_path ~root ~title ~source) ~data;
    Printf.eprintf "waiting\n%!";
    (* wikisource seem to be very aggressive about returning "429 Too Many Requests". Even 10s
       of pause is not enough. *)
    Unix.sleepf 60.;
  )

let extract_zip ~data ~dst =
  let tmp = Filename.dirname dst ^ ".tmp" in
  mkdir_and_write_all tmp ~data;
  Sys_unix.command_exn [%string {|
mkdir -p %{Sys.quote dst} && unzip -q %{Sys.quote tmp} -d %{Sys.quote dst} && rm -f %{Sys.quote tmp}
|}]

let make_the_html_mobile_friendly html =
  String.substr_replace_first
    html
    ~pattern:"<head>"
    ~with_:{|<head><meta name="viewport" content="width=device-width, initial-scale=1"/>|}

let options = lazy { Ortografe.convert_uppercase = true; dict = Lazy.force Ortografe.erofa }
let convert_wikisource epub ~dst =
   (* Sometimes you see headings that are uppercase in the source, instead by using
      text-transform or font-variant, so just convert those. *)
  let epub = Ortografe.epub epub ~dst:String ~options:(Lazy.force options) in
  let epub_conv =
    Ortografe.map_zip epub
      (fun member _file contents ->
        (* There's missing CSS in the wikisource epubs (or maybe it's intended that the
           ereader would provide its own). So add CSS settings to make the look of the
           converted xhtml and the wikisource html similar. *)
        match Filename.basename (Zipc.Member.path member) with
        | f when String.is_suffix f ~suffix:".html"
                 || String.is_suffix f ~suffix:".xhtml"
          -> Some (make_the_html_mobile_friendly (contents ()))
        | "main.css" ->
           Some (contents () ^ {|
body {
  max-width: min(90%, 36em);
  margin: 0 auto;
  font-size:0.875em;
  line-height:1.6;
  font-family: sans-serif;
}
p {
  text-indent: 2em;
  line-height:1.6;
  margin-top: 7px;
  margin-bottom:7px;
}
|})
        | _ -> None)
  in
  extract_zip ~data:epub_conv ~dst;
  epub_conv

let convert_gutenberg zip ~dst =
  let data = Ortografe.htmlz zip ~dst:String ~options:(Lazy.force options) in
  let new_data =
    Ortografe.map_zip data
      (fun member _file contents ->
        (* There's missing CSS in the wikisource epubs (or maybe it's intended that the
           ereader would provide its own). So add CSS settings to make the look of the
           converted xhtml and the wikisource html similar. *)
        match Filename.basename (Zipc.Member.path member) with
        | f when String.is_suffix f ~suffix:".html"
                 || String.is_suffix f ~suffix:".xhtml"
          -> Some (make_the_html_mobile_friendly (contents ()))
        | _ -> None)
  in
  extract_zip ~data:new_data ~dst;
  new_data

let convert url ~root ~title =
  let url = Uri.of_string url in
  let source = which_source ~url in
  let data =
    try In_channel.read_all (dl_path ~root ~title ~source)
    with e -> raise_s [%sexp (e : exn)
                     , "hint: you might need to run "
                     , (Sys.concat_quoted [ (Sys.get_argv ()).(0); "download-all" ] : string)]
  in
  let dst = conv_path ~root ~title ~source in
  match source with
  | `Wikisource -> convert_wikisource data ~dst
  | `Gutenberg -> convert_gutenberg data ~dst

let books =
  (* Dracula would be nice, but it will be in the public domain in 2026, I think
     https://fr.wikisource.org/wiki/Dracula#I._Journal_de_Jonathan_Harker_(St%C3%A9nographi%C3%A9)

     For wikisources, the licence seems to always be
     https://creativecommons.org/licenses/by-sa/4.0/deed.fr (pointed at in the footer)
     and the wiki page is https://foundation.wikimedia.org/wiki/Policy:Terms_of_Use, so
     I think it's enough to say "original from wikisource" with a link to the original.

     For project gutenberg, I think no obligations to even link, but I'll do that anyway.
     https://www.gutenberg.org/policy/permission.html
   *)
  [ "Pensées", "Marc Aurèle", "https://fr.wikisource.org/wiki/Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
  ; "Le Misanthrope", "Molière", "https://fr.wikisource.org/wiki/Le_Misanthrope/%C3%89dition_Louandre,_1910/Acte_I"
  ; "Alice au pays des merveilles", "Lewis Carroll", "https://fr.wikisource.org/wiki/Alice_au_pays_des_merveilles/1"
  ; "Poil de Carotte", "Jules Renard", "https://fr.wikisource.org/wiki/Poil_de_Carotte/01"
  ; "Les Fleurs du mal", "Charles Baudelaire", "https://www.gutenberg.org/ebooks/6099"
  ; "Fables, La cigale et la fourmi", "Jean de La Fontaine", "https://fr.wikisource.org/wiki/Fables_de_La_Fontaine_(%C3%A9d._1874)/La_Cigale_et_la_Fourmi"
  ; "Fables, Le corbeau et le renard", "Jean de La Fontaine", "https://fr.wikisource.org/wiki/Fables_de_La_Fontaine_(%C3%A9d._1874)/Le_Corbeau_et_le_Renard"
  ; "Les Voyages de Gulliver", "Jonathan Swift", "https://fr.wikisource.org/wiki/Les_Voyages_de_Gulliver/Voyage_%C3%A0_Lilliput/I"
  ; "Les Misérables", "Victor Hugo", "https://fr.wikisource.org/wiki/Les_Mis%C3%A9rables/Tome_1/Livre_1/01"
  ; "Cyrano de Bergerac", "Edmond Rostand", "https://fr.wikisource.org/wiki/Cyrano_de_Bergerac_(Rostand)/Acte_I"
  ; "Madame Bovary", "Gustave Flaubert", "https://fr.wikisource.org/wiki/Madame_Bovary/Premi%C3%A8re_partie/1"
  ; "Candide, ou l'Optimisme", "Voltaire", "https://fr.wikisource.org/wiki/Candide,_ou_l%E2%80%99Optimisme/Garnier_1877/Chapitre_1"
  ; "Le Comte de Monte-Cristo", "Alexandre Dumas", "https://fr.wikisource.org/wiki/Le_Comte_de_Monte-Cristo/Chapitre_1"
  ; "Vingt mille lieues sous les mers", "Jules Verne", "https://fr.wikisource.org/wiki/Vingt_mille_lieues_sous_les_mers/Partie_1/Chapitre_1"
  ]

let download_all ~root =
  List.iter books ~f:(fun (title, _author, url) ->
    Printf.eprintf "importing %s\n%!" title;
    download ~root ~title url;
  )

let guess_main_file ~url ~data =
  let files =
    let files = Queue.create () in
    ignore (
        Ortografe.map_zip data (fun member _file _contents ->
            Queue.enqueue files (Zipc.Member.path member);
            None) : string);
    Queue.to_list files
  in
  match
    List.filter files ~f:(fun f ->
        let prefix, ext = Filename.split_extension f in
        match Filename.basename prefix, ext with
        | ("nav" | "title" | "about"), _ -> false
        | _, Some ("html" | "xhtml") -> true
        | _ -> false)
  with
  | [f] -> f
  | files -> raise_s [%sexp "can't find main .html file for",
                  (url : string),
                  ~~(files : string list)]

let html_li ~url ~author ~title ~main_file =
  let rel_url = "/static/books" ^/ title ^/ main_file in
  let de =
    match (String.lowercase author).[0] with
    | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> "d'"
    | _ -> "de "
  in
  [%string {|<li><cite><a href="%{rel_url}">%{title}</a></cite> %{de}%{author} (voir le <a href="%{url}">texte initial</a>)</li>|}]
  ^ "\n"

let convert_all ~root =
  Sys_unix.command_exn [%string "rm -rf %{Sys.quote (directory ~root)}"];
  let lis =
    List.map books ~f:(fun (title, author, url) ->
        let new_data = convert ~root ~title url in
        let main_file = guess_main_file ~url ~data:new_data in
        html_li ~url ~author ~title ~main_file
      )
  in
  let html_fragment =
    String.concat_lines
      (List.concat
         [ [ "<ul class=books>" ]
         ; lis
         ; [ "</ul>"]
         ])
  in
  Out_channel.write_all
    (root ^/ "site" ^/ "static" ^/ "books.html")
    ~data:html_fragment
