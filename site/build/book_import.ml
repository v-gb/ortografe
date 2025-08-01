module Stdlib_sys = Sys
module Z = Unix
open Core
module Unix = Z

let delete_dir s = Sys_unix.command_exn [%string "rm -rf -- %{Sys.quote s}"]

let with_tempdir ?temp_dir a b f =
  let dir = Stdlib.Filename.temp_dir ?temp_dir a b in
  Exn.protect ~finally:(fun () -> delete_dir dir) ~f:(fun () -> f dir)

let which_source ~url =
  match
    Uri.host url ||? failwith (Uri.to_string url) |> String.split ~on:'.' |> List.rev
  with
  | _ :: "wikisource" :: _ -> `Wikisource
  | _ :: "gutenberg" :: _ -> `Gutenberg
  | _ -> failwith ("unknown source site for " ^ Uri.to_string url)

let suf = function `Wikisource -> ".epub" | `Gutenberg -> ".zip"
let dl_path ~books_tmp ~title ~source = books_tmp ^/ title ^ suf source
let conv_path ~title = "books" ^/ title
let conv_basedir = "books"

let mkdir_and_write_all path ~data =
  if not (Stdlib_sys.file_exists (Filename.dirname path))
  then Unix.mkdir (Filename.dirname path) 0o777;
  Out_channel.write_all path ~data

let http_download url =
  Exn.protectx (Filename_unix.temp_file "ortografe-http" "") ~finally:Sys_unix.remove
    ~f:(fun tmp_name ->
      Sys_unix.command_exn
        (Sys.concat_quoted [ "curl"; "--fail-with-body"; "-sS"; "--"; url ]
        ^ " > "
        ^ Sys.quote tmp_name);
      In_channel.read_all tmp_name)

let download_from_wikisource url =
  (* url looks like "https://fr.wikisource.org/wiki/Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
     epub url looks like "https://ws-export.wmcloud.org/?format=epub&lang=fr&page=Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
  *)
  let title = String.chop_prefix_exn ~prefix:"/wiki/" (Uri.path url) in
  http_download ("https://ws-export.wmcloud.org/?format=epub&lang=fr&page=" ^ title)

let download_from_gutenberg url =
  let id = Filename.basename (Uri.path url) in
  http_download [%string "https://www.gutenberg.org/cache/epub/%{id}/pg%{id}-h.zip"]

let download ~before url ~books_tmp ~title =
  let url = Uri.of_string url in
  let source = which_source ~url in
  let path = dl_path ~books_tmp ~title ~source in
  if not (Sys_unix.file_exists_exn path)
  then (
    (* make it easier to do queries one by one, because they throttle so much *)
    before ();
    let data =
      match source with
      | `Wikisource -> download_from_wikisource url
      | `Gutenberg -> download_from_gutenberg url
    in
    mkdir_and_write_all (dl_path ~books_tmp ~title ~source) ~data)

let extract_zip ~data ~dst =
  let tmp = Filename.dirname dst ^ ".tmp" in
  mkdir_and_write_all tmp ~data;
  Sys_unix.command_exn
    [%string
      {|
mkdir -p %{Sys.quote dst} && unzip -q %{Sys.quote tmp} -d %{Sys.quote dst} && rm -f %{Sys.quote tmp}
|}]

let make_the_html_mobile_friendly =
  let re = lazy Re.(compile (seq [ str "<body"; shortest (rep any); str ">" ])) in
  fun ~url html ->
    let html =
      String.substr_replace_first html ~pattern:"<head>"
        ~with_:
          {|<head><meta name="viewport" content="width=device-width, initial-scale=1"/>|}
    in
    Re.replace (force re) html ~f:(fun group ->
        let source =
          match which_source ~url:(Uri.of_string url) with
          | `Wikisource -> "de wikisource"
          | `Gutenberg -> "du projet Gutenberg"
        in
        Re.Group.get group 0
        ^ [%string {|<h3>voir le <a href="%{url}">texte original</a> %{source}</h3>|}])

let options =
  lazy
    { Ortografe.convert_uppercase = true
    ; dict =
        (let dict = Lazy.force Ortografe_embedded.erofa in
         function
         (* modernize archaisms in frankenstein, as the edition is quite old
            and people could get confused and think we made a mistake, or Érofa
            is removing silent t's *)
         | "savans" -> Some "savants"
         | "momens" -> Some "moments"
         | "instrumens" -> Some "instruments"
         | "étudians" -> Some "étudiants"
         | "suffisans" -> Some "sufisants"
         | "sentimens" -> Some "sentiments"
         | "excellens" -> Some "excélents"
         | "appartemens" -> Some "apartements"
         (* Same reasoning, but for les fleurs du mal. gutenberg.org doesn't seeem to
            provide access to the scanned texts, but we can compare to:
            - https://fleursdumal.org/alphabetical-listing
            - https://fr.wikisource.org/wiki/Livre:Baudelaire_Les_Fleurs_du_Mal.djvu
          *)
         | "détonneraient" -> Some "détoneraient"
         | "clef" -> Some "clé"
         | "soûlerai" -> Some "soulerai"
         | "soûleront" -> Some "souleront"
         | "ennuîrai" -> Some "ennuierai"
         | "mimoir" -> Some "miroir" (* typo *)
         | "étranches" -> Some "étranges" (* typo *)
         | "provoquants" -> Some "provocants" (* adjectif ici, pas participe présent *)
         | "gît" -> Some "git"
         | "OFFENSEE" -> Some "OFENSEE"
         | "tonneras" -> Some "toneras"
         | "tettent" -> Some "tètent"
         | "allaita" -> Some "alaita"
         | "dor" -> Some "d'or" (* typo *)
         | "dors" -> Some ""
         | "eperonnant" -> Some "éperonant"
         | "refraîchir" -> Some "rafraichir"
         | "buttant" -> Some "butant"
         | "sompteux" -> Some "somptueus" (* typo *)
         | "arome" -> Some "arôme"
         | "enveloppait.--Eh" ->
             Some "envelopait.--Eh" (* bad word break due to missing spaces *)
         | "Etonnants" -> Some "Etonants"
         | "remercîment" -> Some "remerciement"
         | "ressasiera" -> Some "rassasiera" (* typo *)
         | "refraîchira" -> Some "rafraichira" (* typo *)
         | "choif" -> Some "soif" (* typo *)
         (* Same, Le misantrope *)
         | "reçoi" -> Some "reçois"
         | s -> Stdlib.Hashtbl.find_opt dict s)
    ; interleaved = true
    ; plurals_in_s = Some "s"
    }

let convert_wikisource epub ~url ~dst =
  (* Sometimes you see headings that are uppercase in the source, instead by using
     text-transform or font-variant, so just convert those. *)
  let epub = Ortografe.epub epub ~dst:String ~options:(Lazy.force options) in
  let epub_conv =
    Ortografe.map_zip epub (fun ~path ->
        (* There's missing CSS in the wikisource epubs (or maybe it's intended that the
           ereader would provide its own). So add CSS settings to make the look of the
           converted xhtml and the wikisource html similar. *)
        match Filename.basename path with
        | f
          when String.is_suffix f ~suffix:".html" || String.is_suffix f ~suffix:".xhtml"
          ->
            Some (fun ~contents -> make_the_html_mobile_friendly ~url contents)
        | "main.css" ->
            Some
              (fun ~contents ->
                contents
                ^ {|
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

let convert_gutenberg zip ~url ~dst =
  let data = Ortografe.htmlz zip ~dst:String ~options:(Lazy.force options) in
  let new_data =
    Ortografe.map_zip data (fun ~path ->
        (* There's missing CSS in the wikisource epubs (or maybe it's intended that the
           ereader would provide its own). So add CSS settings to make the look of the
           converted xhtml and the wikisource html similar. *)
        match Filename.basename path with
        | f
          when String.is_suffix f ~suffix:".html" || String.is_suffix f ~suffix:".xhtml"
          ->
            Some (fun ~contents -> make_the_html_mobile_friendly ~url contents)
        | _ -> None)
  in
  extract_zip ~data:new_data ~dst;
  new_data

let convert url ~books_tmp ~title =
  let source = which_source ~url:(Uri.of_string url) in
  let data =
    try In_channel.read_all (dl_path ~books_tmp ~title ~source)
    with e ->
      raise_s
        [%sexp
          (e : exn)
        , "hint: you may need to run "
        , (Sys.concat_quoted [ "_build/default/site/build/build.exe"; "download-all" ]
            : string)]
  in
  let dst = conv_path ~title in
  match source with
  | `Wikisource -> convert_wikisource data ~url ~dst
  | `Gutenberg -> convert_gutenberg data ~url ~dst

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
  [ ( "Pensées"
    , None
    , "de Marc Aurèle"
    , "https://fr.wikisource.org/wiki/Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01" )
  ; ( "Le Misanthrope"
    , None
    , "de Molière"
    , "https://fr.wikisource.org/wiki/Le_Misanthrope/%C3%89dition_Louandre,_1910/Acte_I"
    )
  ; ( "Alice au pays des merveilles"
    , None
    , "de Lewis Carroll"
    , "https://fr.wikisource.org/wiki/Alice_au_pays_des_merveilles/1" )
  ; ( "Le Chat botté"
    , None
    , "de Charles Perrault"
    , "https://fr.wikisource.org/wiki/Contes_de_Perrault_(%C3%A9d._1902)/Le_ma%C3%AEtre_Chat_ou_le_Chat_bott%C3%A9"
    )
  ; ( "Les Fleurs du mal"
    , None
    , "de Charles Baudelaire"
    , "https://www.gutenberg.org/ebooks/6099" )
  ; ( "Fables, La cigale et la fourmi"
    , Some "La cigale et la fourmi"
    , "de Jean de La Fontaine"
    , "https://fr.wikisource.org/wiki/Fables_de_La_Fontaine_(%C3%A9d._1874)/La_Cigale_et_la_Fourmi"
    )
  ; ( "Fables, Le corbeau et le renard"
    , Some "Le corbeau et le renard"
    , "de Jean de La Fontaine"
    , "https://fr.wikisource.org/wiki/Fables_de_La_Fontaine_(%C3%A9d._1874)/Le_Corbeau_et_le_Renard"
    )
  ; ( "Frankenstein, ou le Prométhée moderne"
    , Some "Frankenstein"
    , "de Mary Shelley"
    , "https://fr.wikisource.org/wiki/Frankenstein,_ou_le_Prom%C3%A9th%C3%A9e_moderne_(trad._Saladin)/8"
    )
  ; ( "Les Misérables"
    , None
    , "de Victor Hugo"
    , "https://fr.wikisource.org/wiki/Les_Mis%C3%A9rables/Tome_1/Livre_1/01" )
  ; ( "Cyrano de Bergerac"
    , None
    , "d'Edmond Rostand"
    , "https://fr.wikisource.org/wiki/Cyrano_de_Bergerac_(Rostand)/Acte_I" )
    (* ; "Madame Bovary", None, "de Gustave Flaubert", "https://fr.wikisource.org/wiki/Madame_Bovary/Premi%C3%A8re_partie/1"
     * ; "Candide, ou l'Optimisme", None, "de Voltaire", "https://fr.wikisource.org/wiki/Candide,_ou_l%E2%80%99Optimisme/Garnier_1877/Chapitre_1" *)
  ; ( "Le Comte de Monte-Cristo"
    , None
    , "d'Alexandre Dumas"
    , "https://fr.wikisource.org/wiki/Le_Comte_de_Monte-Cristo/Chapitre_1" )
  ; ( "Vingt mille lieues sous les mers"
    , None
    , "de Jules Verne"
    , "https://fr.wikisource.org/wiki/Vingt_mille_lieues_sous_les_mers/Partie_1/Chapitre_1"
    )
  ]

let download_all ~root =
  with_tempdir "books" "" (fun books_tmp ->
      (* We accumulate new entries into our tarballs without ever cleaning anything.
         Which seems better, in case of back and forth, or for building older revisions.
      *)
      let books_tar = root ^/ "_build/default/site/static/books.tar" in
      let books_local_tar = root ^/ "site/static/books_local.tar" in
      Sys_unix.command_exn
        [%string "tar -xf %{Sys.quote books_tar} -C %{Sys.quote books_tmp}"];

      let first_wait = ref true in
      List.iter books ~f:(fun (title, _user_title, _author, url) ->
          Printf.eprintf "importing %s\n%!" title;
          download
            ~before:(fun () ->
              if not !first_wait
              then (
                Printf.eprintf "waiting\n%!";
                (* wikisource seem to be very aggressive about returning "429 Too Many
                   Requests". Even 10s of pause is not enough. *)
                Unix.sleepf 60.);
              first_wait := false)
            ~books_tmp ~title url);

      (* https://reproducible-builds.org/docs/archives/ *)
      Sys_unix.command_exn
        [%string
          "tar --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner -cf \
           %{Sys.quote books_local_tar} -C %{books_tmp} ."])

let guess_main_file ~url ~data =
  let files =
    let files = Queue.create () in
    ignore
      (Ortografe.map_zip data (fun ~path ->
           Queue.enqueue files path;
           None)
        : string);
    Queue.to_list files
  in
  match
    List.filter files ~f:(fun f ->
        let prefix, ext = Filename.split_extension f in
        match (Filename.basename prefix, ext) with
        | ("nav" | "title" | "about"), _ -> false
        | _, Some ("html" | "xhtml") -> true
        | _ -> false)
  with
  | [ f ] -> f
  | files ->
      raise_s
        [%sexp
          "can't find main .html file for", (url : string), ~~(files : string list)]

let html_li ~url:_ ~author ~user_title ~title ~main_file =
  let rel_url =
    Dream.to_path ([ "static"; "books"; title ] @ String.split main_file ~on:'/')
  in
  [%string
    {|<li><cite><a href="%{rel_url}">%{user_title}</a></cite> <br>%{author} </li>|}]
  ^ "\n"

let convert_all ~books_tar =
  with_tempdir "books" "" (fun books_tmp ->
      Sys_unix.command_exn
        [%string "tar -xf %{Sys.quote books_tar} -C %{Sys.quote books_tmp}"];

      delete_dir conv_basedir;
      let lis =
        List.map books ~f:(fun (title, user_title, author, url) ->
            let new_data = convert ~books_tmp ~title url in
            let main_file = guess_main_file ~url ~data:new_data in
            html_li ~url ~author ~user_title:(user_title ||? title) ~title ~main_file)
      in
      let html_fragment = String.concat_lines [ "<ul class=books>"; +lis; "</ul>" ] in
      Out_channel.write_all "books.html" ~data:html_fragment)
