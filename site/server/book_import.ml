let import_from_wikisource url ~dst =
  let epub_url =
    (* url looks like "https://fr.wikisource.org/wiki/Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
       epub url looks like "https://ws-export.wmcloud.org/?format=epub&lang=fr&page=Pens%C3%A9es_de_Marc-Aur%C3%A8le_(Couat)/01"
     *)
    let title = Core.String.chop_prefix_exn ~prefix:"/wiki/" (Uri.path (Uri.of_string url)) in
    "https://ws-export.wmcloud.org/?format=epub&lang=fr&page=" ^ title
  in
  let epub = Hyper.get epub_url in
   (* Sometimes you see headings that are uppercase in the source, instead by using
      text-transform or font-variant, so just convert those. *)
  let epub_conv = Ortografe.epub ~convert_uppercase:true epub ~dst:String in
  let epub_conv_styled =
    Ortografe.map_zip epub_conv
      (fun member _file contents ->
        (* There's missing CSS in the wikisource epubs (or maybe it's intended that the
           ereader would provide its own). So add CSS settings to make the look of the
           converted xhtml and the wikisource html similar. *)
        match Filename.basename (Zipc.Member.path member) with
        | "main.css" ->
           Some (contents () ^ {|
body {
  max-width:36em;
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
  Core.Out_channel.write_all dst ~data:epub_conv_styled
