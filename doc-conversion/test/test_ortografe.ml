open Core

let options =
  { Ortografe.convert_uppercase = false
  ; dict = Stdlib.Hashtbl.find_opt (force Ortografe.erofa)
  ; interleaved = true
  ; plurals_in_s = true
  }

let pp_xml src =
  Markup.parse_xml
    (Markup.string src)
  |> Markup.signals
  |> Markup.pretty_print
  |> Markup.write_xml
  |> Markup.to_string

let pp_html src =
  Markup.parse_html
    (Markup.string src)
  |> Markup.signals
  |> Markup.pretty_print
  |> Markup.write_html
  |> Markup.to_string

let diff_strings ?(context = 1) str1 str2 =
  if String.(=) str1 str2
  then ""
  else
    let lines1 = String.split_lines str1 |> Array.of_list in
    let lines2 = String.split_lines str2 |> Array.of_list  in
    let hunks =
      Patience_diff_lib.Patience_diff.String.get_hunks
        ~transform:Fn.id
        ~context
        ~prev:lines1
        ~next:lines2
        ()
    in
    let ranges = Patience_diff_lib.Patience_diff.Hunks.ranges hunks in
    let b = ref [] in
    List.iter ranges ~f:(fun range ->
        match range with
        | Same a -> Array.iter a ~f:(fun (s, _) -> b := (" ", s) :: !b)
        | Unified a -> Array.iter a ~f:(fun s -> b := (" ", s) :: !b)
        | Prev a -> Array.iter a ~f:(fun s -> b := ("-", s) :: !b)
        | Next a -> Array.iter a ~f:(fun s -> b := ("+", s) :: !b)
        | Replace (a, a') ->
           Array.iter a ~f:(fun s -> b := ("-", s) :: !b);
           Array.iter a' ~f:(fun s -> b := ("+", s) :: !b);
      );
    List.rev !b
    |> List.map ~f:(fun (sign, s) -> sign ^ s ^ "\n")
    |> String.concat

let%expect_test "pure text" = (
  let test = "
Mots simples: comment choix photographie.
Diacritiques: nfd la\204\128 paralle\204\128le
              nfc l\195\160  parall\195\168le
Urls: https://comment/choix/photographie-123-prix
Pluriels: hommes.
Capitalisation: Choix.
Tirets: plouf-européenne
Majuscules: HISTOIRE.
Nom propre: pierre Pierre pierre-feuille-ciseau Jean-Pierre.
" in
  let rewrite1 = Ortografe.pure_text ~options test ~dst:String in
  print_string rewrite1;
  [%expect "
    Mots simples: coment chois fotografie.
    Diacritiques: nfd l\195\160 paral\195\168le
                  nfc l\195\160  paral\195\168le
    Urls: https://comment/choix/photographie-123-prix
    Pluriels: omes.
    Capitalisation: Chois.
    Tirets: plouf-europ\195\169\195\168ne
    Majuscules: HISTOIRE.
    Nom propre: pi\195\168re Pierre pi\195\168re-feuille-ciseau Jean-Pierre. "];
  (
    let rewrite2 =
      Ortografe.pure_text test ~dst:String
        ~options:{options with convert_uppercase = true}
    in
    print_string (diff_strings rewrite1 rewrite2 ~context:0);
    [%expect "
      -Majuscules: HISTOIRE.
      +Majuscules: ISTOIRE."];
  );
  (
    let rewrite2 =
      Ortografe.pure_text test ~dst:String
        ~options:{options with plurals_in_s = false}
    in
    print_string (diff_strings rewrite1 rewrite2 ~context:0);
    [%expect "
      -Pluriels: omes.
      +Pluriels: hommes."];
  );
  let rewrite3 =
    Ortografe.pure_text
      ~dst:String
      ~options:{options with dict = Stdlib.Hashtbl.find_opt (force Ortografe.rect1990) }
      {|
Le maître a dû goûter ce week-end.
Un sèche-cheveux, des sèche-cheveux.
|}
  in
  print_string rewrite3;
  [%expect "
    Le maitre a d\195\187 gouter ce weekend.
    Un s\195\168che-cheveux, des s\195\168che-cheveux."]
)


let%expect_test "html" = (
    (* checking:
- a standard looking page (standard headers), including uppercase ones
- title and body should be transcribed
- script (and CSS, although not tested) should not be transcribed
*)
    let html = {|
<!DOCTYPE html>
<html class="" lang="fr" dir="ltr">
<HEAD>
<meta charset="UTF-8">
<title>Photographie — Wikipédia</title>
<script> // bug if rewritten: comment </script>
<body>
<div class="comment">comment</div>
<br>
<script> // bug if rewritten: comment </script>
<p>Les <span>éléphants</span></p>
<div contenteditable=true>bug if rewritten: comment</div>
</body>
</head>
</html>
|} in
    let new_html = Ortografe.html ~options html ~dst:String in
    print_string (diff_strings (pp_html html) (pp_html new_html));
    [%expect {|
   <title>
-   Photographie — Wikipédia
+   Fotografie — Wikipédia
   </title>
   <div class="comment">
-   comment
+   coment
   </div>
   <p>
-   Les <span>éléphants</span>
+   Les <span>éléfants</span>
   </p> |}]
)

let docx_document zip =
  Ortografe.Private.grab_from_zip zip "word/document.xml"

let%expect_test "docx" = (
    let docx = In_channel.read_all "test.docx" in
    print_string (docx_document docx);
    [%expect {|
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" mc:Ignorable="w14 wp14 w15"><w:body><w:p><w:pPr><w:pStyle w:val="Normal"/><w:bidi w:val="0"/><w:jc w:val="left"/><w:rPr></w:rPr></w:pPr><w:r><w:rPr></w:rPr><w:t>Allusion choix</w:t></w:r></w:p><w:p><w:pPr><w:pStyle w:val="Normal"/><w:bidi w:val="0"/><w:jc w:val="left"/><w:rPr></w:rPr></w:pPr><w:r><w:rPr></w:rPr></w:r></w:p><w:sectPr><w:type w:val="nextPage"/><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:left="1134" w:right="1134" w:gutter="0" w:header="0" w:top="1134" w:footer="0" w:bottom="1134"/><w:pgNumType w:fmt="decimal"/><w:formProt w:val="false"/><w:textDirection w:val="lrTb"/></w:sectPr></w:body></w:document> |}];
    let orig_xml = pp_xml (docx_document docx) in
    let new_xml = pp_xml (docx_document (Ortografe.officeopenxml `Docx ~options docx ~dst:String)) in
    print_string (diff_strings orig_xml new_xml);
    [%expect {|
     <w:t>
-     Allusion choix
+     Alusion chois
     </w:t> |}];
    let new_xml2 = pp_xml (docx_document (Ortografe.officeopenxml `Docx ~options:{options with interleaved = false} docx ~dst:String)) in
    print_string (diff_strings new_xml new_xml2);
    [%expect {| |}];
)


let%expect_test "rewrite through tag noise in docx" = (
    let sexp_of_xmlstr src =
      (* we print a sexp because xml pp doesn't respect meaningful whitespaces, so it
         changes the meaning of the doc in confusing ways *)
      Markup.parse_xml
        (Markup.string src)
      |> Markup.signals
      |> Ortografe.More_markup.trees
      |> Markup.to_list
      |> [%sexp_of: Ortografe.More_markup.tree list]
      |> Sexp.to_string_hum
    in
    let text = {|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" mc:Ignorable="w14 wp14 w15"><w:body><w:p><w:pPr><w:pStyle w:val="Normal"/><w:bidi w:val="0"/><w:jc w:val="left"/><w:rPr></w:rPr></w:pPr><w:r><w:rPr></w:rPr><w:t>Un deu</w:t></w:r><w:r><w:rPr></w:rPr><w:t>x</w:t></w:r><w:r><w:rPr></w:rPr><w:t xml:space="preserve"> trois quatre.</w:t></w:r></w:p><w:p><w:pPr><w:pStyle w:val="Normal"/><w:bidi w:val="0"/><w:jc w:val="left"/><w:rPr></w:rPr></w:pPr><w:r><w:rPr></w:rPr></w:r></w:p><w:sectPr><w:type w:val="nextPage"/><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:left="1134" w:right="1134" w:gutter="0" w:header="0" w:top="1134" w:footer="0" w:bottom="1134"/><w:pgNumType w:fmt="decimal"/><w:formProt w:val="false"/><w:textDirection w:val="lrTb"/><w:docGrid w:type="default" w:linePitch="100" w:charSpace="0"/></w:sectPr></w:body></w:document>
 |}
    in
    print_string (diff_strings ~context:3
                    (sexp_of_xmlstr text)
                    (sexp_of_xmlstr (Ortografe.Private.convert_officeopenxml `Docx
                                       ~options text ~dst:String)));
    [%expect{|
                  (docx:r
                   ((Element (docx:rPr ())) (Element (docx:t ((Text "Un deu")))))))
                 (Element
      -           (docx:r ((Element (docx:rPr ())) (Element (docx:t ((Text x)))))))
      +           (docx:r ((Element (docx:rPr ())) (Element (docx:t ((Text s)))))))
                 (Element
                  (docx:r
                   ((Element (docx:rPr ())) |}];
    let text = {|<w:document xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" mc:Ignorable="w14 wp14"><w:body><w:p w:rsidR="A" w:rsidRDefault="B"><w:pPr><w:spacing w:line="360" w:lineRule="auto"/><w:ind w:left="5670"/><w:jc w:val="both"/><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="22"/></w:rPr></w:pPr><w:r><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="22"/></w:rPr><w:t xml:space="preserve">M. </w:t></w:r><w:r w:rsidR="C"><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="22"/></w:rPr><w:t>Jacques d</w:t></w:r><w:r><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="22"/></w:rPr><w:t xml:space="preserve">e </w:t></w:r><w:proofErr w:type="spellStart"/><w:r><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/><w:sz w:val="22"/></w:rPr><w:t>Bravil</w:t></w:r><w:proofErr w:type="spellEnd"/></w:p></w:body></w:document>|} in
    print_string (diff_strings ~context:3
                    (sexp_of_xmlstr text)
                    (sexp_of_xmlstr (Ortografe.Private.convert_officeopenxml `Docx
                                       ~options text ~dst:String)));
    [%expect{|
                        (docx:rFonts ((docx:ascii Arial) (docx:hAnsi Arial)) ()))
                       (Element (docx:sz ((docx:val 22)) ())))))
                    (Element (docx:t ((xml:space preserve)) ((Text "e ")))))))
      -          (Element (docx:proofErr ((docx:type spellStart)) ()))
                 (Element
                  (docx:r
                   ((Element
                      ((Element
                        (docx:rFonts ((docx:ascii Arial) (docx:hAnsi Arial)) ()))
                       (Element (docx:sz ((docx:val 22)) ())))))
      -             (Element (docx:t ((Text Bravil)))))))
      -          (Element (docx:proofErr ((docx:type spellEnd)) ()))))))))))))
      +             (Element (docx:t ((Text Bravil)))))))))))))))))
|}];
  )

let%expect_test "epub" = (
    let src = In_channel.read_all "epub.md" in
    Sys_unix.command_exn "pandoc epub.md -o epub.epub";
    Out_channel.write_all "epub-conv.epub"
      ~data:(Ortografe.epub ~options (In_channel.read_all "epub.epub") ~dst:String);
    Sys_unix.command_exn "pandoc epub-conv.epub -o epub-conv.md";
    let src_conv = In_channel.read_all "epub-conv.md" in
    print_string src;
    [%expect "
      % title Orthographe

      # Orthographe

      Allusion, choix."];
    print_string src_conv;
    [%expect "
      []{#title_page.xhtml}

      []{#ch001.xhtml}

      ::: {#ch001.xhtml#orthographe .section .level1 number=\"1\"}
      # Ortografe {number=\"1\"}

      Alusion, chois.
      :::"];
)

let%expect_test "doc" = (
    (* It seems that libreoffice can read .doc files but not
       create. Since we mostly want to test that the command line
       invocation of libreoffice works, and since that command line doesn't
       specify the source file type, just use a .docx file as a .doc *)
    let docx = In_channel.read_all "test.docx" in
    let orig_xml = pp_xml (docx_document docx) in
    let new_xml = pp_xml (docx_document (Ortografe.officeopenxml `Docx ~options docx ~dst:String)) in
    print_string (diff_strings orig_xml new_xml);
    [%expect {|
     <w:t>
-     Allusion choix
+     Alusion chois
     </w:t> |}];
)

let%expect_test "odt" = (
    let grab_xml zip =
      pp_xml (Ortografe.Private.grab_from_zip zip "content.xml")
      ^ "\n"
      ^ pp_xml (Ortografe.Private.grab_from_zip zip "styles.xml")
    in
    let odt = In_channel.read_all "test.odt" in
    let orig_xml = grab_xml odt in
    let new_xml =
      grab_xml (Ortografe.opendocument ~options:{options with interleaved = false} odt ~dst:String) in
    let new_xml2 =
      grab_xml (Ortografe.opendocument ~options odt ~dst:String) in
    print_string (diff_strings orig_xml new_xml);
    [%expect {|
    <text:p text:style-name="P3">
-    Corps : allumé
+    Corps : alumé
    </text:p>
     <text:span text:style-name="T1">
-     Deux en gras
+     Deus en gras
     </text:span>
     <text:span text:style-name="T2">
-     deux en italique
+     deus en italique
     </text:span>
    <text:p text:style-name="P4">
-    Plusieurs espaces : deux
+    Plusieurs espaces : deus
     <text:s text:c="2"/>
     <text:p text:style-name="MP1">
-     En-tete : homme
+     En-tete : ome
     </text:p>
     <text:p text:style-name="MP2">
-     Pied de page : allure
+     Pied de page : alure
     </text:p> |}];
    print_string (diff_strings ~context:5 new_xml new_xml2);
    [%expect {|
     </text:span>
     <text:span text:style-name="T2">
      u
     </text:span>
     <text:span text:style-name="T1">
-     x
+     s
     </text:span>
     en mis-gras, mi-italique
    </text:p>
    <text:p text:style-name="P4">
     Plusieurs espaces : deus
    <text:p text:style-name="P4">
     Édité :
     <text:span text:style-name="T4">
      fa
     </text:span>
-    ux
+    us
    </text:p>
   </office:text>
  </office:body>
 </office:document-content> |}];
)

let set_vm_limit =
  lazy (
      let oneGiB = Int64.of_int (1024 * 1024 * 1024) in
      let vmem_limit = ok_exn Core_unix.RLimit.virtual_memory in
      let current_limit = Core_unix.RLimit.get vmem_limit in
      Core_unix.RLimit.set
        vmem_limit
        { current_limit with cur =
                               match current_limit.cur with
                               | Infinity -> Limit oneGiB
                               | Limit a -> Limit (Int64.min a oneGiB) };
      Ortografe.max_size := 50_000_000; (* to speed up the test *)
    )

let time f =
  let before = Time_ns.now () in
  let x = f () in
  let after = Time_ns.now () in
  x, (Time_ns.diff after before)

let read_whole_zip zip =
  Ortografe.map_zip zip (fun _ content -> Some (content ()))

let%expect_test "zip_bomb" = (
    let dir = "/tmp/zip-bomb" in
    Core_unix.mkdir_p dir;
    force set_vm_limit;
    let urls =
      [ "https://www.bamsoftware.com/hacks/zipbomb/zbsm.zip"
      ; "https://www.bamsoftware.com/hacks/zipbomb/42.zip" (* version of 42.zip without the annoying password, as found at https://www.bamsoftware.com/hacks/zipbomb/ *)
      ; "https://web.archive.org/web/20160130230432/http://www.steike.com/code/useless/zip-file-quine/droste.zip"
      ]
    in
    List.iter urls ~f:(fun url ->
        let basename = Filename.basename url in
        if not (Sys_unix.file_exists_exn (dir ^/ basename))
        then
          Sys_unix.command_exn
            [%string "curl -S -s %{url} -o %{Sys.quote (dir ^/ basename)}"];
      );
    List.iter urls ~f:(fun url ->
        let zip_bomb = In_channel.read_all (dir ^/ Filename.basename url) in
        let result, duration =
          time (fun () -> Result.try_with (fun () -> read_whole_zip zip_bomb))
        in
        if false
        then print_s [%sexp
                         (Result.is_ok result : bool),
                         (url : string),
                         (duration : Time_ns.Span.t)];
        if Float.(>) (Time_ns.Span.to_sec duration) 1.
        then failwith "too slow!" (* takes us in practice *)
      )
  )

let%expect_test "just large file" = (
    force set_vm_limit;
    Sys_unix.command_exn "if ! [ -f /tmp/zip-bomb/large.zip ]; then head -c 5000000000 /dev/zero > /tmp/zip-bomb/large && zip -q /tmp/zip-bomb/large.zip /tmp/zip-bomb/large && rm -f /tmp/zip-bomb/large; fi";
    let result, duration =
      time (fun () ->
          Result.try_with (fun () ->
              read_whole_zip (In_channel.read_all "/tmp/zip-bomb/large.zip")))
    in
    print_s [%sexp (result : (_, exn) Result.t)];
    [%expect "(Error (Failure \"files in zip too large\"))"];
    if Float.(>) (Time_ns.Span.to_sec duration) 1.
    then failwith "too slow!" (* takes us in practice *)
  )
