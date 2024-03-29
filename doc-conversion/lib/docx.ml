open Common

let docx_ns = More_markup.docx_ns
(* This is the spec: http://officeopenxml.com/WPcontentOverview.php.
   I think that:
   - all text is inside w:t nodes
   - all w:t nodes are inside w:p nodes. Tables apparently contain <w:p> for their
     text for instance.
 *)

let drop_squigglies signals =
  (* docs contains things like:

    <w:proofErr w:type="spellStart"/>
    <w:r>
      <w:rPr>
        <w:rFonts w:ascii="Arial" w:hAnsi="Arial"/>
        <w:sz w:val="22"/>
      </w:rPr>
      <w:t>bla bla bla</w:t>
    </w:r>
    <w:proofErr w:type="spellEnd"/>

    Just discard it all. This is probably not necessary at this point, but oh well it's
    already written.
   *)
  let open Core in
  let stack = Stack.of_list [true] in
  Markup.filter (function
      | `Start_element (name, _) ->
         Stack.push stack
           (not ([%compare.equal: (string * string)] name (docx_ns, "proofErr")));
         Stack.top_exn stack
      | `End_element ->
         let b = Stack.top_exn stack in
         ignore (Stack.pop_exn stack);
         b
      | _ -> Stack.top_exn stack
    ) signals

let docx_convert ~buf ~options signal =
  let open Core in
  let stack = Stack.create () in
  let convert_text src = Text.convert ~buf ~options src ~dst:String in
  let state = Text.Interleaved.create ~embed:(fun s -> `Text [s]) ~convert:convert_text in
  Markup.transform (fun () elt ->
      match elt with
      | `Start_element (ns_tag, _) ->
         Stack.push stack ns_tag;
         Text.Interleaved.emit_structure state elt `Not_special, Some ()
      | `End_element ->
         let effect =
           match Stack.pop stack with
           | Some (ns, "p") when String.(=) ns docx_ns -> `Flush
           | _ -> `Not_special
         in
         Text.Interleaved.emit_structure state elt effect, Some ()
      | `Text strs when
             [%compare.equal: (string * string) option] (Stack.top stack) (Some (docx_ns, "t"))
        ->
         Text.Interleaved.emit_text state (String.concat strs), Some ()
      | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ ->
         Text.Interleaved.emit_structure state elt `Not_special, Some ()
    ) () signal

let convert_xml ?buf ~options src ~dst =
  let buf = buffer buf in
  More_markup.transform ~flavor:`Xml src ~dst ~transform:(fun signals ->
      signals
      |> drop_squigglies
      |> docx_convert ~buf ~options)

let convert ?buf ~options src ~dst =
  let buf = buffer buf in
  Zip.map src (fun member contents ->
      match Zipc.Member.path member with
      | "word/document.xml"
      | "word/footnotes.xml"
      | "word/endnotes.xml" ->
         Some (convert_xml ~buf ~options (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let sys_command_exn str =
  let i = Sys.command str in
  if i <> 0
  then failwith (str ^ " exited with code " ^ Int.to_string i)

let convert_doc ?buf ~options src ~dst =
  let buf = buffer buf in
  (* should put a time limit and memory, perhaps with the cgroup exe? *)
  let d = Filename.temp_dir "ortografe" "tmp" in
  Fun.protect
    ~finally:(fun () ->
      sys_command_exn [%string {|rm -rf -- %{Filename.quote d}|}])
    (fun () ->
      let doc_path = Filename.concat d "it.doc" in
      let docx_path = doc_path ^ "x" in
      Out_channel.with_open_bin doc_path
        (fun oc -> Out_channel.output_string oc src);
      sys_command_exn [%string {|cd %{Filename.quote d} && timeout -s SIGKILL 10s bwrap --unshare-all --die-with-parent --new-session --dev-bind / / libreoffice --headless --convert-to docx %{Filename.basename doc_path} >&2|}];
      let src =
        In_channel.with_open_bin docx_path (fun ic ->
            In_channel.input_all ic)
      in
      convert ~buf ~options src ~dst)
