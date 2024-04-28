open Common

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
           (not ([%compare.equal: (string * string)] name (More_markup.docx_ns, "proofErr")));
         Stack.top_exn stack
      | `End_element ->
         let b = Stack.top_exn stack in
         ignore (Stack.pop_exn stack);
         b
      | _ -> Stack.top_exn stack
    ) signals

let convert_stream ~convert_text ~doc_ns signal =
  let open Core in
  let stack = Stack.create () in
  let state = Text.Interleaved.create ~embed:(fun s -> `Text [s]) ~convert:convert_text in
  Markup.transform (fun () elt ->
      match elt with
      | `Start_element (ns_tag, _) ->
         Stack.push stack ns_tag;
         Text.Interleaved.emit_structure state elt `Not_special, Some ()
      | `End_element ->
         let effect =
           match Stack.pop stack with
           | Some (ns, "p") when String.(=) ns doc_ns -> `Flush
           | _ -> `Not_special
         in
         Text.Interleaved.emit_structure state elt effect, Some ()
      | `Text strs when
             [%compare.equal: (string * string) option] (Stack.top stack) (Some (doc_ns, "t"))
        ->
         Text.Interleaved.emit_text state (String.concat strs), Some ()
      | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ ->
         Text.Interleaved.emit_structure state elt `Not_special, Some ()
    ) () signal

let ns = function
  | `Docx -> More_markup.docx_ns
  | `Pptx -> "http://schemas.openxmlformats.org/drawingml/2006/main"

let files_to_rewrite = function
  | `Docx ->
     (function
      | "word/document.xml"
      | "word/footnotes.xml"
      | "word/endnotes.xml" -> true
      | _ -> false)
  | `Pptx ->
     (fun str ->
       if Filename.extension str = ".xml"
       then
         match Filename.dirname str with
         | "ppt/slides" -> true
         | _ -> false
       else false)

let convert_xml which ?convert_text ?buf ~options src ~dst =
  let buf = buffer buf in
  let convert_text =
    match convert_text with
    | Some f -> f
    | None -> fun src -> Text.convert ~buf ~options src ~dst:String
  in
  More_markup.transform ~flavor:`Xml src ~dst ~transform:(fun signals ->
      signals
      |> drop_squigglies
      |> convert_stream ~convert_text ~doc_ns:(ns which))

let convert which ?convert_text ?buf ~options src ~dst =
  let buf = buffer buf in
  let files_to_rewrite = files_to_rewrite which in
  Zip.map src (fun member contents ->
      if files_to_rewrite (Zipc.Member.path member)
      then Some (convert_xml ?convert_text which ~buf ~options (contents ()) ~dst:String)
      else None)
  |> write_out dst

let sys_command_exn str =
  let i = Sys.command str in
  if i <> 0
  then failwith (str ^ " exited with code " ^ Int.to_string i)

let convert_old which ?convert_text ?buf ~options src ~dst =
  let buf = buffer buf in
  (* should put a time limit and memory, perhaps with the cgroup exe? *)
  let d = Filename.temp_dir "ortografe" "tmp" in
  Fun.protect
    ~finally:(fun () ->
      sys_command_exn [%string {|rm -rf -- %{Filename.quote d}|}])
    (fun () ->
      let old_ext =
        match which with
        | `Doc -> "doc"
        | `Ppt -> "ppt"
      in
      let new_ext = old_ext ^ "x" in
      let old_path = Filename.concat d ("it." ^ old_ext) in
      let new_path = old_path ^ "x" in
      Out_channel.with_open_bin old_path
        (fun oc -> Out_channel.output_string oc src);
      sys_command_exn [%string {|cd %{Filename.quote d} && timeout -s SIGKILL 10s bwrap --unshare-all --die-with-parent --new-session --dev-bind / / libreoffice --headless --convert-to %{new_ext} %{Filename.basename old_path} >&2|}];
      let src =
        In_channel.with_open_bin new_path (fun ic ->
            In_channel.input_all ic)
      in
      convert ?convert_text
        (match which with
         | `Doc -> `Docx
         | `Ppt -> `Pptx)
        ~buf ~options src ~dst)
