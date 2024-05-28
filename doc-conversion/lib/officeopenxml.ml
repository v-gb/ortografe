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
  More_markup.filter (function
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
  More_markup.concat_map (fun elt ->
      match elt with
      | `Start_element (ns_tag, _) ->
         Stack.push stack ns_tag;
         Text.Interleaved.emit_structure state elt `Not_special
      | `End_element ->
         let effect =
           match Stack.pop stack with
           | Some (ns, "p") when String.(=) ns doc_ns -> `Flush
           | _ -> `Not_special
         in
         Text.Interleaved.emit_structure state elt effect
      | `Text strs when
             [%compare.equal: (string * string) option] (Stack.top stack) (Some (doc_ns, "t"))
        ->
         Text.Interleaved.emit_text state (String.concat strs)
      | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ ->
         Text.Interleaved.emit_structure state elt `Not_special
    ) signal

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

let convert_xml which ?convert_text ~options src ~dst =
  let convert_text =
    match convert_text with
    | Some f -> f
    | None ->
       let buf = buffer None in
       fun src -> Text.convert ~buf ~options src ~dst:String
  in
  More_markup.transform
    ~impl:options.impl
    ~flavor:`Xml src ~dst
    ~transform:(fun signals ->
      signals
      |> drop_squigglies
      |> convert_stream ~convert_text ~doc_ns:(ns which))

let convert which ?convert_text ~options src ~dst =
  let files_to_rewrite = files_to_rewrite which in
  Zip.map src (fun member contents ->
      if files_to_rewrite (Zipc.Member.path member)
      then Some (convert_xml ?convert_text which ~options (contents ()) ~dst:String)
      else None)
  |> write_out dst

let sys_command_exn ?(handle = Fun.const None) str =
  let i = Sys.command str in
  if i <> 0
  then
    match handle i with
    | Some e -> raise e
    | None -> failwith (str ^ " exited with code " ^ Int.to_string i)

let convert_old which ?convert_text ~options src ~dst =
  let old_ext =
    match which with
    | `Doc -> "doc"
    | `Ppt -> "ppt"
  in
  let new_ext = old_ext ^ "x" in
  (match Stdlib.Sys.backend_type with
  | Native | Bytecode -> ()
  | Other _ ->
     failwith [%string "Les fichiers « .%{old_ext} » ne sont pas supportés ici. Veuillez l'ouvrir, et le sauver en « .%{new_ext} »."]
  );
  (* Should put a memory limit, perhaps with the cgroup exe? At least, in prod the OOM
     killer is selecting the open office process, not the server. *)
  let d = Filename.temp_dir "ortografe" "tmp" in
  Fun.protect
    ~finally:(fun () ->
      sys_command_exn [%string {|rm -rf -- %{Filename.quote d}|}])
    (fun () ->
      let old_path = Filename.concat d ("it." ^ old_ext) in
      let new_path = old_path ^ "x" in
      Out_channel.with_open_bin old_path
        (fun oc -> Out_channel.output_string oc src);
      sys_command_exn [%string {|cd %{Filename.quote d} && timeout -s SIGKILL 10s bwrap --unshare-all --die-with-parent --new-session --dev-bind / / libreoffice --headless --convert-to %{new_ext} %{Filename.basename old_path} >&2|}]
        ~handle:(function
          | 137 (* SIGKILL *) ->
             Some
               (Failure
                  [%string "Échec de la conversion de votre fichier (qui utilise un \
                            ancien format de Microsoft Office) en un format plus \
                            récent, probablement par manque de mémoire.\n\nOuvrez votre \
                            fichier, enregistrez-le en tant que .%{new_ext}, puis \
                            réessayez." ])
          | _ -> None);
      let src =
        In_channel.with_open_bin new_path (fun ic ->
            In_channel.input_all ic)
      in
      convert ?convert_text
        (match which with
         | `Doc -> `Docx
         | `Ppt -> `Pptx)
        ~options src ~dst)
