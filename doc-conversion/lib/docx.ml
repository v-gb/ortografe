open Common

let docx_ns = More_markup.docx_ns

module Sm = struct
  (* [Sm] is a module that implements a small state machine (or parser combinator, I
     suppose) that recognizes a part of the signal stream. I don't know if it's wise
     to do this in a streaming way, as it's definitely more complicated. On the other
     hand, these xml can be ridiculously verbose, so it does cut down in memory usage,
     which matters for deployment. *)
  open Core

  type t =
    | Tree of { mutable depth : int; elts : More_markup.signal Queue.t }
    | Element of { mutable start_elt : More_markup.signal option
                 ; mutable rev_done : t list
                 ; mutable todo : t list
                 ; mutable done_ : (More_markup.signal * [ `Ok of bool ]) option
                 }
  [@@deriving sexp_of]

  (* [add_and_done] should be called on consecutive elements of the stream until it
     returns [true], indicating it's done parsing (either it has parsed the whole
     structure it was intended to parse, or the stream doesn't have the right shape).

     The first element given to a [Tree] should not be a `[End_element], because this is
     the only possible way for this node to fail, and it has no way to encode failures. *)
  let rec add_and_done t elt =
    match t with
    | Tree r ->
       Queue.enqueue r.elts elt;
       (match elt with
        | `Start_element _ -> r.depth <- r.depth + 1
        | `End_element -> r.depth <- r.depth - 1
        | _ -> ());
       assert (r.depth >= 0);
       r.depth = 0
    | Element r ->
       (match r.start_elt with
        | None ->
           (match elt with
            | `Start_element _ -> r.start_elt <- Some elt; false
            | _ -> r.done_ <- Some (elt, `Ok false); true)
        | Some _ ->
           match r.todo with
           | [] ->
              r.done_ <- Some (elt, `Ok (match elt with `End_element -> true | _ -> false));
              true
           | next :: rest ->
              let problem =
                match elt, next with
                | `End_element, Tree { depth = 0; _ } -> true
                | _, _ -> false
              in
              if problem
              then (r.done_ <- Some (elt, `Ok false); true)
              else
                (if add_and_done next elt
                 then (r.rev_done <- next :: r.rev_done; r.todo <- rest);
                 false))

  let rec fold t acc =
    match t with
    | Tree r ->
       Queue.fold r.elts ~init:acc ~f:(fun acc elt -> elt :: acc)
    | Element r ->
       let acc =
         match r.start_elt with
         | None -> acc
         | Some elt -> elt :: acc
       in
       let acc =
         List.fold (List.rev r.rev_done) ~init:acc
           ~f:(fun acc elt -> fold elt acc) in
       match r.done_ with
       | None -> acc
       | Some (elt, _good) -> elt :: acc
end

include struct
    open Core
    let create_sm elt =
      Sm.Element
        { start_elt = Some elt
        ; rev_done = []
        ; todo = [ Tree { depth = 0; elts = Queue.create () }
                 ; Tree { depth = 0; elts = Queue.create () }
                 ]
        ; done_ = None
        }

    type previously_parsed =
      { start1 : More_markup.signal
      ; block : More_markup.signal Queue.t
      ; start_text : More_markup.signal
      ; text : Buffer.t (* buffer to guarantee linear time construction *)
      ; end_text : More_markup.signal
      ; end2 : More_markup.signal
      }
    [@@deriving sexp_of]

    let previously_parsed sm =
      (* finising parsing the Sm.t into the specific shape we wanted to recognize *)
      match sm with
      | Sm.Element r ->
         (match r.start_elt, r.done_ with
          | Some start1, Some (end2, `Ok true) ->
             (match r.rev_done with
              | [ Tree t2; Tree t1 ] ->
                 if Queue.length t2.elts = 3
                 then
                   match Queue.to_list t2.elts with
                   | [ `Start_element (name, _) as start_text; `Text text; `End_element as end_text ]
                        when [%compare.equal: (string * string)] name (docx_ns, "t")
                     ->
                      let b = Buffer.create (List.sum (module Int) text ~f:String.length) in
                      List.iter text ~f:(Buffer.add_string b);
                      Some { start1; block = t1.elts; start_text; text = b; end_text; end2 }
                   | _ -> None
                 else None
              | _ -> None)
          | _ -> None)
      | _ -> None

    let attr_is_xml_space name =
      [%compare.equal: (string * string)] name (Markup.Ns.xml, "space")

    let fold_previously_parsed { start1; block; start_text; text; end_text; end2 } acc =
      let text = Buffer.contents text in
      let acc = Queue.fold block ~init:(start1 :: acc) ~f:(fun acc elt -> elt :: acc) in
      let start_text =
        match start_text with
        | `Start_element (name, attrs) ->
           let attrs = List.filter attrs ~f:(fun (name, _) -> not (attr_is_xml_space name)) in
           let attrs =
             if String.(=) text (String.strip text)
             then attrs
             else ((Markup.Ns.xml, "space"), "preserve") :: attrs
           in
           `Start_element (name, attrs)
        | _ -> assert false
      in
      end2 :: end_text :: `Text [text] :: start_text :: acc

    let fold_previously_parsed_opt previously_parsed_opt acc =
      match previously_parsed_opt with
      | None -> acc
      | Some p -> fold_previously_parsed p acc

    type state =
      | Outside of previously_parsed option (* Some if we just finished parsing a <r> *)
      | In_r of previously_parsed option * Sm.t
    [@@deriving sexp_of]
end

let join_consecutive_ish_text_nodes signal =
  (* libreoffice is happy to generate spammy boundaries, as shown below when editing
     "un deux trois quatre" by replacing the x with an s. So take out this spam
     so we have long runs that don't cut words in half, thus allowing the next
     phase to see full words instead of word fragments.

    <w:r>
      <w:rPr/>
      <w:t>Un deu</w:t>
    </w:r>
    <w:r>
      <w:rPr/>
      <w:t>s</w:t>
    </w:r>
    <w:r>
      <w:rPr/>
      <w:t xml:space="preserve"> trois quatre.</w:t>
    </w:r>

  This function turns that into:

    <w:r>
      <w:rPr/>
      <w:t>Un deus trois quatre</w:t>
    </w:r>

  MS Word seems to create the same sort of things except it adds properties like
  w:rsidR="00F25C40" on nodes, for what seems to be history of the changes in the doc
  (although concretely, I can't see how to access said history).

  Regardless, we trample over all this stuff. The new document should be considered
  a big C-c/C-v of the previous doc, not a set of minimal edits.

  In principle, the maximally smart thing to do would be to treat the docx-specific
  code as providing a stream of [string option] (stream of "word or boundary", the
  boundaries indicating document structure like the end of a bullet point in a list)
  and the linguistic part of the code would turn every word into exactly one string,
  (which could be the empty string, one word or multiple words). This would then allow
  the docx code to reinsert the structure of the document around the new text in the
  way that alters the structure from the input document the least. But that seems like
  quite a bit of work, and it's not clear that we should care.

  http://officeopenxml.com/WPtext.php might have useful information, because the spec
  itself is huge, and probably unreadable.
   *)
  let open Core in
  let flush previous elt =
    List.rev (elt :: fold_previously_parsed_opt previous [])
  in
  let try_joining_sm (previous : previously_parsed option) current =
    match previously_parsed current with
    | None -> List.rev (Sm.fold current (fold_previously_parsed_opt previous [])), Some (Outside None)
    | Some current ->
      match previous with
      | None -> [], Some (Outside (Some current))
      | Some previous ->
         if Queue.equal More_markup.equal_signal previous.block current.block
         then (Buffer.add_buffer previous.text current.text; [], Some (Outside (Some previous)))
         else List.rev (fold_previously_parsed previous []), Some (Outside (Some current))
  in
  Markup.transform (fun state elt ->
      match state with
      | Outside previous ->
         (match elt with
          | `Start_element (ns_tag, _) ->
             if [%compare.equal: (string * string)] ns_tag (docx_ns, "r")
             then [], Some (In_r (previous, create_sm elt))
             else flush previous elt, Some (Outside None)
          | _ -> flush previous elt, Some (Outside None))
      | In_r (previous, sm) ->
         if Sm.add_and_done sm elt
         then try_joining_sm previous sm
         else [], Some state)
    (Outside None)
    signal

(* il faut plus de tests aussi *)

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

    Just discard it all, so the next transformations can be more effective.
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
  Markup.map (fun elt ->
      (match elt with
       | `Start_element (ns_tag, _) ->
          Stack.push stack ns_tag;
       | `End_element ->
          ignore (Stack.pop stack)
       | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> ());
      if [%compare.equal: (string * string) option] (Stack.top stack) (Some (docx_ns, "t"))
      then More_markup.text_elt ~convert_text elt
      else elt
    ) signal

let convert_xml ?debug ?pp ?buf ~options src ~dst =
  let buf = buffer buf in
  More_markup.transform ?debug ?pp ~flavor:`Xml src ~dst ~transform:(fun signals ->
      signals
      |> drop_squigglies
      |> join_consecutive_ish_text_nodes
      |> docx_convert ~buf ~options)

let convert ?debug ?pp ?buf ~options src ~dst =
  let buf = buffer buf in
  Zip.map src (fun member contents ->
      match Zipc.Member.path member with
      | "word/document.xml"
      | "word/footnotes.xml"
      | "word/endnotes.xml" ->
         Some (convert_xml ~buf ?debug ?pp ~options (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let sys_command_exn str =
  let i = Sys.command str in
  if i <> 0
  then failwith (str ^ " exited with code " ^ Int.to_string i)

let convert_doc ?debug ?pp ?buf ~options src ~dst =
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
      convert ~buf ?debug ?pp ~options src ~dst)

module Private = struct
  let join_consecutive_ish_text_nodes signals =
    (* since this is for testing, including squigglies *)
    signals
    |> drop_squigglies
    |> join_consecutive_ish_text_nodes
end
