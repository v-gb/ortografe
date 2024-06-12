open Common
open struct module Markup = Markup_t end

let odt_transform ~convert_text signal =
  (* spec: http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html#__RefHeading__1415130_253892949 *)
  (* this won't work in many cases, due to words being split up by document structure *)
  let text_ns = "urn:oasis:names:tc:opendocument:xmlns:text:1.0" in
  let open Core in
  let stack = Stack.create () in
  Markup.map (fun elt ->
      match elt with
      | `Start_element (ns_tag, _) ->
         Stack.push stack ns_tag;
         elt
      | `End_element ->
         ignore (Stack.pop stack);
         elt
      | `Text _ when (
        let ns, name = Stack.top_exn stack in
        match name with
        | "h" | "p" | "a" | "span" (* All four things described as "mixed content" in
                                      the doc. Maybe I should instead grab all text below
                                      text:h and text:p and exclude a few things as described
                                      in section 6.1.1 *)
          ->
           String.(=) ns text_ns
        | _ -> false
      ) ->
         More_markup.text_elt ~convert_text elt
      | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> elt
    ) signal

let odt_transform_interleaved ~convert_text signal =
  let text_ns = "urn:oasis:names:tc:opendocument:xmlns:text:1.0" in
  let open Core in
  let stack = Stack.create () in
  let state =
    Text.Interleaved.create
      ~embed:(fun s -> `Text [s])
      ~convert:convert_text
  in
  More_markup.concat_map (fun elt ->
      match elt with
      | `Start_element (ns_tag, _) ->
         Stack.push stack ns_tag;
         Text.Interleaved.emit_structure state elt `Not_special
      | `End_element ->
         let effect =
           match Stack.pop stack with
           | Some (ns, ("h" | "p")) when String.(=) ns text_ns -> `Flush
           | Some (ns, ("s" | "tab" | "line-break")) when String.(=) ns text_ns -> `Space
           | _ -> `Not_special
         in
         Text.Interleaved.emit_structure state elt effect
      | `Text strs when (
        let ns, tag = Stack.top_exn stack in
        match tag with
        | "h" | "p" | "a" | "span" (* All four things described as "mixed content" in
                                      the doc. Maybe I should instead grab all text below
                                      text:h and text:p and exclude a few things as described
                                      in section 6.1.1 *)
          -> String.(=) ns text_ns
        | _ -> false
      ) ->
         Text.Interleaved.emit_text state (String.concat strs)
       | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ ->
         Text.Interleaved.emit_structure state elt `Not_special
    ) signal

let convert ?convert_text ?progress ~options src ~dst =
  let convert_text =
    match convert_text with
    | Some f -> f
    | None ->
       let buf = buffer None in
       fun src -> Text.convert ~buf ~options src ~dst:String
  in
  Zip.map ?progress src (fun ~path ->
      match path with
      | "content.xml"
      | "styles.xml" (* contains header/footer *) ->
         Some (fun ~contents ->
             More_markup.transform
               ~flavor:`Xml
               ~transform:(
                 if options.interleaved
                 then odt_transform_interleaved ~convert_text
                 else odt_transform ~convert_text)
               contents ~dst:String)
      | _ -> None)
  |> write_out dst
