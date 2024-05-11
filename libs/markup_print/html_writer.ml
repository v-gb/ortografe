(* This file is a modified copy of html_writer.ml.source sibling file,
   to remove all the cps stuff that prevents the code from working in javascript *)

let list_mem_string (s : string) l = List.exists (fun s' -> s' = s) l
let html_ns = Markup.Ns.html
let svg_ns = Markup.Ns.svg
let mathml_ns = Markup.Ns.mathml
let xml_ns = Markup.Ns.xml
let xmlns_ns = Markup.Ns.xmlns
let xlink_ns = Markup.Ns.xlink

let escape_attribute s =
  let buffer = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ -> ()
    | `Uchar c ->
      match Uchar.to_int c with
      | 0x0026 -> Buffer.add_string buffer "&amp;"
      | 0x00A0 -> Buffer.add_string buffer "&nbsp;"
      | 0x0022 -> Buffer.add_string buffer "&quot;"
      | _ -> Buffer.add_utf_8_uchar buffer c)
    () s;
  Buffer.contents buffer

let escape_text s =
  let buffer = Buffer.create (String.length s) in
  Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ -> ()
    | `Uchar c ->
      match Uchar.to_int c with
      | 0x0026 -> Buffer.add_string buffer "&amp;"
      | 0x00A0 -> Buffer.add_string buffer "&nbsp;"
      | 0x003C -> Buffer.add_string buffer "&lt;"
      | 0x003E -> Buffer.add_string buffer "&gt;"
      | _ -> Buffer.add_utf_8_uchar buffer c)
    () s;
  Buffer.contents buffer

let void_elements =
  ["area"; "base"; "basefont"; "bgsound"; "br"; "col"; "embed"; "frame"; "hr";
   "img"; "input"; "keygen"; "link"; "meta"; "param"; "source"; "track"; "wbr"]

let prepend_newline_for = ["pre"; "textarea"; "listing"]

let rec starts_with_newline = function
  | [] -> false
  | s::more ->
    if String.length s = 0 then starts_with_newline more
    else s.[0] = '\x0A'

let literal_text_elements =
  ["style"; "script"; "xmp"; "iframe"; "noembed"; "noframes"; "plaintext"]

let write ?(escape_attribute=escape_attribute) ?(escape_text=escape_text) signals dst =
  let open_elements = ref [] in

  let for_next_elt = ref `None in
  let flush () = for_next_elt := `None in

  let in_literal_text_element () =
    match !open_elements with
      | element :: _ -> List.mem element literal_text_elements
      | _ -> false in

  let emit_list l = List.iter dst l in

  signals (function
      | `Start_element ((ns, name') as name, attributes) ->
         flush ();
        let tag_name =
          match name with
          | ns, local_name
              when list_mem_string ns [html_ns; svg_ns; mathml_ns] ->
            local_name
          | ns, local_name when ns = xml_ns -> "xml:" ^ local_name
          | ns, local_name when ns = xmlns_ns -> "xmlns:" ^ local_name
          | ns, local_name when ns = xlink_ns -> "xlink:" ^ local_name
          | _, local_name -> (* An error. *) local_name
        in

        let attributes =
          attributes |> List.map (fun ((ns, local_name) as name, value) ->
            let name =
              match name with
              | "", _ -> local_name
              | _ when ns = xml_ns -> "xml:" ^ local_name
              | _, "xmlns" when ns = xmlns_ns -> "xmlns"
              | _ when ns = xmlns_ns -> "xmlns:" ^ local_name
              | _ when ns = xlink_ns -> "xlink:" ^ local_name
              | _ -> (* An error. *) local_name
            in
            name, value)
        in

        let rec prepend_attributes words = function
          | [] -> words
          | (name, value)::more ->
            prepend_attributes
              (" "::name::"=\""::(escape_attribute value)::"\""::words) more
        in

        let tag =
          "<"::tag_name::(prepend_attributes [">"] (List.rev attributes)) in

        let is_void = ns = html_ns && list_mem_string name' void_elements in

        emit_list tag;
        
        open_elements := tag_name :: !open_elements;
        for_next_elt := 
          (if is_void then `Void
           else if ns = html_ns && list_mem_string name' prepend_newline_for
           then `Prepend_newline
           else `None);

      | `End_element ->
         let for_next_elt = !for_next_elt in
         flush ();
         (begin match !open_elements with
          | [] -> ()
          | name::rest ->
             open_elements := rest;
             match for_next_elt with
             | `Void -> ()
             | `None | `Prepend_newline ->
                emit_list ["</"; name; ">"]
          end)

      | `Text ss ->
         (match !for_next_elt with
          | `Prepend_newline when starts_with_newline ss -> dst "\n"
          | `Prepend_newline | `None | `Void -> ());
         flush ();
        if List.for_all (fun s -> String.length s = 0) ss then
          ()
        else if in_literal_text_element () then
          emit_list ss
        else
          emit_list (List.map escape_text ss)

      | `Comment s ->
         flush ();
        emit_list ["<!--"; s; "-->"]

      | `PI (target, s) ->
         flush ();
        emit_list ["<?"; target; " "; s; ">"]

      | `Doctype _ as doctype ->
         flush ();
        emit_list [Markup.signal_to_string doctype]

      | `Xml _ ->
         flush ();
         ()

    )
