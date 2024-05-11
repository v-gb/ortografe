(* This file is a modified copy of xml_writer.ml.source sibling file,
   to remove all the cps stuff that prevents the code from working in javascript *)



let escape s =
  let buffer = Buffer.create (String.length s) in
  String.iter (function
    | '"' -> Buffer.add_string buffer "&quot;"
    | '&' -> Buffer.add_string buffer "&amp;"
    | '\'' -> Buffer.add_string buffer "&apos;"
    | '<' -> Buffer.add_string buffer "&lt;"
    | '>' -> Buffer.add_string buffer "&gt;"
    | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let attribute_strings end_ attributes =
  let rec prepend_attributes words = function
    | [] -> words
    | (name, value)::more ->
      prepend_attributes
        (" "::name::"=\""::(escape value)::"\""::words) more
  in

  prepend_attributes (Base.Option.to_list end_) (List.rev attributes)

let write (signals : (Markup.signal -> unit) -> unit) dst =

  let stack = Base.Stack.create () in
  let flush_open () =
    match Base.Stack.top stack with
    | Some (_, r, _) when not !r -> dst ">"; r := true
    | _ -> ()
  in
  let prefixed map (ns, name) =
    if name = "xmlns"
    && ns = Markup.Ns.xmlns
    then name
    else (Base.Map.find map ns ||? "") ^ name in

  let emit_list l = List.iter dst l in

  signals (function
      | `Start_element (name, attributes) ->
         flush_open ();
         let map =
           match Base.Stack.top stack with
           | None -> Base.Map.of_alist_exn (module Base.String)
                       [ Markup.Ns.xmlns, "xmlns:"
                       ; Markup.Ns.xml, "xml:"
                       ]
           | Some (_, _, map) -> map
         in
         let map =
           List.fold_left (fun acc ((ns, tag), value) ->
               if ns = Markup.Ns.xmlns
               then
                 let prefix =
                   match tag with
                   | "xmlns" -> ""
                   | _ -> tag ^ ":"
                 in
                 Base.Map.set acc ~key:value ~data:prefix
               else acc) map attributes
         in
         let formatted_name = prefixed map name in
         let formatted_attributes =
           List.map (fun (name, value) -> prefixed map name, value)
             attributes
         in
          let tag =
            "<"::formatted_name::(attribute_strings None formatted_attributes)
          in

          emit_list tag;
          Base.Stack.push stack (formatted_name, ref false, map)

      | `End_element ->
        begin match Base.Stack.pop stack with
        | None -> ()
        | Some (formatted_name, opened, _) ->
           if !opened
           then emit_list ["</"; formatted_name; ">"]
           else dst "/>"
        end

      | `Text ss ->
         flush_open ();
        if List.for_all (fun s -> String.length s = 0) ss then
          ()
        else
          emit_list (List.map escape ss)

      | `Xml {version; encoding; standalone} ->
         flush_open ();
        let attributes =
          match standalone with
          | None -> []
          | Some true -> ["standalone", "yes"]
          | Some false -> ["standalone", "no"]
        in

        let attributes =
          match encoding with
          | None -> attributes
          | Some encoding -> ("encoding", encoding)::attributes
        in

        let attributes = ("version", version)::attributes in

        let declaration = "<?xml"::(attribute_strings (Some "?>") attributes) in

        emit_list declaration

      | `Doctype {raw_text; _} ->
         flush_open ();
        begin match raw_text with
        | None -> ()
        | Some text -> emit_list ["<!DOCTYPE "; text; ">"]
        end

      | `PI (target, s) ->
         flush_open ();
        emit_list ["<?"; target; " "; s; "?>"]

      | `Comment s ->
         flush_open ();
        emit_list ["<!--"; s; "-->"]
    )
