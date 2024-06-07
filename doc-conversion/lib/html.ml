open! Common
open struct module Markup = Markup_t end

let html_transform ~convert_text signal =
  (* maybe we should check the tag ns for the xhtml case *)
  let notranslate_pattern = Core.String.Search_pattern.create "notranslate" in
  let notranscribe_pattern = Core.String.Search_pattern.create "notranscribe" in
  let stack = Core.Stack.create () in
  let hide_current = ref false in
  Markup.map (fun elt ->
      (match elt with
       | `Start_element ((_, tag), attributes) ->
          let hide =
            match tag with
            | "code" | "script" | "noscript" | "style" | "textarea" -> true
            | _ ->
               List.exists (fun ((_, attr), _value) ->
                   match attr with
                   | "class" ->
                      Core.String.Search_pattern.matches notranslate_pattern attr
                      || Core.String.Search_pattern.matches notranscribe_pattern attr
                   | "contenteditable" -> true
                   | _ -> false) attributes
          in
          Core.Stack.push stack (tag, !hide_current);
          hide_current := hide;
       | `End_element ->
          (match Core.Stack.pop stack with
           | None -> ()
           | Some (_, hide) -> hide_current := hide)
       | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> ());
      if not !hide_current
      then More_markup.text_elt ~convert_text elt
      else elt)
    signal

let convert ?convert_text ?progress:_ ~options src ~dst =
  let convert_text =
    convert_text
    ||?
      let buf = Common.buffer None in
      fun src -> Text.convert ~buf ~options src ~dst:String
  in
  More_markup.transform
    ~flavor:`Html src ~dst ~transform:(html_transform ~convert_text)

let convert_xhtml ?convert_text ?progress:_ ~options src ~dst =
  let convert_text =
    convert_text
    ||?
      let buf = Common.buffer None in
      fun src -> Text.convert ~buf ~options src ~dst:String
  in
  More_markup.transform
    ~flavor:`Xml src ~dst ~transform:(html_transform ~convert_text)
