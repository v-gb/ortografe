open! Common

let html_transform ~buf ~options signal =
  (* maybe we should check the tag ns for the xhtml case *)
  let notranslate_pattern = Core.String.Search_pattern.create "notranslate" in
  let notranscribe_pattern = Core.String.Search_pattern.create "notranscribe" in
  let stack = Core.Stack.create () in
  let hide_current = ref false in
  let convert_text src = Text.convert ~buf ~options src ~dst:String in
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

let convert ?debug ?pp ?buf ~options src ~dst =
  let buf = Common.buffer buf in
  More_markup.transform ?debug ?pp ~flavor:`Html src ~dst ~transform:(html_transform ~buf ~options)

let convert_xhtml ?debug ?pp ?buf ~options src ~dst =
  let buf = Common.buffer buf in
  More_markup.transform ?debug ?pp ~flavor:`Xml src ~dst ~transform:(html_transform ~buf ~options)
