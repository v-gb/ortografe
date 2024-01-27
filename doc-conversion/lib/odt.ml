open Common

let odt_transform ~buf ~options signal =
  (* spec: http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html#__RefHeading__1415130_253892949 *)
  (* this won't work in many cases, due to words being split up by document structure *)
  let text_ns = "urn:oasis:names:tc:opendocument:xmlns:text:1.0" in
  let open Core in
  let stack = Stack.create () in
  let convert_text src = Text.convert ~buf ~options src ~dst:String in
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

let lsplit2_delim_right str ~on =
  let open Core in
  match String.index str on with
  | None -> None
  | Some i -> Some (String.prefix str i, String.suffix str (String.length str - i))

let rsplit2_delim_left str ~on =
  let open Core in
  match String.rindex str on with
  | None -> None
  | Some i -> Some (String.prefix str (i + 1), String.suffix str (String.length str - i - 1))

let odt_transform2 ~buf ~options signal =
  let text_ns = "urn:oasis:names:tc:opendocument:xmlns:text:1.0" in
  let open Core in
  let stack = Stack.create () in
  let waiting = Queue.create () in
  let emit_or_wait elt =
    if Queue.is_empty waiting
    then [elt]
    else (Queue.enqueue waiting (`Structure elt); [])
  in
  let handle_one_result s =
    match Queue.dequeue_exn waiting with
    | `Structure _ -> failwith "bug: dequeued got a structure instead of a text"
    | `Text ->
       let res = ref [`Text [s]] in
       while
         match Queue.peek waiting with
         | None | Some `Text -> false
         | Some (`Structure structure) ->
            Queue.dequeue_and_ignore_exn waiting;
            res := structure :: !res;
            true
       do () done;
       List.rev !res
  in
  let handle_result l = List.concat_map l ~f:handle_one_result in
  let emit =
    let r : (string * string * string list) option ref = ref None in
    let flush () =
      match !r with
      | None -> None
      | Some (src_left', left, right) ->
         let right = List.rev right in
         r := None;
         let left' = Text.convert ~buf ~options left ~dst:String in
         let right_concat' = Text.convert ~buf ~options (String.concat right) ~dst:String in
         let right' =
           (* we could also do a character-wise diff2 to line up the strings again *)
           let i_right = ref 0 in
           let count = List.length right in
           List.mapi right ~f:(fun i s ->
               if i + 1 = count
               then String.drop_prefix right_concat' !i_right
               else
                 let i_after = min (String.length right_concat') (!i_right + String.length s) in
                 let res = String.sub right_concat' ~pos:!i_right ~len:(i_after - !i_right) in
                 i_right := i_after;
                 res)
         in
         Some (src_left' ^ left' ^ List.hd_exn right', List.tl_exn right')
    in
    let flush_as_list () =
      match flush () with
      | None -> []
      | Some (hd, tl) -> hd :: tl
    in
    let text_when_no_buffer src_left' src =
      match rsplit2_delim_left src ~on:' ' with
      | None -> r := Some (src_left', "", [src]); []
      | Some (_, "") -> [src_left' ^ Text.convert ~buf ~options src ~dst:String]
      | Some (left, right) -> r := Some (src_left', left, [right]); []
    in
    fun event ->
    let res =
      match event with
      | `Text src ->
         (match !r with
          | None -> text_when_no_buffer "" src
          | Some (r_left', r_left, r_right) ->
             match lsplit2_delim_right src ~on:' ' with
             | None -> r := Some (r_left', r_left, src :: r_right); []
             | Some ("", _) ->
                let l1 = flush_as_list () in
                let l2 = text_when_no_buffer "" src in
                l1 @ l2
             | Some (src_left, src_right) ->
                r := Some (r_left', r_left, src_left :: r_right);
                let l1, src_left' =
                  let res_left, res_right = Option.value_exn (flush ()) in
                  res_left :: List.drop_last_exn res_right, List.last_exn res_right
                in
                let l2 = text_when_no_buffer src_left' src_right in
                l1 @ l2
         )
      | `Flush | `Space -> flush_as_list ()
    in
    handle_result res
  in
  Markup.transform (fun () elt ->
      match elt with
      | `Start_element (ns_tag, _) ->
         Stack.push stack ns_tag;
         emit_or_wait elt, Some ()
      | `End_element ->
         let l =
           match Stack.pop stack with
           | Some (ns, ("h" | "p")) when String.(=) ns text_ns -> emit `Flush
           | Some (ns, ("s" | "tab" | "line-break")) when String.(=) ns text_ns -> emit `Space
           | _ -> []
         in
         l @ emit_or_wait elt, Some ()
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
         Queue.enqueue waiting `Text;
         emit (`Text (String.concat strs)), Some ()
       | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ ->
         emit_or_wait elt, Some ()
    ) () signal

let convert ?(impl = false) ?buf ~options src ~dst =
  let buf = buffer buf in
  Zip.map src (fun member contents ->
      match Zipc.Member.path member with
      | "content.xml"
      | "styles.xml" (* contains header/footer *) ->
         Some (More_markup.transform ~flavor:`Xml
                 ~transform:(
                   if impl
                   then odt_transform2 ~buf ~options
                   else odt_transform ~buf ~options)
                 (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst
