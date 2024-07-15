open Common

let nfc str =
  (* this transforms invalid utf8 into replacement chars, maybe we should rewrite the loop
     to avoid that *)
  Uunf_string.normalize_utf_8 `NFC str

let is_letter c =
  (* http://www.unicode.org/reports/tr44/#General_Category_Values *)
  match Uucp.Gc.general_category c with
  | `Ll | `Lm | `Lo | `Lt | `Lu -> true
  | _ -> false

let is_ascii c f = Uchar.is_char c && f (Uchar.to_char c)

let iter_words1 src ~f =
  let state = ref (`Out 0) in
  let flush j =
    (match !state with
    | `Out i -> if i <> j then f `Out i (j - i)
    | `Word (start, end_) ->
        assert (start < end_);
        f `Word start (end_ - start);
        if end_ <> j then f `Out end_ (j - end_));
    state := `Out j
  in
  Uutf.String.fold_utf_8
    (fun () i -> function
      | `Malformed _ -> ( match !state with `Word _ -> flush i | `Out _ -> ())
      | `Uchar c -> (
          if is_letter c
          then
            let nbytes = Uchar.utf_8_byte_length c in
            match !state with
            | `Out _ ->
                flush i;
                state := `Word (i, i + nbytes)
            | `Word (start, _) -> state := `Word (start, i + nbytes)
          else if is_ascii c (function
                    | '-' | ':' | '.' | '/' | '_' | '0' .. '9' -> true
                    | _ -> false)
          then ()
          else match !state with `Word _ -> flush i | `Out _ -> ()))
    () src;
  flush (String.length src)

let rec mem_substr str start len c =
  len > 0 && (str.[start] = c || mem_substr str (start + 1) (len - 1) c)

let split_including_delims str c =
  let l = ref [] in
  let i = ref 0 in
  let got j =
    if !i <> j
    then (
      l := (!i, j - !i) :: !l;
      i := j)
  in
  while
    match String.index_from str !i c with
    | exception Not_found ->
        got (String.length str);
        false
    | j ->
        got j;
        got (j + 1);
        true
  do
    ()
  done;
  List.rev !l

let iter_words src ~f ~f_mem =
  iter_words1 src ~f:(fun what start len ->
      match what with
      | `Out -> f what start len
      | `Word ->
          if mem_substr src start len '-'
             && Uutf.String.fold_utf_8
                  (fun acc _ chunk ->
                    acc
                    &&
                    match chunk with
                    | `Malformed _ -> assert false
                    | `Uchar c ->
                        is_letter c || is_ascii c (function '-' -> true | _ -> false))
                  true (String.sub src start len)
          then
            let sub = String.sub src start len in
            if f_mem sub
            then f what start len
            else
              List.iter
                (fun (start', len') -> f what (start + start') len')
                (split_including_delims sub '-')
          else f what start len)

let string_of_uchars uchars =
  let nbytes = List.fold_left (fun acc c -> acc + Uchar.utf_8_byte_length c) 0 uchars in
  let b = Bytes.create nbytes in
  let i = ref 0 in
  List.iter (fun c -> i := !i + Bytes.set_utf_8_uchar b !i c) uchars;
  Bytes.to_string b

let split_on_first_uchar src ~f =
  (* should probably split on grapheme cluster instead, but it probably doesn't matter
     given NFC normalization *)
  if String.length src = 0
  then None
  else
    let utf_decode = String.get_utf_8_uchar src 0 in
    let src0 = Uchar.utf_decode_uchar utf_decode in
    match f src0 with
    | Some repl ->
        let src0bytes = Uchar.utf_decode_length utf_decode in
        Some
          (string_of_uchars repl
          ^ String.sub src src0bytes (String.length src - src0bytes))
    | None -> None

let depluralize w =
  if String.ends_with w ~suffix:"s"
  then Some (String.sub w 0 (String.length w - 1))
  else None

let pluralize w = w ^ "s"

let uncapitalize w =
  split_on_first_uchar w ~f:(fun w0 ->
      if Uucp.Case.is_upper w0
      then match Uucp.Case.Map.to_lower w0 with `Uchars w0l -> Some w0l | _ -> None
      else None)

let capitalize w =
  split_on_first_uchar w ~f:(fun w0 ->
      if Uucp.Case.is_lower w0
      then match Uucp.Case.Map.to_upper w0 with `Uchars w0u -> Some w0u | _ -> None
      else None)

let map_case w ~f =
  let b = Buffer.create (String.length w) in
  let add l = List.iter (fun u -> Buffer.add_utf_8_uchar b u) l in
  if Uutf.String.fold_utf_8
       (fun bad _i -> function
         | `Malformed _ -> true
         | `Uchar c -> (
             bad
             ||
             match f c with
             | `Uchars l ->
                 add l;
                 false
             | `Self -> true))
       false w
  then None
  else Some (Buffer.contents b)

let lowercase w =
  map_case w ~f:(fun c ->
      if Uucp.Case.is_upper c then Uucp.Case.Map.to_lower c else `Self)

let uppercase_as_much_as_possible w =
  (* ortograf.net uses "+" as a "letter". So do a best effort uppercasing. *)
  map_case w ~f:(fun c ->
      match Uucp.Case.Map.to_upper c with
      | `Self -> `Uchars [ c ]
      | `Uchars _ as uchars -> uchars)

let mem dict x = Option.is_some (dict x)

let iter_pure_text ~options src ~f =
  let dict = options.dict in
  let src = nfc src in
  iter_words src ~f_mem:(mem dict) ~f:(fun what start len ->
      let w = String.sub src start len in
      match what with
      | `Out -> f w
      | `Word -> (
          let wu, recapitalize =
            match uncapitalize w with
            | None -> (w, fun x -> x)
            | Some wu -> (
                match dict w with
                | Some "" -> ("", fun x -> x)
                | Some _ -> (w, fun x -> x)
                | None -> (
                    match if options.convert_uppercase then lowercase w else None with
                    | None -> (wu, fun w -> capitalize w ||? w)
                    | Some wl ->
                        if match capitalize wl with
                           | None -> false
                           | Some c -> mem dict c
                        then ("", fun x -> x)
                        else (wl, fun w -> uppercase_as_much_as_possible w ||? w)))
          in
          match dict wu with
          | Some "" -> f w
          | Some res -> f (recapitalize res)
          | None -> (
              match if options.plurals_in_s then depluralize wu else None with
              | None -> f w
              | Some wu -> (
                  match dict wu with
                  | Some "" -> f w
                  | Some res -> f (recapitalize (pluralize res))
                  | None -> f w))))

let convert (type a) ?buf ?progress:_ ~options src ~(dst : a out) : a =
  match dst with
  | Ignore -> ()
  | String ->
      let b = buffer buf ~n:(String.length src) in
      iter_pure_text ~options src ~f:(Buffer.add_string b);
      Buffer.contents b
  | Channel ch -> iter_pure_text ~options src ~f:(Out_channel.output_string ch)

module Interleaved = struct
  open Core

  let lsplit2_delim_right str ~on =
    match String.index str on with
    | None -> None
    | Some i -> Some (String.prefix str i, String.suffix str (String.length str - i))

  let rsplit2_delim_left str ~on =
    match String.rindex str on with
    | None -> None
    | Some i ->
        Some (String.prefix str (i + 1), String.suffix str (String.length str - i - 1))

  type 'a t =
    { waiting : [ `Structure of 'a | `Text ] Queue.t
    ; mutable emit_state : (string * string * string list) option
    ; embed_text : string -> 'a
    ; convert_text : string -> string
    }

  let create ~embed ~convert =
    { waiting = Queue.create ()
    ; emit_state = None
    ; embed_text = embed
    ; convert_text = convert
    }

  let handle_one_result t s =
    match Queue.dequeue_exn t.waiting with
    | `Structure _ -> failwith "bug: dequeued got a structure instead of a text"
    | `Text ->
        let res = ref [ t.embed_text s ] in
        while
          match Queue.peek t.waiting with
          | None | Some `Text -> false
          | Some (`Structure structure) ->
              Queue.dequeue_and_ignore_exn t.waiting;
              res := structure :: !res;
              true
        do
          ()
        done;
        List.rev !res

  let handle_result t l = List.concat_map l ~f:(handle_one_result t)

  let flush t =
    match t.emit_state with
    | None -> None
    | Some (src_left', left, right) ->
        let right = List.rev right in
        t.emit_state <- None;
        let left' = t.convert_text left in
        let right_concat' = t.convert_text (String.concat right) in
        let right' =
          (* we could also do a character-wise diff2 to line up the strings again.
             Or at least line up the longest prefix and longest suffix, and only then
             reassign characters left-to-right in a greedy fashion. *)
          let i_right = ref 0 in
          let count = List.length right in
          List.mapi right ~f:(fun i s ->
              if i + 1 = count
              then String.drop_prefix right_concat' !i_right
              else
                let i_after =
                  min (String.length right_concat') (!i_right + String.length s)
                in
                let res =
                  String.sub right_concat' ~pos:!i_right ~len:(i_after - !i_right)
                in
                i_right := i_after;
                res)
        in
        Some (src_left' ^ left' ^ List.hd_exn right', List.tl_exn right')

  let flush_as_list t = match flush t with None -> [] | Some (hd, tl) -> hd :: tl

  let text_when_no_emit_state t src_left' src =
    assert (Option.is_none t.emit_state);
    match rsplit2_delim_left src ~on:' ' with
    | None ->
        t.emit_state <- Some (src_left', "", [ src ]);
        []
    | Some (_, "") -> [ src_left' ^ t.convert_text src ]
    | Some (left, right) ->
        t.emit_state <- Some (src_left', left, [ right ]);
        []

  let emit_text t src =
    Queue.enqueue t.waiting `Text;
    let res =
      match t.emit_state with
      | None -> text_when_no_emit_state t "" src
      | Some (r_left', r_left, r_right) -> (
          match lsplit2_delim_right src ~on:' ' with
          | None ->
              t.emit_state <- Some (r_left', r_left, src :: r_right);
              []
          | Some ("", _) ->
              let l1 = flush_as_list t in
              let l2 = text_when_no_emit_state t "" src in
              l1 @ l2
          | Some (src_left, src_right) ->
              t.emit_state <- Some (r_left', r_left, src_left :: r_right);
              let l1, src_left' =
                let res_left, res_right = Option.value_exn (flush t) in
                (res_left :: List.drop_last_exn res_right, List.last_exn res_right)
              in
              let l2 = text_when_no_emit_state t src_left' src_right in
              l1 @ l2)
    in
    handle_result t res

  let emit_structure_not_special t a =
    if Queue.is_empty t.waiting
    then [ a ]
    else (
      Queue.enqueue t.waiting (`Structure a);
      [])

  let emit_structure t a effect =
    match effect with
    | `Not_special -> emit_structure_not_special t a
    | `Flush | `Space -> handle_result t (flush_as_list t) @ [ a ]
end
