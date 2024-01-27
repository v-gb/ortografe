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

let iter_words1 src ~f = (
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
  Uutf.String.fold_utf_8 (fun () i -> function
      | `Malformed _ ->
         (match !state with
          | `Word _ -> flush i
          | `Out _ -> ())
      | `Uchar c ->
         if is_letter c
         then
           let nbytes = Uchar.utf_8_byte_length c in
            (match !state with
             | `Out _ -> flush i; state := `Word (i, i + nbytes)
             | `Word (start, _) -> state := `Word (start, i + nbytes))
         else
            if is_ascii c (function
                | '-' | ':' | '.' | '/' | '_' | '0'..'9' -> true
                | _ -> false)
            then ()
            else (match !state with
                  | `Word _ -> flush i
                  | `Out _ -> ())
    )
    () src;
  flush (String.length src);
)

let rec mem_substr str start len c =
  len > 0 &&
    (str.[start] = c || mem_substr str (start + 1) (len - 1) c)

let split_including_delims str c =
  let l = ref [] in
  let i = ref 0 in
  let got j =
    if !i <> j then (
      l := (!i, j - !i) :: !l;
      i := j
    )
  in
  while
    match String.index_from str !i c with
    | exception Not_found -> got (String.length str); false
    | j -> got j; got (j + 1); true
  do () done;
  List.rev !l

let iter_words src ~f ~f_mem = (
    iter_words1 src ~f:(fun what start len ->
        match what with
        | `Out -> f what start len
        | `Word ->
           if mem_substr src start len '-'
           && Uutf.String.fold_utf_8 (fun acc _ chunk ->
                  acc
                  && match chunk with
                     | `Malformed _ -> assert false
                     | `Uchar c ->
                        is_letter c || is_ascii c (function '-' -> true | _ -> false))
                true
                (String.sub src start len)
           then
             let sub = String.sub src start len in
             if f_mem sub
             then f what start len
             else
               List.iter
                 (fun (start', len') -> f what (start + start') len')
                 (split_including_delims sub '-')
           else f what start len
      )
  )

let string_of_uchars uchars =
  let nbytes = List.fold_left (fun acc c -> acc + Uchar.utf_8_byte_length c) 0 uchars in
  let b = Bytes.create nbytes in
  let i = ref 0 in
  List.iter (fun c ->
      i := !i + Bytes.set_utf_8_uchar b !i c
  ) uchars;
  Bytes.to_string b

let split_on_first_uchar src ~f =
  (* should probably split on grapheme cluster instead, but it probably doesn't matter
     given NFC normalization *)
  let utf_decode = String.get_utf_8_uchar src 0 in
  let src0 = Uchar.utf_decode_uchar utf_decode in
  match f src0 with
  | Some repl ->
     let src0bytes = Uchar.utf_decode_length utf_decode in
     Some (string_of_uchars repl ^ String.sub src src0bytes (String.length src - src0bytes))
  | None -> None

let depluralize w =
  if String.ends_with w ~suffix:"s"
  then Some (String.sub w 0 (String.length w - 1))
  else None

let pluralize w = w ^ "s"

let uncapitalize w =
  split_on_first_uchar w ~f:(fun w0 ->
      if Uucp.Case.is_upper w0
      then
        match Uucp.Case.Map.to_lower w0 with
        | `Uchars w0l -> Some w0l
        | _ -> None
      else None)

let capitalize w =
  split_on_first_uchar w ~f:(fun w0 ->
      if Uucp.Case.is_lower w0
      then
        match Uucp.Case.Map.to_upper w0 with
        | `Uchars w0u -> Some w0u
        | _ -> None
      else None)

let map_case w ~f =
  let b = Buffer.create (String.length w) in
  let add l = List.iter (fun u -> Buffer.add_utf_8_uchar b u) l in
  if
    Uutf.String.fold_utf_8 (fun bad _i -> function
        | `Malformed _ -> true
        | `Uchar c ->
           bad ||
             match f c with
             | `Uchars l -> add l; false
             | `Self -> true
      ) false w
  then None
  else Some (Buffer.contents b)

let lowercase w =
  map_case w ~f:(fun c ->
      if Uucp.Case.is_upper c
      then Uucp.Case.Map.to_lower c
      else `Self)

let uppercase w =
  map_case w ~f:(fun c ->
      if Uucp.Case.is_lower c
      then Uucp.Case.Map.to_upper c
      else `Self)

let iter_pure_text ~options src ~f =
  let dict = options.dict in
  let src = nfc src in
  iter_words src ~f_mem:(Hashtbl.mem dict) ~f:(fun what start len ->
    let w = String.sub src start len in
    match what with
    | `Out -> f w
    | `Word ->
       let wu, recapitalize =
         match uncapitalize w with
         | None -> w, (fun x -> x)
         | Some wu ->
            if Hashtbl.mem dict w
            then "", (fun x -> x)
            else
              match
                if options.convert_uppercase
                then lowercase w
                else None
              with
              | None -> wu, (fun w -> Option.value (capitalize w) ~default:w)
              | Some wl ->
                 if
                   match capitalize wl with
                   | None -> false
                   | Some c -> Hashtbl.mem dict c
                 then "", (fun x -> x)
                 else wl, (fun w -> Option.value (uppercase w) ~default:w)
       in
       match Hashtbl.find_opt dict wu with
       | Some res -> f (recapitalize res)
       | None ->
          match depluralize wu with
          | None -> f w
          | Some wu ->
             match Hashtbl.find_opt dict wu with
             | Some res -> f (recapitalize (pluralize res))
             | None -> f w
    )

let convert (type a) ?buf ~options src ~(dst : a out) : a =
  match dst with
  | String ->
     let b = buffer buf ~n:(String.length src) in
     iter_pure_text ~options src ~f:(Buffer.add_string b);
     Buffer.contents b
  | Channel ch ->
     iter_pure_text ~options src ~f:(Out_channel.output_string ch)
