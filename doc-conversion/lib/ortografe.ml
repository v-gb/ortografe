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

let iter_words src ~f = (
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
             List.iter
               (fun (start', len') -> f what (start + start') len')
               (split_including_delims (String.sub src start len) '-')
           else f what start len
      )
  )

let load_dict () =
  let l = String.split_on_char '\n' Dict.value in
  let h = Hashtbl.create 25000 in
  List.iter (fun str ->
      match String.split_on_char ',' str with
      | [] | [""] -> ()
      | [ a; b ] ->
         Hashtbl.replace h a b
      | _ -> failwith ("wtf " ^ str)) l;
  h

let string_of_uchar uchar =
  let nbytes = Uchar.utf_8_byte_length uchar in
  let b = Bytes.create nbytes in
  ignore (Bytes.set_utf_8_uchar b 0 uchar : int);
  Bytes.to_string b

let split_on_first_uchar src ~f =
  (* should probably split on grapheme cluster instead, but it probably doesn't matter
     given NFC normalization *)
  let utf_decode = String.get_utf_8_uchar src 0 in
  let src0 = Uchar.utf_decode_uchar utf_decode in
  match f src0 with
  | Some src0_new ->
     if src0 <> src0_new
     then
       let src0bytes = Uchar.utf_decode_length utf_decode in
       Some (string_of_uchar src0_new ^ String.sub src src0bytes (String.length src - src0bytes))
     else None
  | None -> None  

let depluralize w =
  if String.ends_with w ~suffix:"s"
  then Some (String.sub w 0 (String.length w - 1))
  else None

let pluralize w = w ^ "s"

let uncapitalize w =
  split_on_first_uchar w ~f:(fun w0 ->
      match Uucp.Case.Map.to_lower w0 with
      | `Uchars (w0l :: _) -> Some w0l
      | _ -> None)

let capitalize w =
  split_on_first_uchar w ~f:(fun w0 ->
      match Uucp.Case.Map.to_upper w0 with
      | `Uchars (w0u :: _) -> Some w0u
      | _ -> None)

let iter_pure_text src ~f =
  let src = nfc src in
  let dict = load_dict () in
  iter_words src ~f:(fun what start len ->
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
              else wu, (fun w -> Option.value (capitalize w) ~default:w)
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

let pure_text src =
  let b = Buffer.create 17 in
  iter_pure_text src ~f:(Buffer.add_string b);
  Buffer.contents b

let main () =
  let src =
    In_channel.input_all
      (if Array.length Sys.argv <= 1
       then In_channel.stdin
       else open_in (Sys.argv.(1)))
  in
  iter_pure_text src ~f:print_string
