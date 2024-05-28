let bytes_blit_string ~src ~src_pos ~dst ~dst_pos ~len =
  Bytes.blit_string src src_pos dst dst_pos len
;;

open StdLabels

let default_indent = ref 1

let must_escape str =
  let len = String.length str in
  len = 0
  ||
    let rec loop str len ix =
      match str.[ix] with
      | '"' | '(' | ')' | ';' | '\\' -> true
      | '|' ->
         let next = ix + 1 in
         next < len
         && (Char.equal str.[next] '#' || loop str len next)
      | '#' ->
         let next = ix + 1 in
         next < len
         && (Char.equal str.[next] '|' || loop str len next)
      | '\000' .. '\032' -> true
      | '\127' .. '\255' ->
         let utf_decode = String.get_utf_8_uchar str ix in
         let is_alphabetic =
           Uchar.utf_decode_is_valid utf_decode
           && Uucp.Alpha.is_alphabetic (Uchar.utf_decode_uchar utf_decode)
         in
         not is_alphabetic
         || let next = ix + Uchar.utf_decode_length utf_decode in
            next < len && loop str len next
      | _ ->
         let next = ix + 1 in
         next < len && loop str len next
    in
    loop str len 0
;;

let escaped s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n
    := !n
       +
         match String.unsafe_get s i with
         | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | ' ' .. '~' -> 1
         | _ -> 4
  done;
  if !n = String.length s
  then s
  else (
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to String.length s - 1 do
      (match String.unsafe_get s i with
       | ('\"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n c
       | '\n' ->
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n 'n'
       | '\t' ->
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n 't'
       | '\r' ->
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n 'r'
       | '\b' ->
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n 'b'
       | ' ' .. '~' as c -> Bytes.unsafe_set s' !n c
       | c ->
          let a = Char.code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + (a / 100)));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + (a / 10 mod 10)));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + (a mod 10))));
      incr n
    done;
    Bytes.unsafe_to_string s')
;;

let esc_str str =
  let estr = escaped str in
  let elen = String.length estr in
  let res = Bytes.create (elen + 2) in
  bytes_blit_string ~src:estr ~src_pos:0 ~dst:res ~dst_pos:1 ~len:elen;
  Bytes.unsafe_set res 0 '"';
  Bytes.unsafe_set res (elen + 1) '"';
  Bytes.unsafe_to_string res
;;

let index_of_newline str start = String.index_from_opt str start '\n'

let get_substring str index end_pos_opt =
  let end_pos =
    match end_pos_opt with
    | None -> String.length str
    | Some end_pos -> end_pos
  in
  String.sub str ~pos:index ~len:(end_pos - index)
;;

let is_one_line str =
  match index_of_newline str 0 with
  | None -> true
  | Some index -> index + 1 = String.length str
;;

let pp_hum_maybe_esc_str ppf str =
  if not (must_escape str)
  then Format.pp_print_string ppf str
  else if is_one_line str
  then Format.pp_print_string ppf (esc_str str)
  else (
    let rec loop index =
      let next_newline = index_of_newline str index in
      let next_line = get_substring str index next_newline in
      Format.pp_print_string ppf (escaped next_line);
      match next_newline with
      | None -> ()
      | Some newline_index ->
         Format.pp_print_string ppf "\\";
         Format.pp_force_newline ppf ();
         Format.pp_print_string ppf "\\n";
         loop (newline_index + 1)
    in
    Format.pp_open_box ppf 0;
    (* the leading space is to line up the lines *)
    Format.pp_print_string ppf " \"";
    loop 0;
    Format.pp_print_string ppf "\"";
    Format.pp_close_box ppf ())
;;

let rec pp_hum_indent indent ppf = function
  | Sexplib.Sexp.Atom str -> pp_hum_maybe_esc_str ppf str
  | List (h :: t) ->
     Format.pp_open_box ppf indent;
     Format.pp_print_string ppf "(";
     pp_hum_indent indent ppf h;
     pp_hum_rest indent ppf t
  | List [] -> Format.pp_print_string ppf "()"

and pp_hum_rest indent ppf = function
  | h :: t ->
     Format.pp_print_space ppf ();
     pp_hum_indent indent ppf h;
     pp_hum_rest indent ppf t
  | [] ->
     Format.pp_print_string ppf ")";
     Format.pp_close_box ppf ()
;;

let to_buffer_hum ~buf ?(indent = !default_indent) sexp =
  let ppf = Format.formatter_of_buffer buf in
  Format.fprintf ppf "%a@?" (pp_hum_indent indent) sexp
;;

let buffer () = Buffer.create 1024

let to_string_hum ?indent sexp =
  let buf = buffer () in
  to_buffer_hum ?indent sexp ~buf;
  Buffer.contents buf
;;

let () =
  Printexc.register_printer (fun exn ->
      match Sexplib.Conv.sexp_of_exn_opt exn with
      | None -> None
      | Some sexp -> Some (to_string_hum ~indent:2 sexp))
;;

let pp ppf t =
  match Sexplib.Conv.sexp_of_exn_opt t with
  | Some sexp -> pp_hum_indent 2 ppf sexp
  | None -> Stdlib.Format.pp_print_string ppf (Stdlib.Printexc.to_string t)
;;

let print_with_backtrace exc raw_backtrace =
  Stdlib.Format.eprintf "@[<2>Uncaught exception:@\n@\n@[%a@]@]@\n@." pp exc;
  if Stdlib.Printexc.backtrace_status ()
  then Stdlib.Printexc.print_raw_backtrace Stdlib.stderr raw_backtrace;
  Stdlib.flush Stdlib.stderr
;;

let linkme = Stdlib.Printexc.set_uncaught_exception_handler print_with_backtrace
