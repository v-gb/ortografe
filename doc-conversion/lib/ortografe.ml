module Dyn_protect = struct
  open Core
  type t = (unit -> unit) Stack.t

  let add (t : t) ~finally =
    Stack.push t finally

  exception Exns of exn list
  let with_ f =
    let t = Stack.create () in
    Fun.protect ~finally:(fun () ->
        let e = ref [] in
        while
          match Stack.pop t with
          | None -> false
          | Some f ->
             (try f ()
              with exn -> e := exn :: !e);
             true
        do () done;
        match List.rev !e with
        | [] -> ()
        | [exn] -> raise exn
        | exns -> raise (Exns exns)
      ) (fun () -> f t)
end

type 'a out =
  | Channel : Out_channel.t -> unit out
  | String : string out

let write_out (type a) (out : a out) (string : string) : a =
  match out with
  | String -> string
  | Channel ch -> Out_channel.output_string ch string

let markup_output (type a) (out : a out) : ((char, Markup.sync) Markup.stream -> a) =
  match out with
  | String -> Markup.to_string
  | Channel ch -> Markup.to_channel ch
  
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

let dict = lazy (load_dict ())

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
  let dict = Lazy.force dict in
  let src = nfc src in
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

let buffer ?(n = 123) buf =
  match buf with
  | Some buf -> Buffer.clear buf; buf
  | None -> Buffer.create n

let pure_text (type a) ?buf src ~(dst : a out) : a =
  match dst with
  | String ->
     let b = buffer buf ~n:(String.length src) in
     iter_pure_text src ~f:(Buffer.add_string b);
     Buffer.contents b
  | Channel ch ->
     iter_pure_text src ~f:(Out_channel.output_string ch)

let maybe_debug_signal ?(debug = false) signals =
  if debug
  then Markup.map (fun elt ->
           print_endline (Markup.signal_to_string elt);
           elt) signals
  else signals

let maybe_pp_signal ?(pp = false) signals =
  if pp then Markup.pretty_print signals else signals

let text_elt ~buf = function
  | `Text strs -> `Text (List.map (pure_text ~buf ~dst:String) strs)
  | elt -> elt

let html_transform ~buf signal =
  (* maybe we should check the tag ns for the xhtml case *)
  let notranslate_pattern = Core.String.Search_pattern.create "notranslate" in
  let stack = Core.Stack.create () in
  let hide_current = ref false in
  Markup.map (fun elt ->
      (match elt with
       | `Start_element ((_, tag), attributes) ->
          let hide =
            match tag with
            | "code" | "script" | "style" | "textarea" -> true
            | _ ->
               List.exists (fun ((_, attr), _value) ->
                   match attr with
                   | "class" -> Core.String.Search_pattern.matches notranslate_pattern attr
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
      then text_elt ~buf elt
      else elt)
    signal

let html ?buf ?debug ?pp src ~dst =
  (* https://v3.ocaml.org/p/markup/latest/doc/Markup/index.html
     Note: no implicit closing of tags *)
  let buf = buffer buf in
  Markup.parse_html
    (Markup.string src)
  |> Markup.signals
  |> maybe_debug_signal ?debug
  |> html_transform ~buf
  |> maybe_pp_signal ?pp
  |> Markup.write_html
  |> markup_output dst

let xml ?debug ?pp ~transform src ~dst =
  Markup.parse_xml
    (Markup.string src)
  |> Markup.signals
  |> maybe_debug_signal ?debug
  |> transform
  |> maybe_pp_signal ?pp
  |> Markup.write_xml
  |> markup_output dst

let xhtml ?buf ?debug ?pp src ~dst =
  let buf = buffer buf in
  xml ?debug ?pp src ~dst ~transform:(html_transform ~buf)

let docx_transform ~buf signal =
  let stack = Core.Stack.create () in
  Markup.map (fun elt ->
      (match elt with
       | `Start_element (ns_tag, _) ->
          Core.Stack.push stack ns_tag;
       | `End_element ->
          ignore (Core.Stack.pop stack)
       | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> ());
      match Core.Stack.top stack with
      | Some ("http://schemas.openxmlformats.org/wordprocessingml/2006/main", "t") ->
         text_elt ~buf elt
      | _ -> elt
    ) signal

let docx_xml ?buf ?debug ?pp src ~dst =
  let buf = buffer buf in
  xml ?debug ?pp src ~dst ~transform:(docx_transform ~buf)

let map_zip src f =
  let zipc = Zipc.of_binary_string src |> Core.Result.ok_or_failwith in
  let new_zipc =
    Zipc.fold (fun member acc ->
        match Zipc.Member.kind member with
        | Dir -> acc
        | File file ->
           match f member file (fun () -> Zipc.File.to_binary_string file |> Core.Result.ok_or_failwith) with
           | None -> acc
           | Some new_content ->
              let new_file =
                match Zipc.File.compression file with
                | Stored -> Zipc.File.stored_of_binary_string new_content |> Core.Result.ok_or_failwith
                | Deflate -> Zipc.File.deflate_of_binary_string new_content |> Core.Result.ok_or_failwith
                | _ -> failwith "unknown compression type, should have failed earlier"
              in
              let new_member =
                Zipc.Member.make
                  ~mtime:(Zipc.Member.mtime member)
                  ~mode:(Zipc.Member.mode member)
                  ~path:(Zipc.Member.path member)
                  (File new_file)
                |> Core.Result.ok_or_failwith
              in
              Zipc.add new_member acc
              
      )
      zipc zipc
  in
  Zipc.to_binary_string new_zipc |> Core.Result.ok_or_failwith

let max_size = ref 300_000_000
let count_size () =
  let total_size = ref 0 in
  (fun file ->
    (* Not the best, but it helps with non-advertisarial users, and
       low-tech advertisarial users.  We could ask zipc to respect the
       compressed size as well (or fork to make it do so, or at least
       provide a smaller max size). *)
    total_size := !total_size + Zipc.File.decompressed_size file;
    if !total_size > !max_size
    then failwith "files in zip too large")

let read_whole_zip src =
  (* A variation of docx so we can test for zip bombs without having
     to ensure we build a zip bomb with exactly the right filename
     inside. *)
  let count = count_size () in
  map_zip src (fun _ file contents ->
      count file;
      Some (contents ()))

let docx ?buf ?debug ?pp src ~dst =
  let buf = buffer buf in
  let count = count_size () in
  map_zip src (fun member file contents ->
      match Zipc.Member.path member with
      | "word/document.xml" ->
         count file;
         Some (docx_xml ~buf ?debug ?pp (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let docx_document src =
  let zipc = Zipc.of_binary_string src |> Core.Result.ok_or_failwith in
  match Zipc.find "word/document.xml" zipc with
  | None -> "<absent>"
  | Some member ->
     match Zipc.Member.kind member with
     | Dir -> "<directory>"
     | File file -> Zipc.File.to_binary_string file |> Core.Result.ok_or_failwith

let sys_command_exn str =
  let i = Sys.command str in
  if i <> 0
  then failwith (str ^ " exited with code " ^ Int.to_string i)

let doc ?buf ?debug ?pp src ~dst =
  let buf = buffer buf in
  (* should put a time limit and memory, perhaps with the cgroup exe? *)
  let d = Filename.temp_dir "ortografe" "tmp" in
  Fun.protect
    ~finally:(fun () ->
      sys_command_exn [%string {|rm -rf -- %{Filename.quote d}|}])
    (fun () ->
      let doc_path = Filename.concat d "it.doc" in
      let docx_path = doc_path ^ "x" in
      Out_channel.with_open_bin doc_path
        (fun oc -> Out_channel.output_string oc src);
      sys_command_exn [%string {|cd %{Filename.quote d} && timeout -s SIGKILL 10s bwrap --unshare-all --die-with-parent --new-session --dev-bind / / libreoffice --headless --convert-to docx %{Filename.basename doc_path} >&2|}];
      let src =
        In_channel.with_open_bin docx_path (fun ic ->
            In_channel.input_all ic)
      in
      docx ~buf ?debug ?pp src ~dst)

let epub ?buf ?debug ?pp src ~dst =
  let buf = buffer buf in
  let count = count_size () in
  map_zip src (fun member file contents ->
      (* The xhtml is the bulk of the pages, but in principle, we
         could rewrite more stuff: content.opf, toc.ncx *)
      if String.ends_with (Zipc.Member.path member) ~suffix:".xhtml"
      then (
        count file;
        Some (xhtml ~buf ?debug ?pp (contents ()) ~dst:String)
      ) else None)
  |> write_out dst

let main () =
  Dyn_protect.with_ (fun dp ->
      let src, dst, type_ =
        match Sys.argv with
        | [||] | [|_|] ->
           In_channel.input_all In_channel.stdin, Out_channel.stdout, `Text
        | argv ->
           let name = argv.(1) in
           let new_name =
             if Array.length argv > 2
             then argv.(2)
             else Filename.remove_extension name ^ "-conv" ^ Filename.extension name
           in
           let type_ =
             match Filename.extension name with
             | ".html" -> `Html
             | ".xhtml" -> `Xhtml
             | ".docx" -> `Docx
             | ".doc" -> `Doc
             | ".epub" -> `Epub
             | _ -> `Text (* realistically, this includes markdown *)
           in
           let out_ch = Out_channel.open_bin new_name in
           Dyn_protect.add dp ~finally:(fun () -> Out_channel.close out_ch);
           In_channel.input_all (open_in name), out_ch, type_
      in
      let dst = Channel dst in
      match type_ with
      | `Doc -> doc src ~dst
      | `Docx -> docx src ~dst
      | `Epub -> epub src ~dst
      | `Html -> html src ~dst
      | `Text -> pure_text src ~dst
      | `Xhtml -> xhtml src ~dst
    )

module Private = struct
  let docx_document = docx_document
  let read_whole_zip = read_whole_zip
end
