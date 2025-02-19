open! Core
open Common

open struct
  module Markup = Markup_t
end

module Dyn_protect = struct
  type t = (unit -> unit) Stack.t

  let add (t : t) ~finally = Stack.push t finally

  exception Exns of exn list

  let with_ f =
    let t = Stack.create () in
    Fun.protect
      ~finally:(fun () ->
        let e = ref [] in
        while
          match Stack.pop t with
          | None -> false
          | Some f ->
              (try f () with exn -> e := exn :: !e);
              true
        do
          ()
        done;
        match List.rev !e with
        | [] -> ()
        | [ exn ] -> raise exn
        | exns -> raise (Exns exns))
      (fun () -> f t)
end

module More_markup = More_markup
module Common = Common

type 'a out = 'a Common.out =
  | Channel : Out_channel.t -> unit out
  | String : string out
  | Substring : int -> Common.Substring.t out
  | Ignore : unit out

type options = Common.options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : string option
  }

type 'a convert = 'a Common.convert

let pure_text ?convert_text ?buf ?progress ~options src ~dst =
  match convert_text with
  | Some f -> write_out dst (f src)
  | None -> Text.convert ?progress ?buf ~options src ~dst

let html = Html.convert
let xhtml = Html.convert_xhtml
let officeopenxml = Officeopenxml.convert
let officeopenxml_old = Officeopenxml.convert_old
let opendocument = Opendocument.convert

let epub ?convert_text ?progress ~options src ~dst =
  Zip.map' ?progress src (fun ~path ->
      (* The xhtml is the bulk of the pages, but in principle, we
         could rewrite more stuff: content.opf, toc.ncx *)
      match Stdlib.Filename.extension path with
      | ".xhtml" | ".html" ->
          (* in principle we'd need to read the root file to know how
             to interpret the various files. *)
          Some
            (fun ~contents ->
              xhtml ?convert_text ~options contents ~dst:(substring_out contents))
      | _ -> None)
  |> write_out dst

let htmlz ?convert_text ?progress ~options src ~dst =
  Zip.map' ?progress src (fun ~path ->
      match Stdlib.Filename.extension path with
      | ".html" ->
          Some
            (fun ~contents ->
              html ?convert_text ~options contents ~dst:(substring_out contents))
      | _ -> None)
  |> write_out dst

let known_exts =
  [ (".txt", "text/plain")
  ; (".md", "text/x-markdown")
  ; (".mkd", "text/x-markdown")
  ; (".html", "text/html")
  ; (".xhtml", "application/xhtml+xml")
  ; (".htmlz", "application/octet-stream")
  ; (".docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
  ; (".ppt", "application/vnd.ms-powerpoint")
  ; ( ".pptx"
    , "application/vnd.openxmlformats-officedocument.presentationml.presentation" )
  ; (".doc", "application/msword")
  ; (".odt", "application/vnd.oasis.opendocument.text")
  ; (".odp", "application/vnd.oasis.opendocument.presentation")
  ; (".epub", "application/epub+zip")
  ]

let mimetype_by_ext =
  let tbl = lazy (Stdlib.Hashtbl.of_seq (Stdlib.List.to_seq known_exts)) in
  fun str -> Stdlib.Hashtbl.find_opt (Lazy.force tbl) str

let of_ext ext =
  match String.lowercase ext with
  | ".txt" | ".md" | ".mkd" -> Some (ext, `Text)
  | ".html" -> Some (ext, `Html)
  | ".xhtml" -> Some (ext, `Xhtml)
  | ".htmlz" -> Some (ext, `Htmlz) (* export format on wikisource for instance *)
  | ".docx" -> Some (ext, `Docx)
  | ".ppt" -> Some (".pptx", `Ppt)
  | ".pptx" -> Some (ext, `Pptx)
  | ".doc" -> Some (".docx", `Doc)
  | ".odt" | ".odp" -> Some (ext, `Opendocument)
  | ".epub" -> Some (ext, `Epub)
  | _ -> None

let convert ?convert_text typ ?progress ~options src ~dst =
  match typ with
  | `Html -> html ?convert_text ?progress ~options src ~dst
  | `Xhtml -> xhtml ?convert_text ?progress ~options src ~dst
  | `Htmlz -> htmlz ?convert_text ?progress ~options src ~dst
  | `Docx -> officeopenxml `Docx ?convert_text ?progress ~options src ~dst
  | `Pptx -> officeopenxml `Pptx ?convert_text ?progress ~options src ~dst
  | `Doc -> officeopenxml_old `Doc ?convert_text ?progress ~options src ~dst
  | `Ppt -> officeopenxml_old `Ppt ?convert_text ?progress ~options src ~dst
  | `Opendocument -> opendocument ?convert_text ?progress ~options src ~dst
  | `Epub -> epub ?convert_text ?progress ~options src ~dst
  | `Text -> pure_text ?convert_text ?progress ~options src ~dst

let convert_string ~ext ?progress ~options src =
  match of_ext ext with
  | None -> None
  | Some (ext, typ) -> Some (`ext ext, convert typ ?progress ~options src ~dst:String)

let open_channel dp name =
  let out_ch = Out_channel.create name in
  Dyn_protect.add dp ~finally:(fun () -> Out_channel.close out_ch);
  out_ch

let src_dst_typ ?src_type src dst ~dyn_protect:dp =
  let src_type =
    Option.map
      ~f:(fun src_type ->
        of_ext src_type ||? failwith [%string "unknown file type %{src_type}"])
      src_type
  in
  match src with
  | None ->
      ( In_channel.input_all In_channel.stdin
      , (match dst with
        | None -> Out_channel.stdout
        | Some new_name -> open_channel dp new_name)
      , Option.map ~f:snd src_type ||? `Text )
  | Some src_name ->
      let dst_ext, typ =
        src_type
        ||? (of_ext (Stdlib.Filename.extension src_name)
            ||?
            let exts =
              "{" ^ String.concat ~sep:"|" (List.map known_exts ~f:fst) ^ "}"
            in
            failwith
              [%string
                "%{src_name} isn't a supported file format. You might want to pass \
                 --type %{exts} to treat the file as a supported file format"])
      in
      let dst_name =
        match dst with
        | Some new_name -> new_name
        | None -> Stdlib.Filename.remove_extension src_name ^ "-conv" ^ dst_ext
      in
      (* first read, then open for writing, in case input = output *)
      let src_str = In_channel.read_all src_name in
      (src_str, open_channel dp dst_name, typ)

let convert_files ?src_type ~options src dst =
  Dyn_protect.with_ (fun dyn_protect ->
      let src, dst, typ = src_dst_typ ?src_type src dst ~dyn_protect in
      convert typ ~options src ~dst:(Channel dst))

let string_of_sexp_always_quote_avoid_escapes =
  let rec print_sexp buf = function
    | Sexplib.Sexp.Atom s ->
        (* Do the least amount of escaping possible (well except for newlines),
           so we can trivially rewrite strings with sed from the outside without
           having to parse strings. *)
        Buffer.add_string buf "\"";
        if String.exists ~f:(function '"' | '\\' | '\n' -> true | _ -> false) s
        then
          String.iter
            ~f:(function
              | '"' -> Buffer.add_string buf "\\\""
              | '\\' -> Buffer.add_string buf "\\\\"
              | '\n' -> Buffer.add_string buf "\\n"
              | c -> Buffer.add_char buf c)
            s
        else Buffer.add_string buf s;
        Buffer.add_string buf "\""
    | List l ->
        Buffer.add_string buf "(";
        List.iter l ~f:(print_sexp buf);

        Buffer.add_string buf ")"
  in
  fun sexp ->
    let b = Buffer.create 100 in
    print_sexp b sexp;
    Buffer.contents b

let ext_conv ?src_type src dst inex =
  Dyn_protect.with_ (fun dyn_protect ->
      let src, dst, typ = src_dst_typ ?src_type src dst ~dyn_protect in
      let convert_text =
        match inex with
        | `Extract ->
            fun s ->
              Out_channel.output_string dst
                (string_of_sexp_always_quote_avoid_escapes (Atom s) ^ "\n");
              ""
        | `Insert f ->
            let strs =
              (match f with
              | None -> In_channel.input_all In_channel.stdin
              | Some f -> In_channel.read_all f)
              |> Sexplib.Sexp.of_string_many
              |> Stdlib.List.to_seq
              |> Stdlib.Queue.of_seq
            in
            fun _s -> Base.string_of_sexp (Stdlib.Queue.pop strs)
      in
      let options =
        { convert_uppercase = true
        ; interleaved = true
        ; plurals_in_s = Some "s"
        ; dict = (fun x -> Some x)
        }
      in
      match inex with
      | `Extract -> convert ~convert_text typ src ~dst:Ignore ~options
      | `Insert _ -> convert ~convert_text typ src ~dst:(Channel dst) ~options)

let max_size = Zip.max_size
let map_zip = Zip.map

module Private = struct
  let grab_from_zip src name =
    let zipc = Zipc.of_binary_string src |> Result.ok_or_failwith in
    match Zipc.find name zipc with
    | None -> "<absent>"
    | Some member -> (
        match Zipc.Member.kind member with
        | Dir -> "<directory>"
        | File file -> Zipc.File.to_binary_string file |> Result.ok_or_failwith)

  let convert_officeopenxml = Officeopenxml.convert_xml
end
