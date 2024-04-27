open Common

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

module More_markup = More_markup
type 'a out = 'a Common.out =
  | Channel : Out_channel.t -> unit out
  | String : string out
  | Ignore : unit out

type options = Common.options =
  { convert_uppercase : bool
  ; dict : string -> string option
  ; interleaved : bool
  ; plurals_in_s : bool
  }

type 'a convert = 'a Common.convert

let load_dict str =
  let l = String.split_on_char '\n' str in
  let h = Hashtbl.create 25000 in
  List.iter (fun str ->
      match String.split_on_char ',' str with
      | [] | [""] -> ()
      | [ a; b ] ->
         Hashtbl.replace h a b
      | _ -> failwith ("wtf " ^ str)) l;
  h

let erofa = lazy (load_dict Dict.extension_dict_gen_csv)
let rect1990 = lazy (load_dict Dict.extension_dict1990_gen_csv)
let pure_text = Text.convert
let html = Html.convert
let xhtml = Html.convert_xhtml
let officeopenxml = Officeopenxml.convert
let officeopenxml_old = Officeopenxml.convert_old
let opendocument = Opendocument.convert

let epub ?convert_text ?buf ~options src ~dst =
  let buf = buffer buf in
  Zip.map src (fun member contents ->
      (* The xhtml is the bulk of the pages, but in principle, we
         could rewrite more stuff: content.opf, toc.ncx *)
      match Filename.extension (Zipc.Member.path member) with
      | ".xhtml" | ".html" -> (* in principle we'd need to read the root file to know how
                                 to interpret the various files. *)
         Some (xhtml ?convert_text ~buf ~options (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let htmlz ?buf ~options src ~dst =
  let buf = buffer buf in
  Zip.map src (fun member contents ->
      match Filename.extension (Zipc.Member.path member) with
      | ".html" ->
         Some (html ~buf ~options (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let of_ext ext =
  match String.lowercase_ascii ext with
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

let convert ?convert_text typ ~options src ~dst =
  match typ with
  | `Html -> html ?convert_text ~options src ~dst
  | `Xhtml -> xhtml ?convert_text ~options src ~dst
  | `Htmlz -> htmlz ~options src ~dst
  | `Docx -> officeopenxml `Docx ~options src ~dst
  | `Pptx -> officeopenxml `Pptx ~options src ~dst
  | `Doc -> officeopenxml_old `Doc ~options src ~dst
  | `Ppt -> officeopenxml_old `Ppt ~options src ~dst
  | `Opendocument -> opendocument ~options src ~dst
  | `Epub -> epub ?convert_text ~options src ~dst
  | `Text -> pure_text ~options src ~dst

let convert_string ~ext ~options src =
  match
    match String.lowercase_ascii ext with
    | ".txt" | ".md" | ".mkd" -> Some (ext, `Text)
    | _ -> of_ext ext
  with
  | None -> None
  | Some (ext, typ) -> Some (ext, convert typ ~options src ~dst:String)

let open_channel dp name =
  let out_ch = Out_channel.open_bin name in
  Dyn_protect.add dp ~finally:(fun () -> Out_channel.close out_ch);
  out_ch

let read_all name =
  In_channel.with_open_bin name
    In_channel.input_all

let src_dst_typ src dst ~dyn_protect:dp =
  match src with
  | None ->
     In_channel.input_all In_channel.stdin,
     (match dst with
      | None -> Out_channel.stdout
      | Some new_name -> open_channel dp new_name),
     `Text
  | Some name ->
     let new_ext, typ =
       let ext = (Filename.extension name) in
       Option.value (of_ext (Filename.extension name)) ~default:(ext, `Text)
     in
     let new_name =
       match dst with
       | Some new_name -> new_name
       | None -> Filename.remove_extension name ^ "-conv" ^ new_ext
     in
     (* first read, then open for writing, in case input = output *)
     let str_in = read_all name in
     str_in, open_channel dp new_name, typ

let convert_files ~options src dst =
  Dyn_protect.with_ (fun dyn_protect ->
      let src, dst, typ = src_dst_typ src dst ~dyn_protect in
      convert typ ~options src ~dst:(Channel dst)
    )

let string_of_sexp_always_quote_avoid_escapes =
  let rec print_sexp buf = function
    | Sexplib.Sexp.Atom s ->
       (* Do the least amount of escaping possible (well except for newlines),
          so we can trivially rewrite strings with sed from the outside without
          having to parse strings. *)
       Buffer.add_string buf "\"";
       if String.exists (function '"' | '\\' | '\n' -> true | _ -> false) s
       then String.iter (function
                | '"' -> Buffer.add_string buf "\\\""
                | '\\' -> Buffer.add_string buf "\\\\"
                | '\n' -> Buffer.add_string buf "\\n"
                | c -> Buffer.add_char buf c) s
       else Buffer.add_string buf s;
       Buffer.add_string buf "\""
    | List l ->
       Buffer.add_string buf "(";
       List.iter (print_sexp buf) l;
       Buffer.add_string buf ")";
  in
  fun sexp ->
  let b = Buffer.create 100 in
  print_sexp b sexp;
  Buffer.contents b

let ext_conv src dst inex =
  Dyn_protect.with_ (fun dyn_protect ->
      let src, dst, typ = src_dst_typ src dst ~dyn_protect in
      let convert_text =
        match inex with
        | `Extract ->
           (fun s ->
             output_string dst
               (string_of_sexp_always_quote_avoid_escapes (Atom s) ^ "\n");
             "")
        | `Insert f ->
           let strs =
             (match f with
             | None -> In_channel.input_all In_channel.stdin
             | Some f -> read_all f)
             |> Sexplib.Sexp.of_string_many
             |> List.to_seq
             |> Queue.of_seq
           in
           (fun _s -> Base.string_of_sexp (Queue.pop strs))
      in
      let options =
        { convert_uppercase = true
        ; interleaved = true
        ; plurals_in_s = true
        ; dict = (fun x -> Some x)
        }
      in
      match inex with
      | `Extract -> convert ~convert_text typ src ~dst:Ignore ~options
      | `Insert _ -> convert ~convert_text typ src ~dst:(Channel dst) ~options
  )

let max_size = Zip.max_size
let map_zip = Zip.map

let extension_dict1990_gen_csv = Dict.extension_dict1990_gen_csv
module Private = struct
  let grab_from_zip src name =
    let zipc = Zipc.of_binary_string src |> Core.Result.ok_or_failwith in
    match Zipc.find name zipc with
    | None -> "<absent>"
    | Some member ->
       match Zipc.Member.kind member with
       | Dir -> "<directory>"
       | File file -> Zipc.File.to_binary_string file |> Core.Result.ok_or_failwith
  let convert_officeopenxml = Officeopenxml.convert_xml
end
