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

type options = Common.options =
  { convert_uppercase : bool
  ; dict : (string, string) Hashtbl.t
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

let erofa = lazy (load_dict Dict.erofa)
let rect1990 = lazy (load_dict Dict.rect1990)
let pure_text = Text.convert
let html = Html.convert
let xhtml = Html.convert_xhtml
let docx = Docx.convert
let doc = Docx.convert_doc
let odt = Odt.convert

let epub ?buf ~options src ~dst =
  let buf = buffer buf in
  Zip.map src (fun member contents ->
      (* The xhtml is the bulk of the pages, but in principle, we
         could rewrite more stuff: content.opf, toc.ncx *)
      match Filename.extension (Zipc.Member.path member) with
      | ".xhtml" | ".html" -> (* in principle we'd need to read the root file to know how
                                 to interpret the various files. *)
         Some (xhtml ~buf ~options (contents ()) ~dst:String)
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
  | ".doc" -> Some (".docx", `Doc)
  | ".odt" -> Some (ext, `Odt)
  | ".epub" -> Some (ext, `Epub)
  | _ -> None

let convert typ ~options src ~dst =
  match typ with
  | `Html -> html ~options src ~dst
  | `Xhtml -> xhtml ~options src ~dst
  | `Htmlz -> htmlz ~options src ~dst
  | `Docx -> docx ~options src ~dst
  | `Doc -> doc ~options src ~dst
  | `Odt -> odt ~options src ~dst
  | `Epub -> epub ~options src ~dst
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

let convert_files ~options src dst =
  Dyn_protect.with_ (fun dp ->
      let src, dst, typ =
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
           In_channel.input_all (open_in name), open_channel dp new_name, typ
      in
      convert typ ~options src ~dst:(Channel dst)
    )

let max_size = Zip.max_size
let map_zip = Zip.map

module Private = struct
  let grab_from_zip src name =
    let zipc = Zipc.of_binary_string src |> Core.Result.ok_or_failwith in
    match Zipc.find name zipc with
    | None -> "<absent>"
    | Some member ->
       match Zipc.Member.kind member with
       | Dir -> "<directory>"
       | File file -> Zipc.File.to_binary_string file |> Core.Result.ok_or_failwith
  let join_consecutive_ish_text_nodes = Docx.Private.join_consecutive_ish_text_nodes
end
