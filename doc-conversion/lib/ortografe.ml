let docx_ns = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

module More_markup = struct
  open Core

  type name = string * string [@@deriving equal]
  let sexp_of_name (ns, name) =
    let ns =
      if String.(=) ns Markup.Ns.xml
      then "xml"
      else if String.(=) ns docx_ns
      then "docx"
      else ns
    in
    sexp_of_string (ns ^ ":" ^ name)

  type xml_declaration = Markup.xml_declaration =
    {version    : string;
     encoding   : string option;
     standalone : bool option}
      [@@deriving equal, sexp_of]
  type doctype = Markup.doctype =
    {doctype_name      : string option;
     public_identifier : string option;
     system_identifier : string option;
     raw_text          : string option;
     force_quirks      : bool}
      [@@deriving equal, sexp_of]
  type signal =
    [ `Start_element of name * (name * string) list
    | `End_element
    | `Text of string list
    | `Doctype of doctype
    | `Xml of xml_declaration
    | `PI of string * string
    | `Comment of string ]
      [@@deriving equal, sexp_of]

  type 'a node =
    [ `Element of name * (name * string) list * 'a list
    | `Text of string
    | `Doctype of doctype
    | `Xml of xml_declaration
    | `PI of string * string
    | `Comment of string ]
      [@@deriving sexp_of]
  let sexp_of_node sexp_of_a node =
    match node with
    | `Element (name, [], children) ->
       [%sexp_of: [ `Element of (name * a list) ]] (`Element (name, children))
    | _ -> sexp_of_node sexp_of_a node

  type tree = tree node
  [@@deriving sexp_of]

  let trees signals =
    (* should try to upstream this, this is boilerplate that should be provided upstream *)
    Markup.trees signals
      ~text:(fun s -> `Text (String.concat s))
      ~element:(fun a b c -> `Element (a, b, c))
      ~comment:(fun a -> `Comment a)
      ~pi:(fun a b -> `PI (a, b))
      ~xml:(fun a -> `Xml a)
      ~doctype:(fun a -> `Doctype a)
end

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

type options =
  { convert_uppercase : bool
  ; dict : (string, string) Hashtbl.t
  }

type 'a convert = ?buf:Buffer.t -> options:options -> string -> dst:'a out -> 'a
type 'a convert_xml = ?debug:bool -> ?pp:bool -> 'a convert

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

let buffer ?(n = 123) buf =
  match buf with
  | Some buf -> Buffer.clear buf; buf
  | None -> Buffer.create n

let pure_text (type a) ?buf ~options src ~(dst : a out) : a =
  match dst with
  | String ->
     let b = buffer buf ~n:(String.length src) in
     iter_pure_text ~options src ~f:(Buffer.add_string b);
     Buffer.contents b
  | Channel ch ->
     iter_pure_text ~options src ~f:(Out_channel.output_string ch)

let maybe_debug_signal ?(debug = false) signals =
  if debug
  then Markup.map (fun elt ->
           print_endline (Markup.signal_to_string elt);
           elt) signals
  else signals

let maybe_pp_signal ?(pp = false) signals =
  if pp then Markup.pretty_print signals else signals

let text_elt ~options ~buf = function
  | `Text strs -> `Text (List.map (pure_text ~options ~buf ~dst:String) strs)
  | elt -> elt

let html_transform ~buf ~options signal =
  (* maybe we should check the tag ns for the xhtml case *)
  let notranslate_pattern = Core.String.Search_pattern.create "notranslate" in
  let notranscribe_pattern = Core.String.Search_pattern.create "notranscribe" in
  let stack = Core.Stack.create () in
  let hide_current = ref false in
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
      then text_elt ~options ~buf elt
      else elt)
    signal

let html ?debug ?pp ?buf ~options src ~dst =
  (* https://v3.ocaml.org/p/markup/latest/doc/Markup/index.html
     Note: no implicit closing of tags *)
  let buf = buffer buf in
  Markup.parse_html
    (Markup.string src)
  |> Markup.signals
  |> maybe_debug_signal ?debug
  |> html_transform ~buf ~options
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

let xhtml ?buf ?debug ?pp ~options src ~dst =
  let buf = buffer buf in
  xml ?debug ?pp src ~dst ~transform:(html_transform ~buf ~options)

module Docx : sig
  val xml : ?buf:Buffer.t -> ?debug:bool -> ?pp:bool -> options:options -> string -> dst:'a out -> 'a
  val join_consecutive_ish_text_nodes : (Markup.signal, 'a) Markup.stream -> (Markup.signal, 'a) Markup.stream
end = struct

  module Sm = struct
    (* [Sm] is a module that implements a small state machine (or parser combinator, I
       suppose) that recognizes a part of the signal stream. I don't know if it's wise
       to do this in a streaming way, as it's definitely more complicated. On the other
       hand, these xml can be ridiculously verbose, so it does cut down in memory usage,
       which matters for deployment. *)
    open Core

    type t =
      | Tree of { mutable depth : int; elts : More_markup.signal Queue.t }
      | Element of { mutable start_elt : More_markup.signal option
                   ; mutable rev_done : t list
                   ; mutable todo : t list
                   ; mutable done_ : (More_markup.signal * [ `Ok of bool ]) option
                   }
    [@@deriving sexp_of]

    (* [add_and_done] should be called on consecutive elements of the stream until it
       returns [true], indicating it's done parsing (either it has parsed the whole
       structure it was intended to parse, or the stream doesn't have the right shape).

       The first element given to a [Tree] should not be a `[End_element], because this is
       the only possible way for this node to fail, and it has no way to encode failures. *)
    let rec add_and_done t elt =
      match t with
      | Tree r ->
         Queue.enqueue r.elts elt;
         (match elt with
          | `Start_element _ -> r.depth <- r.depth + 1
          | `End_element -> r.depth <- r.depth - 1
          | _ -> ());
         assert (r.depth >= 0);
         r.depth = 0
      | Element r ->
         (match r.start_elt with
          | None ->
             (match elt with
              | `Start_element _ -> r.start_elt <- Some elt; false
              | _ -> r.done_ <- Some (elt, `Ok false); true)
          | Some _ ->
             match r.todo with
             | [] ->
                r.done_ <- Some (elt, `Ok (match elt with `End_element -> true | _ -> false));
                true
             | next :: rest ->
                let problem =
                  match elt, next with
                  | `End_element, Tree { depth = 0; _ } -> true
                  | _, _ -> false
                in
                if problem
                then (r.done_ <- Some (elt, `Ok false); true)
                else
                  (if add_and_done next elt
                   then (r.rev_done <- next :: r.rev_done; r.todo <- rest);
                   false))

    let rec fold t acc =
      match t with
      | Tree r ->
         Queue.fold r.elts ~init:acc ~f:(fun acc elt -> elt :: acc)
      | Element r ->
         let acc =
           match r.start_elt with
           | None -> acc
           | Some elt -> elt :: acc
         in
         let acc =
           List.fold (List.rev r.rev_done) ~init:acc
             ~f:(fun acc elt -> fold elt acc) in
         match r.done_ with
         | None -> acc
         | Some (elt, _good) -> elt :: acc
  end

  include struct
      open Core
      let create_sm elt =
        Sm.Element
          { start_elt = Some elt
          ; rev_done = []
          ; todo = [ Tree { depth = 0; elts = Queue.create () }
                   ; Tree { depth = 0; elts = Queue.create () }
                   ]
          ; done_ = None
          }

      type previously_parsed =
        { start1 : More_markup.signal
        ; block : More_markup.signal Queue.t
        ; start_text : More_markup.signal
        ; text : Buffer.t (* buffer to guarantee linear time construction *)
        ; end_text : More_markup.signal
        ; end2 : More_markup.signal
        }
      [@@deriving sexp_of]

      let previously_parsed sm =
        (* finising parsing the Sm.t into the specific shape we wanted to recognize *)
        match sm with
        | Sm.Element r ->
           (match r.start_elt, r.done_ with
            | Some start1, Some (end2, `Ok true) ->
               (match r.rev_done with
                | [ Tree t2; Tree t1 ] ->
                   if Queue.length t2.elts = 3
                   then
                     match Queue.to_list t2.elts with
                     | [ `Start_element (name, _) as start_text; `Text text; `End_element as end_text ]
                          when [%compare.equal: (string * string)] name (docx_ns, "t")
                       ->
                        let b = Buffer.create (List.sum (module Int) text ~f:String.length) in
                        List.iter text ~f:(Buffer.add_string b);
                        Some { start1; block = t1.elts; start_text; text = b; end_text; end2 }
                     | _ -> None
                   else None
                | _ -> None)
            | _ -> None)
        | _ -> None

      let attr_is_xml_space name =
        [%compare.equal: (string * string)] name (Markup.Ns.xml, "space")

      let fold_previously_parsed { start1; block; start_text; text; end_text; end2 } acc =
        let text = Buffer.contents text in
        let acc = Queue.fold block ~init:(start1 :: acc) ~f:(fun acc elt -> elt :: acc) in
        let start_text =
          match start_text with
          | `Start_element (name, attrs) ->
             let attrs = List.filter attrs ~f:(fun (name, _) -> not (attr_is_xml_space name)) in
             let attrs =
               if String.(=) text (String.strip text)
               then attrs
               else ((Markup.Ns.xml, "space"), "preserve") :: attrs
             in
             `Start_element (name, attrs)
          | _ -> assert false
        in
        end2 :: end_text :: `Text [text] :: start_text :: acc

      let fold_previously_parsed_opt previously_parsed_opt acc =
        match previously_parsed_opt with
        | None -> acc
        | Some p -> fold_previously_parsed p acc

      type state =
        | Outside of previously_parsed option (* Some if we just finished parsing a <r> *)
        | In_r of previously_parsed option * Sm.t
      [@@deriving sexp_of]
  end

  let join_consecutive_ish_text_nodes signal =
    (* libreoffice is happy to generate spammy boundaries, as shown below when editing
       "un deux trois quatre" by replacing the x with an s. So take out this spam
       so we have long runs that don't cut words in half, thus allowing the next
       phase to see full words instead of word fragments.

      <w:r>
        <w:rPr/>
        <w:t>Un deu</w:t>
      </w:r>
      <w:r>
        <w:rPr/>
        <w:t>s</w:t>
      </w:r>
      <w:r>
        <w:rPr/>
        <w:t xml:space="preserve"> trois quatre.</w:t>
      </w:r>

    This function turns that into:

      <w:r>
        <w:rPr/>
        <w:t>Un deus trois quatre</w:t>
      </w:r>

    MS Word seems to create the same sort of things except it adds properties like
    w:rsidR="00F25C40" on nodes, for what seems to be history of the changes in the doc
    (although concretely, I can't see how to access said history).

    Regardless, we trample over all this stuff. The new document should be considered
    a big C-c/C-v of the previous doc, not a set of minimal edits.

    In principle, the maximally smart thing to do would be to treat the docx-specific
    code as providing a stream of [string option] (stream of "word or boundary", the
    boundaries indicating document structure like the end of a bullet point in a list)
    and the linguistic part of the code would turn every word into exactly one string,
    (which could be the empty string, one word or multiple words). This would then allow
    the docx code to reinsert the structure of the document around the new text in the
    way that alters the structure from the input document the least. But that seems like
    quite a bit of work, and it's not clear that we should care.

    http://officeopenxml.com/WPtext.php might have useful information, because the spec
    itself is huge, and probably unreadable.
     *)
    let open Core in
    let flush previous elt =
      List.rev (elt :: fold_previously_parsed_opt previous [])
    in
    let try_joining_sm (previous : previously_parsed option) current =
      match previously_parsed current with
      | None -> List.rev (Sm.fold current (fold_previously_parsed_opt previous [])), Some (Outside None)
      | Some current ->
        match previous with
        | None -> [], Some (Outside (Some current))
        | Some previous ->
           if Queue.equal More_markup.equal_signal previous.block current.block
           then (Buffer.add_buffer previous.text current.text; [], Some (Outside (Some previous)))
           else List.rev (fold_previously_parsed previous []), Some (Outside (Some current))
    in
    Markup.transform (fun state elt ->
        match state with
        | Outside previous ->
           (match elt with
            | `Start_element (ns_tag, _) ->
               if [%compare.equal: (string * string)] ns_tag (docx_ns, "r")
               then [], Some (In_r (previous, create_sm elt))
               else flush previous elt, Some (Outside None)
            | _ -> flush previous elt, Some (Outside None))
        | In_r (previous, sm) ->
           if Sm.add_and_done sm elt
           then try_joining_sm previous sm
           else [], Some state)
      (Outside None)
      signal

  (* il faut plus de tests aussi *)

  let drop_squigglies signals =
    (* docs contains things like:

      <w:proofErr w:type="spellStart"/>
      <w:r>
        <w:rPr>
          <w:rFonts w:ascii="Arial" w:hAnsi="Arial"/>
          <w:sz w:val="22"/>
        </w:rPr>
        <w:t>bla bla bla</w:t>
      </w:r>
      <w:proofErr w:type="spellEnd"/>

      Just discard it all, so the next transformations can be more effective.
     *)
    let open Core in
    let stack = Stack.of_list [true] in
    Markup.filter (function
        | `Start_element (name, _) ->
           Stack.push stack
             (not ([%compare.equal: (string * string)] name (docx_ns, "proofErr")));
           Stack.top_exn stack
        | `End_element ->
           let b = Stack.top_exn stack in
           ignore (Stack.pop_exn stack);
           b
        | _ -> Stack.top_exn stack
      ) signals

  let docx_convert ~buf ~options signal =
    let open Core in
    let stack = Stack.create () in
    Markup.map (fun elt ->
        (match elt with
         | `Start_element (ns_tag, _) ->
            Stack.push stack ns_tag;
         | `End_element ->
            ignore (Stack.pop stack)
         | `Text _ | `Doctype _ | `Xml _ | `PI _ | `Comment _ -> ());
        if [%compare.equal: (string * string) option] (Stack.top stack) (Some (docx_ns, "t"))
        then text_elt ~buf ~options elt
        else elt
      ) signal

  let xml ?buf ?debug ?pp ~options src ~dst =
    let buf = buffer buf in
    xml ?debug ?pp src ~dst ~transform:(fun signals ->
        signals
        |> drop_squigglies
        |> join_consecutive_ish_text_nodes
        |> docx_convert ~buf ~options)

  let join_consecutive_ish_text_nodes signals =
    (* since this is for testing, including squigglies *)
    signals
    |> drop_squigglies
    |> join_consecutive_ish_text_nodes
end

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
    (* Zipc enforces that the content is no larger than the size in the metadata, so this
       check is robust whether large data is submitted accidentally or is adversarial.
       Zip bombs require no special attention: they are just a way to get a higher
       compression ratio than deflate supports (1000x). *)
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

let docx ?debug ?pp ?buf ~options src ~dst =
  let buf = buffer buf in
  let count = count_size () in
  map_zip src (fun member file contents ->
      match Zipc.Member.path member with
      | "word/document.xml"
      | "word/footnotes.xml"
      | "word/endnotes.xml" ->
         count file;
         Some (Docx.xml ~buf ?debug ?pp ~options (contents ()) ~dst:String)
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

let doc ?debug ?pp ?buf ~options src ~dst =
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
      docx ~buf ?debug ?pp ~options src ~dst)

let epub ?debug ?pp ?buf ~options src ~dst =
  let buf = buffer buf in
  let count = count_size () in
  map_zip src (fun member file contents ->
      (* The xhtml is the bulk of the pages, but in principle, we
         could rewrite more stuff: content.opf, toc.ncx *)
      match Filename.extension (Zipc.Member.path member) with
      | ".xhtml" | ".html" -> (* in principle we'd need to read the root file to know how
                                 to interpret the various files. *)
         count file;
         Some (xhtml ~buf ?debug ?pp ~options (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let htmlz ?debug ?pp ?buf ~options src ~dst =
  let buf = buffer buf in
  let count = count_size () in
  map_zip src (fun member file contents ->
      match Filename.extension (Zipc.Member.path member) with
      | ".html" ->
         count file;
         Some (html ~buf ?debug ?pp ~options (contents ()) ~dst:String)
      | _ -> None)
  |> write_out dst

let of_ext ext =
  match String.lowercase_ascii ext with
  | ".html" -> Some (ext, `Html)
  | ".xhtml" -> Some (ext, `Xhtml)
  | ".htmlz" -> Some (ext, `Htmlz) (* export format on wikisource for instance *)
  | ".docx" -> Some (ext, `Docx)
  | ".doc" -> Some (".docx", `Doc)
  | ".epub" -> Some (ext, `Epub)
  | _ -> None

let convert typ ~options src ~dst =
  match typ with
  | `Html -> html ~options src ~dst
  | `Xhtml -> xhtml ~options src ~dst
  | `Htmlz -> htmlz ~options src ~dst
  | `Docx -> docx ~options src ~dst
  | `Doc -> doc ~options src ~dst
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

module Private = struct
  let docx_document = docx_document
  let read_whole_zip = read_whole_zip
  let join_consecutive_ish_text_nodes = Docx.join_consecutive_ish_text_nodes
end
