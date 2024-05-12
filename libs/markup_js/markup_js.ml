let parse_into_dom src ~flavor : Jv.t =
  (* Parse errors apparently don't cause exceptions, but result in a dom that
     embeds the error. Probably something we should turn into an exception.
     https://stackoverflow.com/questions/11563554/how-do-i-detect-xml-parsing-errors-when-using-javascripts-domparser-in-a-cross *)
  Jv.new' (Jv.get Jv.global "DOMParser") [| |]
  |> Jv.call' __ (Jstr.v "parseFromString")
       [| Jv.of_jstr src
        ; Jv.of_jstr (match flavor with
                      | `Xml -> Jstr.v "text/xml"
                      | `Html -> Jstr.v "text/html")
       |]

external array_get : Jv.t -> int -> Jv.t = "caml_js_get"
let attributes (el : Brr.El.t) =
  let el = Brr.El.to_jv el in
  let attrs = Jv.get el "attributes" in
  List.init
    (Jv.get attrs "length" |> Jv.to_int)
    (fun i ->
      let attr = array_get attrs i in
      (Jv.to_option Jv.to_string (Jv.get attr "namespaceURI") ||? Markup.Ns.html,
       Jv.to_string (Jv.get attr "localName")),
      Jv.to_string (Jv.get attr "value"))

let iter_child_nodes (el : Brr.El.t) f =
  let child_nodes = Jv.get (Brr.El.to_jv el) "childNodes" in
  for i = 0 to (Jv.get child_nodes "length" |> Jv.to_int) - 1; do
    let child = array_get child_nodes i in
    f (Brr.El.of_jv child)
  done

let rec signal_from_dom (t : Brr.El.t) f =
  (* Maybe we could emit comments as well. *)
  if Brr.El.is_el t
  then (
    let ns_tag =
      (Jv.to_string (Jv.get (Brr.El.to_jv t) "namespaceURI"),
       Jv.to_string (Jv.get (Brr.El.to_jv t) "localName"))
    in
    f (`Start_element (ns_tag, attributes t));
    iter_child_nodes t (signal_from_dom __ f);
    f `End_element;
  ) else if Brr.El.is_txt t
  then f (`Text [ Jstr.to_string (Brr.El.txt_text t) ])
  else ()

let parse ~flavor src (dst : Markup.signal -> unit) =
  let document = parse_into_dom (Jstr.of_string src) ~flavor in
  (match flavor with
   | `Xml ->
      (* Despite
         https://developer.mozilla.org/en-US/docs/Web/API/ProcessingInstruction, there
         seems to be no interface to retrieve the processing instruction that I can
         see. In practice, the value below seems to be the only flavor of boilerplate I
         observe. Maybe I look for the first "<[^?]" and copy everything identically
         from the source. *)
      dst (`Xml {version = "1.0"; encoding = Some "UTF-8"; standalone = Some true})
   | `Html -> ());
  Jv.get document "doctype"
  |> Jv.to_option Fun.id
  |> Option.iter (fun doctype ->
         (* We could probably be more precise here, but should be good enough. *)
         let doctype_name = Jv.to_string (Jv.get doctype "name") in
         dst (`Doctype { raw_text = Some doctype_name
                       ; doctype_name = Some doctype_name
                       ; public_identifier = None
                       ; system_identifier = None
                       ; force_quirks = false }));
  signal_from_dom (Brr.El.of_jv (Jv.get document "documentElement")) dst
