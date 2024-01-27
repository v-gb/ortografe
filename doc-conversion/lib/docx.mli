val convert : _ More_markup.convert_xml
val convert_doc : _ More_markup.convert_xml

module Private : sig
  val join_consecutive_ish_text_nodes : (Markup.signal, 'a) Markup.stream -> (Markup.signal, 'a) Markup.stream
end
