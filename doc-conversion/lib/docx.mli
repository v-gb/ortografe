val convert : _ Common.convert
val convert_doc : _ Common.convert

module Private : sig
  val join_consecutive_ish_text_nodes : (Markup.signal, 'a) Markup.stream -> (Markup.signal, 'a) Markup.stream
end
