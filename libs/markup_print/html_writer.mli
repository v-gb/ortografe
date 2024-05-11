(* This file is a modified copy of html_writer.mli.source sibling file,
   to remove all the cps stuff that prevents the code from working in javascript *)

val write :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  ((Markup.signal -> unit) -> unit) -> (string -> unit) -> unit
