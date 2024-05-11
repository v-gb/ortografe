(* This file is a modified copy of xml_writer.mli.source sibling file,
   to remove all the cps stuff that prevents the code from working in javascript *)

val write :
  ((Markup.signal -> unit) -> unit) -> (string -> unit) -> unit
