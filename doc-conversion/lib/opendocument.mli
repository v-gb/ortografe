(** OpenDocument is the format for .odt, .odp used by LibreOffice and OpenOffice. Not to
    be confused with OpenOfficeXml, used by the Microsoft equivalents. *)

val convert : ?convert_text:(string -> string) -> _ Common.convert
