val throw : Jv.Error.t -> _
val is_array : Jv.t -> bool
val json_of_string : string -> ([> `Array of 'a list
                                | `Assoc of (string * 'a) list
                                | `Boolean of bool
                                | `Null
                                | `Number of float
                                | `String of string ]
                                as 'a)

val read_bytes : Brr.File.t -> string Fut.or_error
