val throw : Jv.Error.t -> _
val or_throw : ('a, Jv.Error.t) result -> 'a
val is_array : Jv.t -> bool

(** These three functions are better versions of the same names in Fut. The problem with
   Fut is that it fails to forward rejected promises, which breaks the propagation
   of exceptions, and prevents exception handling like error reporting and cleanup. *)
val fut_to_promise : ok:('a -> Jv.t) -> 'a Fut.or_error -> Jv.Promise.t
val fut_to_promise' : ok:('a -> Jv.t) -> error:('b -> Jv.t) -> ('a, 'b) Fut.result -> Jv.Promise.t
val fut_await : 'a Fut.t -> (('a, Jv.Error.t) result -> unit) -> unit

val json_of_string : string -> ([> `Array of 'a list
                                | `Assoc of (string * 'a) list
                                | `Bool of bool
                                | `Null
                                | `Number of float
                                | `String of string ]
                                as 'a)
val json_to_string : ([< `Array of 'a list
                      | `Assoc of (string * 'a) list
                      | `Bool of bool
                      | `Null
                      | `Number of float
                      | `String of string ]
                      as 'a) -> string

val read_bytes : Brr.File.t -> string Fut.or_error

(** This is mimicking the effect of clicking on an anchor tag, but with data that's
    provided as code in the client, instead of as a url on the server. *)
val download_from_memory :
  mime:string
  -> filename:Jstr.t
  -> [< `Jstr of Jstr.t | `Str_in_base64 of string ]
  -> unit

(** The data doesn't return utf8 necessarily, it just returns whatever is received
    on the wire. *)
val fetch : Jstr.t -> string Fut.or_error

val get_element_by_id : Jstr.t -> Jv.t

type ww_cache
val ww_cache : unit -> ww_cache
type rpc
val rpc : ('q -> 'r Fut.or_error)
          -> rpc * (?ww_cache:ww_cache -> ?local:bool -> 'q -> 'r Fut.or_error)
val rpc_with_progress
    : (?progress:(int -> unit) -> 'q -> 'r Fut.or_error)
  -> rpc
     * (?ww_cache:ww_cache -> ?local:bool -> ?progress:(int -> unit) -> 'q -> 'r Fut.or_error)
val main : rpc list -> (unit -> unit) -> unit


(** Bindings  *)
module B : sig
  val unit : Jv.t -> unit
  val jstr : Jv.t -> Jstr.t
  val string : Jv.t -> string
  val bool : Jv.t -> bool
  val int : Jv.t -> int
  val magic : Jv.t -> _
  val option : (Jv.t -> 'a) -> Jv.t -> 'a option
  val fun1 : ('a1 -> Jv.t) -> (Jv.t -> 'r) -> Jv.t -> ('a1 -> 'r)

  val unit' : unit -> Jv.t
  val jstr' : Jstr.t -> Jv.t
  val string' : string -> Jv.t
  val bool' : bool -> Jv.t
  val int' : int -> Jv.t
  val magic' : _ -> Jv.t
  val option' : ('a -> Jv.t) -> 'a option -> Jv.t
  val promise_or_error' : ('a -> Jv.t) -> 'a Fut.or_error -> Jv.t
  val t2' : ('a1 -> Jv.t) -> ('a2 -> Jv.t) -> 'a1 * 'a2 -> Jv.t
  val t3' : ('a1 -> Jv.t) -> ('a2 -> Jv.t) -> ('a3 -> Jv.t) -> 'a1 * 'a2 * 'a3 -> Jv.t
  val fun1'
      : (Jv.t -> 'a1)
        -> ('r -> Jv.t)
        -> ('a1 -> 'r)
        -> Jv.t
  val fun2'
      : (Jv.t -> 'a1)
        -> (Jv.t -> 'a2)
        -> ('r -> Jv.t)
        -> ('a1 -> 'a2 -> 'r)
        -> Jv.t
  val fun3'
      : (Jv.t -> 'a1)
        -> (Jv.t -> 'a2)
        -> (Jv.t -> 'a3)
        -> ('r -> Jv.t)
        -> ('a1 -> 'a2 -> 'a3 -> 'r)
        -> Jv.t
  val fun5'
      : (Jv.t -> 'a1)
        -> (Jv.t -> 'a2)
        -> (Jv.t -> 'a3)
        -> (Jv.t -> 'a4)
        -> (Jv.t -> 'a5)
        -> ('r -> Jv.t)
        -> ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'r)
        -> Jv.t
  val fun6'
      : (Jv.t -> 'a1)
        -> (Jv.t -> 'a2)
        -> (Jv.t -> 'a3)
        -> (Jv.t -> 'a4)
        -> (Jv.t -> 'a5)
        -> (Jv.t -> 'a6)
        -> ('r -> Jv.t)
        -> ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'r)
        -> Jv.t
end
