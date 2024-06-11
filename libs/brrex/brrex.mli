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

val read_bytes : Brr.Blob.t -> string Fut.or_error
val read_bytes_from_file : Brr.File.t -> string Fut.or_error

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
  type 'a t = Jv.t -> 'a
  type 'a t' = 'a -> Jv.t
  val unit : unit t
  val jstr : Jstr.t t
  val string : string t
  val bool : bool t
  val int : int t
  val magic : _ t
  val jv : Jv.t t
  val option : 'a t -> 'a option t
  val map : ('i -> 'a) -> 'i t -> 'a t
  val t2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
  val t3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
  val t4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
  val fun1 : 'a1 t' -> 'r t -> ('a1 -> 'r) t

  val unit' : unit t'
  val jstr' : Jstr.t t'
  val string' : string t'
  val bool' : bool t'
  val int' : int t'
  val magic' : _ t'
  val jv' : Jv.t t'
  val option' : ('a t') -> 'a option t'
  val map' : ('a -> 'r) -> 'r t' -> 'a t'
  val promise_or_error' : 'a t' -> 'a Fut.or_error t'
  val t2' : 'a1 t' -> 'a2 t' -> ('a1 * 'a2) t'
  val t3' : 'a1 t' -> 'a2 t' -> 'a3 t' -> ('a1 * 'a2 * 'a3) t'
  val t4' : 'a1 t' -> 'a2 t' -> 'a3 t' -> 'a4 t' -> ('a1 * 'a2 * 'a3 * 'a4) t'
  val fun1' : 'a1 t -> 'r t' -> ('a1 -> 'r) t'
  val fun2' : 'a1 t -> 'a2 t -> 'r t' -> ('a1 -> 'a2 -> 'r) t'
  val fun3' : 'a1 t -> 'a2 t -> 'a3 t -> 'r t' -> ('a1 -> 'a2 -> 'a3 -> 'r) t'
  val fun4' : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'r t' -> ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'r) t'
  val fun5' : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'r t' -> ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'r) t'
  val fun6' : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'r t' -> ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'r) t'

  val magic_ : unit -> (Jv.t -> 'a) * ('a -> Jv.t)
end
