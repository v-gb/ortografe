(** A module that implements approximate dictionary search.

    Specifically, given a request "nec" :
    - it returns words that starts with that request, like "nectar",
      including with diacritics like "nécessaire"
    - if the query contains diacritics, then completions will only
      contain words that respect such diacritics
    - it returns the shortest completions first
    - it doesn't return duplicate entries if the both words match
    - œ is treated as oe
    - it's quick (20us to 400us kind of numbers)
 *)

type 'a t [@@deriving bin_io]
val create : (('a -> string -> unit) -> unit) -> 'a t
val search : 'a t -> string -> compare:('a -> 'a -> int) -> limit:int -> (string * 'a) list

module Erofa : sig
  type nonrec t =
    (string list * string * string) array * int t
  val to_persist : t -> string
  val of_persist : string -> t
end
