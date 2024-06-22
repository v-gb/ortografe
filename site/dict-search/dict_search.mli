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
  type index = int
  (** When >= 0, an index in the array below. When < 0, it indicates an unchanged entry
     (which is omitted from the array). When < 0, the two lower bits encode information
     other than the identity of the entry:
     - 1 lsl 0 indicates whether the entry also includes a plural in s
     - 1 lsl 1 indictates whether the matched word is the singular or the plural
       (only meaningful when bit zero is also set)
     *)

  type nonrec t =
    (string list * string * string * int) array * index t
  val to_persist : t -> string
  val of_persist : string -> t

  type flags = { implied_plural : bool }
  val search : t -> string -> limit:int -> (string list * string * string * flags) list
end
