open Core

module K = struct
  type t = int * string [@@deriving bin_io, compare, sexp_of]

  include (val Comparator.make ~sexp_of_t ~compare)
end

let strip_marks str =
  let b = Buffer.create (String.length str) in
  Uunf_string.normalize_utf_8 `NFD str
  |> Uutf.String.fold_utf_8
       (fun () _ -> function
         | `Uchar c ->
             if Stdlib.Uchar.to_int c = 339
             then Buffer.add_string b "oe"
             else if not (Uucp.Func.is_diacritic c)
             then Stdlib.Buffer.add_utf_8_uchar b c
         | `Malformed s -> Buffer.add_string b s)
       ();
  Buffer.contents b

let is_compat str =
  let re =
    Of_iter.list (fun yield ->
        let e_re = Re.(alt [ str "e"; str "é"; str "è"; str "ê"; str "ë" ]) in
        let o_re = Re.(alt [ str "o"; str "ô"; str "ö" ]) in
        let i = ref 0 in
        while !i < String.length str do
          (match String.get str !i with
          | 'a' -> yield Re.(alt [ str "a"; str "à"; str "â"; str "ä" ])
          | 'e' -> yield e_re
          | 'i' -> yield Re.(alt [ str "i"; str "î"; str "ï" ])
          | 'o' ->
              if !i + 1 >= String.length str
              then yield Re.(alt [ seq [ o_re ]; str "œ" ])
              else if Char.( = ) str.[!i + 1] 'e'
              then (
                yield Re.(alt [ seq [ o_re; e_re ]; str "œ" ]);
                i := !i + 1)
              else yield o_re
          | 'u' -> yield Re.(alt [ str "u"; str "ù"; str "û"; str "ü" ])
          | 'y' -> yield Re.(alt [ str "y"; str "ÿ" ])
          | 'c' -> yield Re.(alt [ str "c"; str "ç" ])
          | c -> yield (Re.char c));
          i := !i + 1
        done)
    |> fun res -> Re.compile Re.(seq (bos :: res))
  in
  fun (str, _) -> Re.execp re str

type 'a t =
  { index : 'a array Map.M(String).t Map.M(K).t
  ; max_length : int
  }
[@@deriving bin_io]

let create iter =
  let index =
    Of_iter.list (fun yield ->
        iter (fun a str ->
            let str' = strip_marks str in
            yield ((String.length str', str'), (str, a))))
    |> Map.of_alist_multi (module K)
    |> Map.map ~f:(fun l ->
           Map.of_alist_multi (module String) l
           (* The 'a here are in order that iter gives them to us. We could take a
              compare function like search, but in search, the ordering is meaningless,
              only the equality is, so the situation is different, so it doesn't follow
              that we should do what search does. *)
           |> Map.map ~f:Array.of_list)
  in
  { index; max_length = Map.max_elt_exn index |> fst |> fst }

let search (type a) ({ index; max_length } : a t) term ~compare ~limit =
  let markless_term = strip_marks term in
  let search n =
    Map.to_sequence index ~keys_greater_or_equal_to:(n, markless_term)
      ~keys_less_or_equal_to:(n, markless_term ^ "zzzzzzzzzzzzzzzzzzzzzzzzz")
    |> Sequence.map ~f:snd
  in
  let sequence_take_unique (type a) ~compare s limit =
    let module C = struct
      type t = a

      let compare : t -> t -> int = compare
      let sexp_of_t = [%sexp_of: _]

      include (val Comparator.make ~sexp_of_t ~compare)
    end in
    Sequence.unfold_step
      ~init:(limit, s, Set.empty (module C))
      ~f:(fun (remaining, s, seen) ->
        if remaining <= 0
        then Done
        else
          match Sequence.next s with
          | None -> Done
          | Some (a, s) ->
              if Set.mem seen a
              then Skip { state = (remaining, s, seen) }
              else Yield { value = a; state = (remaining - 1, s, Set.add seen a) })
  in
  let all_matches =
    List.init
      (1 + max_length - String.length markless_term)
      ~f:(fun i -> i + String.length markless_term)
    |> Sequence.of_list
    |> Sequence.concat_map ~f:search
    |> Sequence.concat_map ~f:(fun map -> Sequence.of_list (Map.to_alist map))
    |> Sequence.filter ~f:(is_compat term)
    |> Sequence.concat_map ~f:(fun (a, l) ->
           Array.to_sequence_mutable (Array.map l ~f:(fun b -> (a, b))))
    |> (fun l ->
    sequence_take_unique ~compare:(fun (_, a1) (_, a2) -> compare a1 a2) l limit)
    |> Sequence.to_list
  in
  all_matches

module Erofa = struct
  type index = int

  let compare_index a b =
    let a = if a < 0 then a asr 2 else a in
    let b = if b < 0 then b asr 2 else b in
    Int.compare a b

  module T = struct
    type nonrec t = (string list * string * string * int) array * int t
    [@@deriving bin_io]
  end

  include T

  let to_persist pair = Binable.to_string (module T) pair
  let of_persist str = Binable.of_string (module T) str

  type flags = { implied_plural : bool }

  let flags bits = { implied_plural = bits mod 2 = 1 }
  let convert_record (a, b, c, d) = (a, b, c, flags d)

  let search (q, t) term ~limit =
    search t ~compare:compare_index term ~limit
    |> List.map ~f:(fun (str, a) ->
           if a >= 0
           then convert_record q.(a)
           else
             let a = -a in
             let str =
               if a land 2 <> 0 then String.chop_suffix_exn str ~suffix:"s" else str
             in
             convert_record ([], str, str, a mod 2))
end
