open Core

module Change = struct
  type t =
    [ `Unchanged
    | `Old of string
    | `New of string
    ]
      [@@deriving bin_io, compare, sexp_of]
  include (val Comparator.make ~sexp_of_t ~compare)       
end

module K = struct
  type t = int * string
                   [@@deriving bin_io, compare, sexp_of]
  include (val Comparator.make ~sexp_of_t ~compare)       
end

let strip_marks str =
  let b = Buffer.create (String.length str) in
  Uunf_string.normalize_utf_8 `NFD str
  |> Uutf.String.fold_utf_8 (fun () _ -> function
         | `Uchar c ->
            if Stdlib.Uchar.to_int c = 339
            then Buffer.add_string b "oe"
            else
              if not (Uucp.Func.is_diacritic c)
              then Stdlib.Buffer.add_utf_8_uchar b c
         | `Malformed s ->
            Buffer.add_string b s) ();
  Buffer.contents b

let is_compat str =
  (* le traitement des œ est un peu redondant avec le fait que le dictionnaire a des
     entrées avec oe et œ *)
  let oe_pattern = String.Search_pattern.create "œ" in
  let re =
    String.Search_pattern.replace_all oe_pattern ~with_:"oe" ~in_:str
    |> String.to_list
    |> List.map ~f:(function
           | 'a' -> Re.(alt [ str "a"; str "à"; str "â"; str "ä" ])
           | 'e' -> Re.(alt [ str "e"; str "é"; str "è"; str "ê"; str "ë" ])
           | 'i' -> Re.(alt [ str "i"; str "î"; str "ï" ])
           | 'o' -> Re.(alt [ str "o"; str "ô"; str "ö" ])
           | 'y' -> Re.(alt [ str "y"; str "ÿ" ])
           | 'c' -> Re.(alt [ str "c"; str "ç" ])
           | c -> Re.char c)
    |> (fun res -> Re.compile (Re.(seq (bos :: res))))
  in
  fun (str, _) ->
  Re.execp re (String.Search_pattern.replace_all oe_pattern ~with_:"oe" ~in_:str)

type t =
  { index : Change.t array Map.M(String).t Map.M(K).t
  ; max_length : int
  }
[@@deriving bin_io]

let create iter =
  let index =
    Of_iter.list (fun yield ->
        iter (fun a b ->
            if String.(=) a b
            then
              let a' = strip_marks a in
              yield ((String.length a', a'), (a, `Unchanged))
            else (
              let a' = strip_marks a in
              let b' = strip_marks b in
              yield ((String.length a', a'), (a, `New b));
              yield ((String.length b', b'), (b, `Old a)))))
    |> Map.of_alist_multi (module K)
    |> Map.map ~f:(fun l ->
           Map.of_alist_multi (module String) l
           |> Map.map ~f:(fun l ->
                  List.dedup_and_sort ~compare:Change.compare l
                  |> Array.of_list))
  in
  { index; max_length = Map.max_elt_exn index |> fst |> fst }

let to_persist t =
  Binable.to_string
    (module struct
       type nonrec t = t [@@deriving bin_io]
     end) t

let of_persist str =
  Binable.of_string
    (module struct
       type nonrec t = t [@@deriving bin_io]
     end) str

let search { index; max_length } term ~limit =
  let markless_term = strip_marks term in
  let search n =
    Map.to_sequence index
      ~keys_greater_or_equal_to:(n, markless_term)
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
    List.init (1 + max_length - String.length markless_term) ~f:(fun i ->
        i + String.length markless_term)
    |> Sequence.of_list
    |> Sequence.concat_map ~f:search
    |> Sequence.concat_map ~f:(fun map -> Sequence.of_list (Map.to_alist map))
    |> Sequence.filter ~f:(is_compat term)
    |> Sequence.concat_map ~f:(fun (a, l) ->
           Array.to_sequence_mutable
             (Array.map l ~f:(function
                  | `Unchanged -> (a, a)
                  | `Old b -> (b, a)
                  | `New b -> (a, b))))
    |> (fun l -> sequence_take_unique ~compare:[%compare: string * string] l limit)
    |> Sequence.to_list
  in
  all_matches
