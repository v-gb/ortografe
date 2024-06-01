(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type 'a t = {mutable ff : exn cont -> unit cont -> 'a cont -> trampoline}

let make f = {ff = f}
let f =
  match Sys.backend_type with
  | Other "js_of_ocaml" ->
     fun {ff} -> (fun throw e k ->
       ff (fun a -> Go_back (fun () -> throw a))
         (fun a -> Go_back (fun () -> e a))
         (fun a -> Go_back (fun () -> k a)))
  | _ -> fun {ff} -> ff

let construct c =
  let s = ref None in
  (fun throw e k ->
    match !s with
    | None -> c throw (fun s' -> s := Some s'; f s' throw e k)
    | Some s' -> f s' throw e k)
  |> make

let empty () = (fun _ e _ -> e ()) |> make

let next t throw e k = (f t) throw e k

let next_option t throw k = (f t) throw (fun () -> k None) (fun v -> k (Some v))

let next_expected t throw k =
  (f t) throw (fun () -> throw (Failure "stream empty")) k

let next_n n s throw k =
  if n < 0 then throw (Invalid_argument "n is negative")
  else
    let rec iterate acc = function
      | 0 -> k (List.rev acc)
      | n ->
        next s throw
          (fun () -> iterate acc 0) (fun v -> iterate (v::acc) (n - 1))
    in

    iterate [] n

let push ({ff} as s) v = s.ff <- fun _ _ k -> s.ff <- ff; k v

let push_option s = function
  | None -> ()
  | Some v -> push s v

let push_list ({ff} as s) = function
  | [] -> ()
  | vs ->
    let remainder = ref vs in
    s.ff <- fun throw e k ->
      match !remainder with
      | [] -> s.ff <- ff; ff throw e k
      | v::vs -> remainder := vs; k v

let peek s throw e k = next s throw e (fun v -> push s v; k v)

let peek_option s throw k =
  peek s throw (fun () -> k None) (fun v -> k (Some v))

let peek_expected s throw k =
  peek s throw (fun () -> throw (Failure "stream empty")) k

let peek_n n s throw k = next_n n s throw (fun vs -> push_list s vs; k vs)

let tap g ({ff} as s) =
  (s.ff <- fun throw e k -> ff throw e (fun v -> g v; k v));
  fun () -> s.ff <- ff

let checkpoint s =
  let buffer = ref [] in
  let s' =
    (fun throw e k ->
      (f s) throw e (fun v -> buffer := v::!buffer; k v))
    |> make
  in
  let restore () = push_list s (List.rev !buffer) in
  s', restore

let transform f init s =
  let current_acc = ref (Some init) in
  let to_emit = ref [] in
  let rec operate throw e k =
    match !to_emit with
    | v::more -> to_emit := more; k v
    | [] ->
      match !current_acc with
      | None -> e ()
      | Some acc ->
        next s throw e (fun v ->
          f acc v throw (fun (vs, acc') ->
            to_emit := vs;
            current_acc := acc';
            operate throw e k))
  in
  make operate

let map f s = (fun throw e k -> next s throw e (fun v -> f v throw k)) |> make

let rec fold f v s throw k =
  next s throw
    (fun () -> k v)
    (fun v' -> f v v' throw (fun v'' -> fold f v'' s throw k))

let iter f s throw k = fold (fun () v throw k -> f v throw k) () s throw k

let filter_map f s =
  let rec emit throw e k =
    next s throw e (fun v ->
      f v throw (function
        | None -> emit throw e k
        | Some v -> k v))
  in
  make emit

let filter f s =
  s |> filter_map (fun v throw k ->
    f v throw (function
      | true -> k (Some v)
      | false -> k None))

let of_list l =
  let l = ref l in
  (fun _ e k ->
    match !l with
    | [] -> e ()
    | v::l' -> l := l'; k v)
  |> make

let to_list s throw k =
  fold (fun l v _ k -> k (v::l)) [] s throw (fun l -> k (List.rev l))

let enumerate s =
  let index = ref 0 in
  s |> map (fun v _ k -> index := !index + 1; k ((!index - 1), v))
