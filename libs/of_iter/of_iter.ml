let list f =
  let r = ref [] in
  f (fun x -> r := x :: !r);
  List.rev !r

