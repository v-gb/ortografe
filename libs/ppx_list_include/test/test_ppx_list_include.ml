open Base

let%test_unit _ =
  let l1 = [ 1
           ; +[2; 3] (* static inclusion case *)
           ; 4
           ; +(let x = 5 in [x; x+1]) (* dynamic inclusion case *)
           ; 7
           ; 4 + List.length [0;+[1;2;3]] (* nested inclusion *)
           ]
  in
  [%test_eq: int list] l1 (List.init 8 ~f:(fun x -> x + 1));
  let l2 = 1 :: +[2] :: (let x = [3] in x) (* dynamic tail of list *) in
  [%test_eq: int list] l2 (List.init 3 ~f:(fun x -> x + 1));
