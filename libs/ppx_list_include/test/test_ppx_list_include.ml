open Base

let eq_list_int = [%test_eq: int list] (* uninlined so the generated code is readable *)
let dyn x = x

let%test_unit "basic usage" =
  let l1 = [ 1
           ; +[2; 3] (* static inclusion case *)
           ; 4
           ; +(let x = 5 in [x; x+1]) (* dynamic inclusion case *)
           ; 7
           ; 4 + List.length [0;+[1;2;3]] (* nested inclusion *)
           ]
  in
  eq_list_int l1 (List.init 8 ~f:(fun x -> x + 1));
  let l2 = 1 :: +[2] :: dyn [3] (* dynamic tail of list *) in
  eq_list_int l2 (List.init 3 ~f:(fun x -> x + 1))

let%test_unit "optimized cases" = (
  eq_list_int [+[1;2]] [1;2];
  eq_list_int [+dyn [1;2]] [1;2];
  eq_list_int [0; +dyn [1;2]] [0;1;2];
)
