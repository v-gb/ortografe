open Base

let ( >$ ) = ( > )
let ( <$ ) = ( < )
let ( =$ ) = ( = )
let ( <>$ ) = ( <> )
let ( <=$ ) = ( <= )
let ( >=$ ) = ( >= )
let ( > ) = String.( > )
let ( < ) = String.( < )
let ( = ) = String.( = )
let ( <> ) = String.( <> )
let ( <= ) = String.( <= )
let ( >= ) = String.( >= )

let[@ocamlformat "disable"] _ =
  ((>$), (<$), (=$), (<>$), (<=$), (>=$), (>), (<), (=), (<>), (<=), (>=))

let utf8_exists_non_shortcut str ~f =
  let found = ref false in
  let i = ref 0 in
  while !i <$ String.length str do
    let decode = Stdlib.String.get_utf_8_uchar str !i in
    found := !found || f (Stdlib.Uchar.utf_decode_uchar decode);
    i := !i + Stdlib.Uchar.utf_decode_length decode
  done;
  !found

let uchar_of_str str =
  let utf_decode = Stdlib.String.get_utf_8_uchar str 0 in
  assert (Stdlib.Uchar.utf_decode_length utf_decode =$ String.length str);
  Stdlib.Uchar.utf_decode_uchar utf_decode

let _ = uchar_of_str

let str_of_uchar uchar =
  let b = Bytes.create (Uchar.utf8_byte_length uchar) in
  ignore (Stdlib.Bytes.set_utf_8_uchar b 0 uchar : int);
  Bytes.to_string b

let _ = str_of_uchar

let sub str a b =
  let a = Int.clamp_exn a ~min:0 ~max:(String.length str) in
  let b = Int.clamp_exn b ~min:a ~max:(String.length str) in
  String.sub str ~pos:a ~len:(b - a)

let ( #: ) str (a, b) = sub str a b

let prev_uchar str i =
  if i >=$ 1 && Char.( < ) str.[i - 1] '\128'
  then i - 1
  else if i >=$ 2 && Char.( >= ) str.[i - 2] '\192'
  then i - 2
  else if i >=$ 3 && Char.( >= ) str.[i - 3] '\224'
  then i - 3
  else i - 4

let ( #:: ) str (i, n) =
  if n >=$ 0
  then (
    let i = ref i in
    for _ = 1 to n do
      if !i <$ String.length str
      then
        i := !i + Stdlib.Uchar.utf_decode_length (Stdlib.String.get_utf_8_uchar str !i)
    done;
    if !i >=$ String.length str
    then Stdlib.Uchar.rep
    else Stdlib.Uchar.utf_decode_uchar (Stdlib.String.get_utf_8_uchar str !i))
  else
    let i = ref i in
    for _ = 1 to -n do
      if !i >=$ 0 then i := prev_uchar str !i
    done;
    if !i <$ 0
    then Stdlib.Uchar.rep
    else Stdlib.Uchar.utf_decode_uchar (Stdlib.String.get_utf_8_uchar str !i)

let ortho_weak_vowels =
  Hash_set.of_list (module Uchar) [ !!"e"; !!"é"; !!"è"; !!"ê"; !!"i"; !!"î"; !!"y" ]

let in_ortho_weak_vowels s = Hash_set.mem ortho_weak_vowels s

let[@ocamlformat "disable"] ortho_vowels =
  Hash_set.union ortho_weak_vowels
    (Hash_set.of_list
       (module Uchar)
       [ !!"a"; !!"à"; !!"â"; !!"ä"; !!"ë"; !!"ï"; !!"ù"; !!"o"; !!"ô"; !!"ö"; !!"u"; !!"û"; !!"ù"; !!"ü"; !!"ÿ"])

let in_ortho_vowels s = Hash_set.mem ortho_vowels s

let[@ocamlformat "disable"] phon_vowels =
  Hash_set.of_list
    (module Uchar)
    [ !!"a"; !!"e"; !!"E"; !!"2"; !!"9"; !!"°"; !!"@"; !!"5"; !!"§"; !!"1"; !!"i"; !!"y"; !!"u"; !!"o"; !!"O"; !!"ē"]

let in_phon_vowels s = Hash_set.mem phon_vowels s

type importance =
  | Core_optional
    (* Core_optional veut dire : règles usuelles du français, ce sont les règles
       qu'on peut utiliser autant qu'on veut. *)
  | Core
    (* Core est comme Core_optional mais en plus, quand une règle Core matche une section
       d'un mot, on préfère une telle règle à une règle plus courte. Par exemple "ai" /e/
       est Core, parce que même si "a" /a/ et "i" /i/ sont Core aussi, il serait très
       surprenant d'avoir "mai" prononcé /mai/ plutôt que /me/. *)
  | Surprising
(* Surprising veut dire : on peut exprimer des mots avec ces règles mais
   on ne veut pas introduire de nouveaus usages dans les réécritures. *)

type rule_fun = string -> int -> int -> (string * importance) list
type rule = string * rule_fun
type t = (char, rule array) Hashtbl.t

let create () : t =
  let r : rule list ref = ref [] in
  let new_ a f = r := (a, f) :: !r in
  let new_fixed graphem l = new_ graphem (fun _ _ _ -> l) in

  (* ponctuation *)
  new_fixed "'" [ ("", Core) ];
  new_fixed "-" [ ("", Core) ];
  new_fixed " " [ ("", Core) ];

  (* consonnes simples *)
  new_ "b" (fun word _ j ->
      match word #: (j, j + 1) with
      | "s" | "t" | "c" -> [ ("b", Core); ("p", Core) ]
      | _ -> [ ("b", Core) ]);
  new_fixed "bb" [ ("b", Core) ];
  (* c n'est pas simple, mais ç est simple *)
  new_fixed "ç" [ ("s", Core) ];
  new_fixed "sç" [ ("s", Core) ];
  (* immisçait *)
  new_fixed "d" [ ("d", Core) ];
  new_fixed "dd" [ ("d", Core) ];
  new_fixed "f" [ ("f", Core) ];
  new_fixed "ff" [ ("f", Core) ];
  (* g est un peu compliqué *)
  new_fixed "h" [ ("", Core) ];
  new_fixed "j" [ ("Z", Core) ];
  new_fixed "k" [ ("k", Core) ];
  new_fixed "kk" [ ("k", Core) ];
  new_fixed "ck" [ ("k", Core) ];
  (* "il" peut être compliqué, mais c'est géré plus loin *)
  new_fixed "l" [ ("l", Core) ];
  new_fixed "ll" [ ("l", Core) ];
  (* interactions avec les voyelles sont plus loin *)
  new_fixed "m" [ ("m", Core) ];
  new_fixed "mm" [ ("m", Core) ];
  new_fixed "n" [ ("n", Core) ];
  new_fixed "nn" [ ("n", Core) ];
  new_fixed "p" [ ("p", Core) ];
  new_fixed "pp" [ ("p", Core) ];
  new_fixed "ph" [ ("f", Core) ];
  List.iter [ ""; "c" ] ~f:(fun prefix ->
      new_fixed (prefix ^ "qu") [ ("k", Core); ("kw", Surprising) ];
      new_fixed (prefix ^ "q") [ ("k", Core) ]);
  new_fixed "r" [ ("R", Core) ];
  new_fixed "rr" [ ("R", Core) ];
  (* s est compliqué *)
  (* t tout seul est un peu compliqué *)
  new_fixed "tt" [ ("t", Core) ];
  new_fixed "v" [ ("v", Core) ];
  new_fixed "w" [ ("w", Core); ("v", Surprising) ];
  (* il y plus de règles pour les x silencieux plus bas *)
  new_fixed "x"
    [ ("gz", Core)
    ; ("ks", Core)
    ; ("s", Surprising (* dix *))
    ; ("z", Surprising (* deuxième *))
    ];
  new_fixed "z" [ ("z", Core); ("s", Surprising (* tzigane, pré 90 *)) ];
  new_fixed "zz" [ ("z", Core) ];

  (* consonnes plus compliquées, et combinaisons de consonnes *)
  (* C *)
  new_ "c" (fun word _ j ->
      if in_ortho_weak_vowels word #:: (j, 0)
      then [ ("s", Core) ]
      else [ ("k", Core); ("g", Surprising) ]);
  new_fixed "ch" [ ("k", Core); ("S", Core) ];
  new_fixed "cch" [ ("k", Core) ];
  new_fixed "sch" [ ("S", Core) ];
  new_ "sc" (fun word _ j ->
      if in_ortho_weak_vowels word #:: (j, 0) then [ ("s", Core) ] else [ ("sk", Core) ]);
  new_ "cc" (fun word _ j ->
      if in_ortho_weak_vowels word #:: (j, 0) then [ ("ks", Core) ] else [ ("k", Core) ]);
  new_ "xc" (fun word _ j ->
      if in_ortho_weak_vowels word #:: (j, 0) then [ ("ks", Core) ] else [ ("k", Core) ]);

  (* G *)
  new_ "gu" (fun word _ j ->
      if in_ortho_vowels word #:: (j, 0)
      then [ ("g", Core); ("gw", Surprising) ]
      else [ ("gy", Core) ]);
  new_ "g" (fun word _ j ->
      if in_ortho_weak_vowels word #:: (j, 0) then [ ("Z", Core) ] else [ ("g", Core) ]);
  new_fixed "guë" [ ("gy", Core) ];
  new_fixed "gn" [ ("N", Core) ];
  new_fixed "gn" [ ("nj", Core) ] (* pour faire marcher la réécriture gn/ni. *);
  new_fixed "ign" [ ("N", Surprising) ];
  new_fixed "gni" [ ("N", Core) ];
  new_fixed "ni" [ ("N", Core_optional) ];
  new_fixed "ng" [ ("G", Core) ];

  (* L qui fait /j/
     Le découpage choisit est que travail est a|il, et travailleur est
     a|ill|eu|r. Ça marche pour accueil sans avoir à créer des
     graphèmes pour toutes les voyelles précédentes, mais pas pour
     ensommeillé. *)
  List.iter
    [ ("il$", Core)
    ; ("ils$", Core)
    ; ("ill", Core)
    ; ("illi", Core_optional (* joaillier, mais pas milliseconde, donc optionnel *))
    ]
    ~f:(fun (digraph, imp) ->
      new_fixed digraph [ ("il", imp); ("ij", imp); ("j", imp) ]);
  new_fixed "il" [ ("il", Core); ("ij", Surprising); ("j", Surprising) ];
  (* gentilhomme *)
  List.iter [ "eil$"; "eils$"; "eill"; "eilli" ] ~f:(fun digraph ->
      new_fixed digraph [ ("ej", Core); ("Ej", Core) ]);

  (* S *)
  new_fixed "ss" [ ("s", Core) ];
  new_ "s" (fun word i j ->
      if in_ortho_vowels word #:: (j, 0) && in_ortho_vowels word #:: (i, -1)
      then [ ("z", Core); ("s", Surprising) ]
      else if word #: (j, j + 1) = "m" || word #: (i - 4, i) = "tran"
      then
        (* I think this is mostly wrong, but lexique has dubious prononciations *)
        [ ("z", Core); ("s", Core) ]
      else
        (* bonshommes, gentilshommes, subsiste, lesbien, bonsaï, isthme *)
        [ ("s", Core); ("z", Surprising) ]);
  new_fixed "sh" [ ("S", Core) ];
  new_fixed "désh" [ ("dez", Core) ] (* déshabiller, pour pas que sh se prononce sh *);

  (* T *)
  new_ "t" (fun word _ j ->
      if word #: (j, j + 1) = "i" then [ ("s", Core); ("t", Core) ] else [ ("t", Core) ]);
  List.iter [ "t$"; "ts$" ] ~f:(fun digraph ->
      new_ digraph (fun word i _ ->
          (* Le cas surprising nous permet de dire que mamout avec un t prononcé est
             surprenant/douteux *)
          if word #: (i - 2, i) = "ac"
          then [ ("", Core); ("t", Core) ]
          else [ ("", Core); ("t", Surprising) ]));
  new_fixed "th" [ ("t", Core); ("", Surprising) ] (* asthme *);

  (* consonnes, cas bizarres et consonnes muettes *)
  new_fixed "p" [ ("", Surprising) (* temps *) ];
  new_fixed "l" [ ("", Surprising) (* fils *) ];
  new_fixed "l$" [ ("", Surprising) (* gentil *) ];
  new_fixed "aul" [ ("o", Surprising) (* gentil *) ];
  new_fixed "g" [ ("", Surprising) (* sang *) ];
  new_fixed "x$" [ ("", Core) (* ?? too general? *) ];
  new_fixed "ct$" [ ("", Surprising) (* respect *) ];
  new_fixed "cts$" [ ("", Surprising) (* respects *) ];
  new_fixed "c$" [ ("", Surprising) (* blanc *) ];
  new_fixed "cs$" [ ("", Surprising) (* blancs *) ];
  new_fixed "d$" [ ("", Surprising) (* pied *) ];
  new_fixed "ds$" [ ("", Surprising) (* pieds *) ];
  new_fixed "f$" [ ("", Surprising) (* oeuf *) ];
  new_fixed "fs$" [ ("", Surprising) (* oeufs *) ];
  new_fixed "b$" [ ("", Surprising) (* plomb *) ];
  new_fixed "bs$" [ ("", Surprising) (* plombs *) ];
  new_fixed "t" [ ("", Surprising) (* montgolfière *) ];
  new_fixed "s$" [ ("", Core) ];
  new_fixed "s" [ ("", Surprising) (* disjoncteur *) ];
  new_fixed "c" [ ("", Surprising) (* succinctement *) ];
  new_fixed "coe" [ ("se", Surprising) (* coelacanthe *) ];
  new_fixed "cœ" [ ("se", Surprising) (* coelacanthe *) ];
  new_fixed "sc" [ ("S", Surprising) (* fasciste *) ];
  new_fixed "iz$" [ ("i", Surprising) (* riz *) ];
  new_fixed "az$" [ ("a", Surprising) (* raz *) ];
  new_fixed "mn" [ ("mn", Core); ("n", Surprising) ] (* damné *);

  (* voyelles, sauf voyelles nasales *)
  (* A *)
  new_fixed "à" [ ("a", Core) ];
  new_fixed "â" [ ("a", Core) ];
  new_fixed "ä" [ ("a", Core) ];
  new_fixed "a" [ ("a", Core) ];
  new_fixed "au" [ ("o", Core) ];
  new_ "aux" (fun word _ j ->
      (* wish I could say "otherwise treat it as not a graphem" *)
      if in_ortho_weak_vowels word #:: (j, 0)
      then [ ("oks", Core); ("ogz", Core) ]
      else [ ("o", Core) ] (* auxquelles *));
  new_fixed "ai"
    [ ("e", Core); ("E", Core); ("°", Surprising); ("2", Surprising (* faisant *)) ];
  new_fixed "aî" [ ("e", Core); ("E", Core) ];
  new_fixed "ay"
    [ ("aj", Core)
    ; ("e", Core)
    ; ("E", Core)
    ; ("ej", Core)
    ; ("Ej", Core)
    ; ("ei", Core)
    ; ("Ei", Core)
    ];
  new_fixed "aou" [ ("u", Surprising) ] (* aout *);

  (* E *)
  new_fixed "è"
    [ ("E", Core)
    ; ( "e"
      , Surprising
        (* Quand on ajuste le lexique avec l'orthographe post
           90 sans ajuster la phonétique en même temps. *) )
    ];
  new_fixed "ë" [ ("e", Core) ];
  new_fixed "ê" [ ("e", Core); ("E", Core) ];
  new_fixed "é" [ ("e", Core); ("E", Surprising (* médecin *)) ];
  let syllable_starts =
    Hash_set.of_list
      (module String)
      (List.cartesian_product
         [ "b"; "c"; "d"; "f"; "g"; "j"; "k"; "p"; "q"; "s"; "t"; "v"; "w"; "z" ]
         [ "r"; "l" ]
      |> List.map ~f:(fun (a, b) -> a ^ b))
  in
  new_ "e" (fun word i j ->
      if String.length word >$ j
         && Uchar.( = ) word #:: (j, 0) word #:: (j, 1)
         && not (in_ortho_vowels word #:: (j, 0))
      then
        [ ("e", Core)
        ; ("E", Core (* messe, eE dépend de l'ouverture de la syllabe  *))
        ; ("°", Surprising)
        ; ("2", Surprising (* dessus, ressauter *))
        ]
      else
        let _ = i in
        if in_ortho_vowels word #:: (j, 0) (* pas le droit d'enlever le h de dehors *)
        then [ ("", Core) ]
        else if String.length word =$ j (* un e en fin de mot n'est jamais é ou è *)
                || (String.length word =$ j + 1 && Char.( = ) word.[j] 's')
        then [ ("2", Core); ("°", Core); ("", Core) ]
        else
          let syllable_is_unfinished =
            (not (in_ortho_vowels word #:: (j, 0)))
            && ((not (in_ortho_vowels word #:: (j, 1)))
               || Uchar.( = ) word #:: (j, 1) Stdlib.Uchar.rep (* fin de mot *)
               || Uchar.( = ) word #:: (j, 0) !!"x")
            && (not (Hash_set.mem syllable_starts word #: (j, j + 2)))
            && not
                 ((word #: (j, j + 2) = "ch" || word #: (j, j + 2) = "th")
                 && in_ortho_vowels word #:: (j, 2))
          in
          if syllable_is_unfinished
          then
            [ ("e", Core)
            ; ("E", Core)
            ; ("°", Surprising)
            ; ("2", Surprising (* restructuration *))
            ]
          else
            [ ("°", Core)
            ; ("2", Core)
            ; ("", Core)
            ; ("e", Surprising)
            ; ("E", Surprising (* papeterie mais papetier ?? *))
            ; ("9", Surprising)
            ]);
  new_fixed "ei" [ ("e", Core); ("E", Core) ];
  new_fixed "ey"
    [ ("e", Core); ("E", Core); ("ej", Core); ("Ej", Core); ("ei", Core); ("Ei", Core) ];
  List.iter [ "eu"; "eû"; "eux" ] ~f:(fun g ->
      new_fixed g [ ("2", Core); ("9", Core); ("y", Surprising (* avoir *)) ]);
  List.iter [ "er$"; "ers$" ] ~f:(fun digraph ->
      new_fixed digraph [ ("e", Core); ("Er", Core) ]);
  new_fixed "ez$" [ ("e", Core); ("E", Core) ];
  (* avez *)
  new_fixed "es" [ ("e", Surprising); ("E", Surprising) ];
  (* lesquelles *)
  new_fixed "ess" [ ("es", Core); ("Es", Core) ];
  (* « essayer » a la prononciation
     attendue, donc on ne veut pas utiliser
     la règle précédente. *)
  List.iter [ "eau"; "eaux" ] ~f:(fun g -> new_fixed g [ ("o", Core) ]);
  new_fixed "et$" [ ("E", Core) ];
  new_fixed "est$" [ ("Est", Core) ];
  new_fixed "est$" [ ("e", Core) ];
  new_fixed "ests$" [ ("Est", Core) ];

  (* I *)
  new_fixed "î" [ ("i", Core) ];
  new_fixed "ï" [ ("i", Core) ];
  (* maïs *)
  new_fixed "ï" [ ("j", Core) ];
  (* paranoïaque *)
  new_ "i" (fun word i j ->
      (* really want to look at past phonem, not past letter here. Even past phonem is a
         problem, because of syllable boundary *)
      if i =$ 0
      then [ ("j", Core); ("i", Core) ]
      else if j <$ String.length word && not (in_ortho_vowels word #:: (j, 0))
      then
        [ ("i", Core) ]
        (* on ne permet pas le son en face d'une consonne, pour éviter
           que «sommeil» puisse être interprété avec i/j/ et l silencieux *)
      else if in_ortho_vowels word #:: (i, -1) (* évite tuyau -> tuiau *)
      then [ ("j", Core); ("i", Core) ]
      else
        (* Pas facile d'éviter d'éviter de permettre à i de prendre le son y.
           "inquiète", "voyiez", "jouiez", "figuier", "théière". *)
        [ ("ij", Core); ("j", Core); ("i", Core) ]);

  (* O *)
  new_fixed "o" [ ("o", Core); ("O", Core) ];
  new_fixed "ô" [ ("o", Core) ];
  new_fixed "ö" [ ("o", Core) ];
  new_fixed "oo"
    [ ("oo", Core (* zoo *))
    ; ("oO", Core (* cohorte *))
    ; ("O", Surprising)
    ; ("o", Surprising (* alcool *))
    ];
  new_fixed "oi" [ ("wa", Core) ];
  new_fixed "oî" [ ("wa", Core) ];
  new_fixed "oix$" [ ("wa", Core) ];
  new_fixed "oy"
    [ ("oj", Core); ("Oj", Core); ("wa", Core); ("waj", Core); ("wai", Core) ];
  List.iter [ "ou"; "oû"; "où" ] ~f:(fun digraph ->
      new_ digraph (fun word i _ ->
          (* [true ||] pour faire marcher cacahouète. Pas sûr que ça vaille le coup en
             général, mais j'ai pas vu de problème avec le fait d'être aussi permissif
             pour l'instant*)
          if true
             || i =$ 0
             || (not (in_ortho_vowels word #:: (i, -1)))
             || word #: (i - 2, i) = "qu"
          then [ ("u", Core); ("w", Core) ]
          else [ ("u", Core) ]));
  new_fixed "oux$" [ ("u", Core) ];
  List.iter [ "oeu"; "œu" ] ~f:(fun oeu -> new_fixed oeu [ ("9", Core); ("2", Core) ]);
  new_fixed "oell" [ ("wal", Core) ];
  (* moelleux et compagnie *)
  List.iter [ "oe"; "œ" ] ~f:(fun oe ->
      new_fixed oe
        [ ("e", Core (* foetus *)); ("2", Core (* oedeme *)); ("9", Core (* oeil *)) ]);
  new_fixed "oê" [ ("wa", Surprising) ] (* poêle *);

  (* U *)
  new_ "u" (fun word i _ ->
      if i =$ 0 || not (in_ortho_vowels word #:: (i, -1))
      then
        [ ("y", Core)
        ; ("8", Core)
        ; ("w", Surprising (* guatémala *))
        ; ("9", Surprising (* summum *))
        ]
      else [ ("y", Core) ]);
  new_fixed "û" [ ("y", Core) ];
  new_fixed "ù" [ ("y", Core) ];
  new_fixed "ü" [ ("y", Core) ];
  new_fixed "us$" [ ("y", Core (* aigus *)); ("ys", Surprising (* bonus *)) ];
  List.iter [ "um"; "ums$" ] ~f:(fun digraph ->
      new_fixed digraph [ ("om", Surprising); ("Om", Surprising) ]);
  new_fixed "ue" [ ("2", Surprising); ("9", Surprising) ] (* accueil *);

  (* Y *)
  new_fixed "y" [ ("ij", Core); ("j", Core); ("i", Core) ];

  (* Voyelles nasales *)

  (* E à part, car c'est plus compliqué (deux prononciations possibles pour "en") *)
  new_ "en" (fun word _ j ->
      if in_ortho_vowels word #:: (j, 0)
      then
        [ ("2n", Core)
        ; ("°n", Core (* mener *))
        ; ("@n", Surprising (* enivrer *))
        ; ("5n", Surprising (* bieneureux (sans le h) *))
        ]
      else
        [ ("5", Core (* agenda *))
        ; ("@", Core (* mentir *))
        ; ("5n", Surprising (* bienheureux *))
        ]);
  new_fixed "enn"
    [ ("en", Core)
    ; ("En", Core (* européenne *))
    ; ("@n", Core (* ennui *))
    ; ("an", Surprising (* solennel *))
    ];
  new_ "em" (fun word _ j ->
      match word #: (j, j + 1) with
      | "b" | "p" -> [ ("@", Core) ]
      | _ -> [ ("2m", Core); ("°m", Core); ("m", Core) ]);
  new_fixed "emm"
    [ ("@m", Core (* emmène *))
    ; ("am", Core (* évidemment *))
    ; ("em", Core)
    ; ("Em", Core (* flemme *))
    ];
  new_fixed "ent$" [ ("", Surprising); ("@", Core) ];

  new_fixed "ont$" [ ("§", Core) ];
  new_fixed "anc$" [ ("@", Core) ];
  new_fixed "and$" [ ("@", Core) ];
  new_fixed "ant$" [ ("@", Core) ];
  new_fixed "onts$" [ ("§", Core) ];
  new_fixed "ands$" [ ("@", Core) ];
  new_fixed "ants$" [ ("@", Core) ];
  new_fixed "ans$" [ ("@", Core) ];

  (let rule_xm x alone nasal =
     let r_non_nasal = List.map alone ~f:(fun a -> (a ^ "m", Core)) in
     let r_nasal = [ (nasal, Core) ] in
     let r_non_nasal_preferred = r_non_nasal @ [ (nasal, Surprising) ] in
     new_fixed (x ^ "mm") r_non_nasal;
     new_ (x ^ "m") (fun word _ j ->
         match word #: (j, j + 1) with
         | "b" | "p" -> r_nasal
         | _ -> r_non_nasal_preferred)
   in
   rule_xm "a" [ "a" ] "@";
   rule_xm "ai" [ "e"; "E" ] "5";
   rule_xm "i" [ "i" ] "5";
   rule_xm "o" [ "o"; "O" ] "§";
   rule_xm "u" [ "y" ] "1";
   rule_xm "y" [ "i" ] "5");

  (let rule_xn ?(importance = Core) graphem without_n with_n =
     let r_non_nasal = List.map without_n ~f:(fun w -> (w ^ "n", importance)) in
     let r_nasal = [ (with_n, importance) ] in
     new_fixed (graphem ^ "nn") r_non_nasal;
     new_ (graphem ^ "n") (fun word _ j ->
         if in_ortho_vowels word #:: (j, 0) then r_non_nasal else r_nasal)
   in
   rule_xn "a" [ "a" ] "@";
   rule_xn "ai" [ "e"; "E" ] "5";
   rule_xn "ao" [ "a" ] "@" ~importance:Core_optional;
   rule_xn "ei" [ "e"; "E" ] "5";
   rule_xn "i" [ "i" ] "5";
   rule_xn "î" [ "i" ] "5";
   rule_xn "ï" [ "i" ] "5";
   rule_xn "o" [ "o"; "O" ] "§";
   rule_xn "oi" [ "wa" ] "w5";
   rule_xn "u" [ "y" ] "1";
   rule_xn "y" [ "i" ] "5");

  let h = Hashtbl.create (module Char) in
  List.iter !r ~f:(fun (graphem, f) ->
      Hashtbl.add_multi h ~key:graphem.[0] ~data:(graphem, f));
  Hashtbl.map h ~f:(fun l ->
      let a = Array.of_list l in
      Array.stable_sort a ~compare:(fun (g1, _) (g2, _) ->
          Comparable.reverse Int.compare (String.length g1) (String.length g2));
      a)

type path_elt =
  { graphem : string
  ; phonem : string
  ; i : int
  ; j : int
  ; this_surprise : int
  ; importance : importance
  }

let __ p = (p.i, p.j, p.this_surprise, p.importance)

type search_res =
  { path : path_elt list
  ; surprise : int
  }

let to_string { path; surprise } =
  let ortho =
    List.map path ~f:__.graphem
    |> String.concat
    |> String.chop_suffix_if_exists ~suffix:"$"
  in
  let graphemes =
    List.map path ~f:(fun p ->
        if String.( = ) p.phonem "" then "[32m" ^ p.graphem ^ "[39m" else p.graphem)
    |> String.concat ~sep:"|"
  in
  let phonemes = List.map path ~f:__.phonem |> String.concat ~sep:"|" in
  [%string "%{surprise#Int}  %{ortho} %{graphemes}  %{phonemes}\n"]

module Heap = Binary_heap.Make (struct
  type t = int * int * int * path_elt list

  let compare (i1, j1, _, _) (i2, j2, _, _) =
    (* reverse the operands, as Binary_heap is a min-heap but we want a max heap.  We
       compare by decreasing j, so we try paths that match the most of the phonetics,
       which favors graphems where letters are pronounced, instead of being
       mute. Maybe a more principled search would be A* with an order by surprise. *)
    match Int.compare j2 j1 with 0 -> Int.compare i2 i1 | c -> c
end)

let search (rules : t) word phon =
  let first_value = (0, 0, 0, []) in
  let pqueue = Heap.create ~dummy:first_value 5 in
  Heap.add pqueue first_value;
  let word_dollar = word ^ "$" in
  let furthest = ref (0, 0) in
  let rec loop () =
    if Heap.is_empty pqueue
    then
      Error
        [%sexp
          "can't express"
          , (word : string)
          , (phon : string)
          , (String.drop_prefix word (fst !furthest) : string)
          , (String.drop_prefix phon (snd !furthest) : string)]
    else
      let i, j, surprise, path = Heap.pop_minimum pqueue in
      if i >=$ String.length word
      then
        if j =$ String.length phon
        then Ok { path = List.rev path; surprise }
        else loop ()
      else (
        if [%compare: int * int] (j, i) (snd !furthest, fst !furthest) >$ 0
        then furthest := (i, j);
        let longest_matching_core_graphem = ref None in
        Array.iter
          (Hashtbl.find_exn rules word.[i])
          ~f:(fun (graphem, f) ->
            if String.is_substring_at word_dollar ~pos:i ~substring:graphem
            then
              let phonems = f word i (i + String.length graphem) in
              List.iter phonems ~f:(fun (phonem, importance) ->
                  if Option.is_none !longest_matching_core_graphem
                     &&
                     match importance with
                     | Core -> true
                     | Surprising | Core_optional -> false
                     (* should probably count code points instead of String.length *)
                  then longest_matching_core_graphem := Some (String.length graphem);
                  if String.is_substring_at phon ~pos:j ~substring:phonem
                  then
                    let this_surprise =
                      match importance with
                      | Core | Core_optional -> (
                          match !longest_matching_core_graphem with
                          | None -> 0
                          | Some longest ->
                              Bool.to_int (String.length graphem <$ longest))
                      | Surprising -> 1
                    in
                    Heap.add pqueue
                      ( i + String.length graphem
                      , j + String.length phonem
                      , surprise + this_surprise
                      , { graphem; phonem; i; j; this_surprise; importance } :: path )));
        loop ())
  in
  loop ()

let[@ocamlformat "disable"] accent_aigu =
  let syllable_starts =
    lazy
      (Hash_set.of_list
         (module String)
         [ "bR" ; "bl" ; "bw" ; "bj" ; "b8"
         ; "pR" ; "pl" ; "pw" ; "pj" ; "p8"
         ; "tR" ; "tl" ; "tw" ; "tj" ; "t8"
         ; "dR" ; "dl" ; "dw" ; "dj" ; "d8"
         ; "kR" ; "kl" ; "kw" ; "kj" ; "k8"
         ; "gR" ; "gl" ; "gw" ; "gj" ; "g8"
         ; "vR" ; "vl" ; "vw" ; "vj" ; "v8"
         ; "fR" ; "fl" ; "fw" ; "fj" ; "f8"
         ])
  in
  fun right_phon ->
    if Uchar.( = ) right_phon #:: (0, 1) Stdlib.Uchar.rep (* length = 1 *)
    then false (* cette -> è *)
    else if in_phon_vowels right_phon #:: (0, 1)
            ||
            let uc = right_phon #:: (0, 1) in
            (Uchar.( = ) uc !!"j" || Uchar.( = ) uc !!"8" || Uchar.( = ) uc !!"w")
            && in_phon_vowels right_phon #:: (0, 2)
    then Uchar.( <> ) right_phon #:: (0, 1) !!"°" (* verra -> é *)
    else
      let vowels_follow =
        utf8_exists_non_shortcut right_phon ~f:(fun uc ->
            in_phon_vowels uc && Uchar.( <> ) uc !!"°")
      in
      if vowels_follow
      then
        (* é dans des cas comme mettrons, effroi pas sur d'un exemple pour le cas è *)
        Hash_set.mem (force syllable_starts) right_phon #: (0, 2)
      else
        (* si pas de voyelle dans le reste du mot, alors pas de syllabe
           mettre -> è *)
        false
