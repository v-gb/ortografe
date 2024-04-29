open Base

let string_search_pattern_replace_first_opt ?pos t ~in_:s ~with_ =
  (* String.Search_pattern.replace_first, modified to return an option so we can tell
     whether something changed or not. Otherwise we need to use phys_equal (which may not
     work well in javascript on strings), or run String.Search_pattern.index twice. *)
  match String.Search_pattern.index ?pos t ~in_:s with
  | None -> None
  | Some i ->
     let len_s = String.length s in
     let len_t = String.length (String.Search_pattern.pattern t) in
     let len_with = String.length with_ in
     let dst = Bytes.create (len_s + len_with - len_t) in
     Bytes.From_string.blit ~src:s ~src_pos:0 ~dst ~dst_pos:0 ~len:i;
     Bytes.From_string.blit ~src:with_ ~src_pos:0 ~dst ~dst_pos:i ~len:len_with;
     Bytes.From_string.blit
       ~src:s
       ~src_pos:(i + len_t)
       ~dst
       ~dst_pos:(i + len_with)
       ~len:(len_s - i - len_t);
     Some (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst, i)
;;

let string_search_pattern_replace_all_opt t ~in_ ~with_ =
  (* String.Search_pattern.replace_all, modified to return an option so we can
     tell whether something changed or not *)
  match String.Search_pattern.index t ~in_ with
  | None -> None
  | Some _ -> Some (String.Search_pattern.replace_all t ~in_ ~with_)

let (>$) = (>)
let (<$) = (<)
let (=$) = (=)
let (<>$) = (<>)
let (<=$) = (<=)
let (>=$) = (>=)
let (>) = String.(>)
let (<) = String.(<)
let (=) = String.(=)
let (<>) = String.(<>)
let (<=) = String.(<=)
let (>=) = String.(>=)
let _ = (>$), (<$), (=$), (<>$), (<=$), (>=$), (>), (<), (=), (<>), (<=), (>=)

let (#:) = Rules.(#:)

type env =
  { rules : Rules.t
  ; accept : string -> bool
  }

type aligned_row = { row : Data.Lexique.row; alignment : Rules.search_res }

let rewrite ?(start = 0) env (aligned_row : aligned_row) ~target ~repl =
  let pos = ref start in
  let aligned_row = ref aligned_row in
  while !pos <$ String.length !aligned_row.row.ortho; do
    let aligned_row1 = !aligned_row in
    let row1 = aligned_row1.row in
    match string_search_pattern_replace_first_opt target ~in_:row1.ortho ~with_:repl ~pos:!pos with
    | None -> pos := String.length row1.ortho
    | Some (ortho2, match_i) ->
       (match Rules.search env.rules ortho2 row1.phon with
        | Ok search_res2 when search_res2.surprise <=$ aligned_row1.alignment.surprise
                              && env.accept ortho2 ->
           aligned_row := { row = { row1 with ortho = ortho2 }; alignment = search_res2 }
        | _ -> ());
       pos := match_i + 1
  done;
  !aligned_row

let keep_if_plausible_phon_opt env (aligned_row : aligned_row) ortho2 phon2 =
  match Rules.search env.rules ortho2 phon2 with
  | Ok search_res2 when search_res2.surprise <=$ aligned_row.alignment.surprise
                        && env.accept ortho2 ->
     Some { row = { aligned_row.row with ortho = ortho2; phon = phon2 }; alignment = search_res2 }
  | _ -> None

let keep_if_plausible_opt env (aligned_row : aligned_row) ortho2 =
  keep_if_plausible_phon_opt env aligned_row ortho2 aligned_row.row.phon

let keep_if_plausible env (aligned_row : aligned_row) ortho2 =
  match Rules.search env.rules ortho2 aligned_row.row.phon with
  | Ok search_res2 when search_res2.surprise <=$ aligned_row.alignment.surprise
                        && env.accept ortho2 ->
     { row = { aligned_row.row with ortho = ortho2 }; alignment = search_res2 }
  | _ -> aligned_row

let keep_regardless env (aligned_row : aligned_row) ortho2 =
  match Rules.search env.rules ortho2 aligned_row.row.phon with
  | Ok search_res2 when env.accept ortho2 ->
     { row = { aligned_row.row with ortho = ortho2 }; alignment = search_res2 }
  | _ -> aligned_row

let keep_regardless_exn rules (row : Data.Lexique.row) =
  match Rules.search rules row.ortho row.phon with
  | Error s -> raise_s s
  | Ok search_res2 -> { row; alignment = search_res2 }

let rewrite_e_double_consonants =
  let rebuild_section ortho phon (p1 : Rules.path_elt) (p2 : Rules.path_elt) left_bit right_bit =
    let left_phon = String.prefix phon p1.j in
    let right_phon = String.drop_prefix phon p2.j in
    let e_phon, e_ortho = if Rules.accent_aigu right_phon then "e", "é" else "E", "è" in
    let ortho' =
      String.concat
        [ String.prefix ortho p1.i
        ; left_bit
        ; e_ortho
        ; right_bit
        ; String.drop_prefix ortho (p2.i + String.length p2.graphem)
        ]
    in
    let phon' = String.concat [ left_phon; e_phon; right_phon ] in
    ortho', phon'
  in
  fun env (aligned_row : aligned_row) ->
    let aligned_row = ref aligned_row in
    let keep_going = ref true in
    while !keep_going; do
      keep_going := false;
      let aligned_row1 = !aligned_row in
      let rec loop k : Rules.path_elt list -> _ = function
        | ({ graphem = "e"; phonem = ("e" | "E"); _ } as p1)
          :: ({ graphem = ("bb" | "nn" | "mm" | "ll" | "tt" | "pp" | "ff" | "rr" | "cc" | "dd")
              ; phonem
              ; _ } as p2)
          :: rest
             when String.length phonem =$ 1
          ->
           let ortho2, phon2 =
             rebuild_section
               aligned_row1.row.ortho aligned_row1.row.phon
               p1 p2 "" (String.prefix p2.graphem 1)
           in
           (match keep_if_plausible_phon_opt env aligned_row1 ortho2 phon2 with
            | Some aligned_row2 -> aligned_row := aligned_row2; keep_going := true
            | None -> loop (k + 2) rest)
        | ({ graphem = ("enn" | "emm")
           ; phonem = ("en" | "En" | "em" | "Em"); j; _ } as p1) :: rest ->
           let consonant = p1.graphem#:(1,2) in
           let left_phon = String.prefix aligned_row1.row.phon j in
           let right_phon = consonant ^ String.drop_prefix aligned_row1.row.phon (p1.j + String.length p1.phonem) in
           let e_phon, e_ortho = if Rules.accent_aigu right_phon then "e", "é" else "E", "è" in
           let ortho2 =
             String.concat
               [ String.prefix aligned_row1.row.ortho p1.i
               ; e_ortho
               ; consonant
               ; String.drop_prefix aligned_row1.row.ortho (p1.i + String.length p1.graphem)
               ]
           in
           let phon2 = String.concat [ left_phon; e_phon; right_phon ] in
           (match keep_if_plausible_phon_opt env aligned_row1 ortho2 phon2 with
            | Some aligned_row2 -> aligned_row := aligned_row2; keep_going := true
            | None -> loop (k + 1) rest)
        | _ :: rest -> loop (k + 1) rest
        | [] -> ()
      in
      loop 0 aligned_row1.alignment.path
    done;
    !aligned_row

let rewrite_graphem' ?(start = 0) env aligned_row ~filter =
  let keep_going = ref true in
  let aligned_row = ref aligned_row in
  while !keep_going do
    keep_going := false;
    let aligned_row1 = !aligned_row in
    let (None | Some ()) =
      List.find_mapi aligned_row1.alignment.path ~f:(fun k (path_elt : Rules.path_elt) ->
          if path_elt.i <$ start
          then None
          else
            match filter path_elt with
            | None -> None
            | Some to_ ->
               let ortho2 =
                 List.mapi aligned_row1.alignment.path ~f:(fun k' elt ->
                     if k =$ k'
                     then to_
                     else elt.graphem)
                 |> String.concat
                 |> String.chop_suffix_if_exists ~suffix:"$"
               in
               match keep_if_plausible_opt env aligned_row1 ortho2 with
               | Some res -> (keep_going := true; aligned_row := res; Some ())
               | None -> None)
    in
    ()
  done;
  !aligned_row

let rewrite_graphem ?start env aligned_row ~from:(from_g, from_p) ~to_ =
  if not (String.Search_pattern.matches from_g aligned_row.row.ortho)
  then aligned_row
  else rewrite_graphem' ?start env aligned_row
         ~filter:(fun path_elt ->
           if path_elt.graphem = String.Search_pattern.pattern from_g
              && path_elt.phonem = from_p
           then Some to_
           else None)

let erofa_preserve =
  let re =
    lazy (
        let open Re in
        let prefix s = seq [ bos; str s ] in
        compile
          (alt
             [ prefix "souhai"
             ; prefix "alleman"
             ; prefix "hollan"
             ; prefix "mourr"
             ; str "chrétien"
             ; str "christ"
             ; str "olymp"
             ; str "hébr"
             ; str "gallo"
             ; str "bohém"
             ; str "bohèm"
             ; str "bohêm"
             ; str "bouddh"
             ; str "buddh"
             ; str "thaï"
             ; str "hispan"
             ; str "hockey"
             ; str "éthiop"
             ; str "hindou"
             ; str "amhar"
             ; str "stakhanov"
             ; str "tyrrh"
             ; str "dionys"
             ; str "tyrol"
             ; str "élysé"
             ; str "abyssin"
             ; str "dreyfus"
             ; str "corinth"
             ; str "hitlér"
             ; str "égypt"
             ; str "lilliput"
             ; str "catherin"
             ; str "stendhal"
             ; str "pyrén"
             ; str "hymala"
             ; str "ghan"
             ; str "méditerran"
             ; str "hongkong"
             ; str "liby"
             ; str "maghréb"
             ; str "babylon"
             ; str "lithuan"
             ; str "vichy"
             ; str "byzan"
             ; str "assyr"
             ; str "carthag"
             ; str "hercul"
             ; str "sahar"
             ; str "luthér"
             ; prefix "phénic"
             ; str "syri" (* syrien *)
             ; str "athén"
             ; prefix "héro"
             ; str "arthur"
             ; str "corréz"
             ; str "hellèn"
             ; str "hellén"
             ; str "helvèt"
             ; str "helvét"
             ; str "hugo"
             ; str "aryen"
             ; str "philipp"
             ; str "kazakh"
             ; str "khmèr"
             ; str "khmer"
             ; str "rugby"
             ; str "shanghai"
             ; str "auxerr"
             ; str "beethovén"
             ; str "beethoven"
             ; str "bruxell"
             ; str "montpelli"
             ; str "hertz"
             ; str "brahman"
             ; str "afghan"
             ; str "ottoman"
             ; str "munich"
             ; str "navarr"
             ; str "hassid"
             ; str "maccarthy"
             ; str "hitchcock"
             ; str "élisabéth"
             ; str "chaldé"
             ; str "hambourg"
             ; str "kénya"
             ; str "thessali"
             ; str "hondur"
             ; str "machiavél"
             ; str "érythré"
             ; str "wallon"
             ; str "bellevill"
             ; str "berrich"
             ; str "cinghal"
             ; str "dahomé"
             ; str "diepp"
             ; str "haïti"
             ; str "helvèt"
             ; str "helvét"
             ; str "himalay"
             ; str "hippocrat"
             ; str "lorrain"
             ; str "mahomét"
             ; str "mallarmé"
             ; str "mulhous"
             ; str "panathén"
             ; str "pythagor"
             ; str "quichott"
             ; str "thatchér"
             ; str "thatchèr"
             ; str "wilhelm"
             ; str "zurich"
             ; str "stéphan"
             ; str "trotsky"
             ; str "brecht"
             ; str "héraclit"
             ; prefix "letton"
             ; prefix "mycén"
             ; prefix "mycèn"
             ; prefix "lycien"
             ; prefix "lycièn"
             ; prefix "tyrien"
             ; prefix "tyrièn"
             ; prefix "palladien"
             ; prefix "palladièn"
             ; str "œdip"
             ; str "oedip"
             ; prefix "phocé"
             ; prefix "hawaï"
             ; prefix "nippo"
             ; str "prométhé"
             ; seq [rep any; str "flux"] (* reflux, influx comme flux *)
             (* pour préserver le t dans wisigoth et quelques autres mots. Je
                ne comprends pas ce que fait le dico érofa d'ailleurs. ostrogoth
                est un nom de peuple, pourquoi le toucher ? *)
             ; seq [any; str "got"; opt (str "h"); opt (str "s"); eos ]
      ]))
  in
  let set = lazy (Hash_set.of_list (module String) [
    "croyiez"; "payiez"; "appuyiez"; "essuyions"; "envoyiez"; "fuyions";
    "ennuyiez"; "ennuyions";

    "sexy"; "jury"; "papy"; "mamy"; "thomas"; "manhattan"; "ecstasy"; "antihéros"; "brandy";
    "off"; "dandy"; "bodhi"; "maharadjah"; "monopoly"; "zloty"; "body"; "sophie"; "crucifix";
    "party"; "fanny"; "gruyère"; "mylord"; "rotary"; "rochelle"; "lloyd"; "pennies"; "carry";
    "mary"; "city"; "hadji"; "yéyé"; "derby"; "till"; "cheikh"; "husky"; "anya";
    "dharma"; "oye"; "hodja"; "mickey"; "bighorn"; "kalachnikov"; "roy"; "charybde"; "henry";
    "boukha"; "grizzly"; "haggadah"; "angleterre"; "bobsleigh"; "lobby"; "bobby"; "hickory";
    "scottish"; "sikh"; "sulky"; "théo"; "sammy"; "thrace"; "cosy"; "tommy"; "byte"; "regency";
    "william"; "mach"; "thrill"; "dolby"; "fifty"; "graff"; "hun"; "panty"; "bacchanal"; "chouya";
    "moly"; "gipsy"; "dey"; "goth"; "machmètre"; "ohm"; "cypriote"; "chypriote"; "finnois"; "fy";
    "harpagon"; "hurrah"; "kabyle"; "lyonnais"; "lyonnaise"; "moghol"; "nay"; "perrier"; "pouilly";
    "royalty"; "abkhaze"; "alhambra"; "antiallemand"; "bachaghas"; "banyuls"; "bertha"; "bithynien";
    "bouzy"; "béhémoth"; "cattleyas"; "chaboisseaux"; "chaix"; "chypre"; "chnord"; "cécidomyies";
    "dandysme"; "galathée"; "ganymèdes"; "gaulle"; "gaullisme"; "gaulliste"; "gengiskhanide";
    "ghât"; "golgotha"; "havanais"; "hittite"; "hosannah"; "jeannot"; "jerseys";
    "khazar"; "landwehr"; "lillois"; "lilloise"; "malherbe"; "marennes"; "margay"; "mathurins";
    "mercurey"; "mithriaque"; "mithridatisé"; "ouighour"; "parthique"; "phynances"; "proudhonisme";
    "préraphaélite"; "puy"; "ptyx"; "pyrrhonien"; "pyrrhonisme"; "raphaélesque"; "rennais";
    "rennaise"; "rhodanien"; "rhodia"; "rhénan"; "rhénane"; "rébecca"; "smyrniote"; "sopha";
    "sylvie"; "syriaque"; "sévillan"; "sévillane"; "taylorisme"; "taylorisé"; "thomisme";
    "thomiste"; "tilbury"; "tokharien"; "transylvain"; "varenne"; "vouvray"; "youyou"; "wallace";
    "chantilly"; "marianne"; "ardennais"; "joseph"; "hégélienne"; "tommies"; "allemagne"; "apollon";
    "margaux"; "crécy"; "cary"; "cayenne"; "antigaulliste"; "ardennaise";
    "chaldaïque"; "garrick"; "hébertisme"; "hégélianisme"; "hégélien"; "narbonnais"; "sarrois";
    "sarroise"; "siennois"; "siennoise"; "sorbonne"; "sorbonnarde"; "ferry"; "rallye"; "haseki";
    "hobby"; "curry"; "paddy"; "caddy"; "guppy"; "hippy"; "cherry"; "wallaby"; "jenny"; "shimmy";
    "bobbies"; "scrabble"; "sherry"; "hermès";
  ]) in
  fun old_ortho ->
  Hash_set.mem (force set) old_ortho
  || (match String.chop_suffix old_ortho ~suffix:"s" with
      | None -> false
      | Some prefix -> Hash_set.mem (force set) prefix)
  || Re.execp (force re) old_ortho

let load_skip () =
  let skip = Data.Lexique.not_usable_words () in
  fun (row : Data.Lexique.row) ->
    Hash_set.mem skip row.ortho
    || ((row.lemme ^ "s") = row.ortho && Hash_set.mem skip row.lemme)
    || String.length row.ortho =$ 1
    || String.mem row.ortho '-'

type f1 =
  { rules : Rules.t
  ; compute : aligned_row -> aligned_row
  }
type f = Rules. t -> f1

type rule =
  { name : string
  ; doc : string
  ; f : f
  ; prefilter : unit -> [ `Re of Re.t | `All ]
  ; supports_repeated_rewrites : bool
  ; plurals_in_s : bool
  }

let all = ref []
let new_rule ?(supports_repeated_rewrites = true) ?(plurals_in_s = true) name doc ~prefilter f =
  let rule = { name; doc; f; prefilter; supports_repeated_rewrites; plurals_in_s } in
  all := rule :: !all;
  rule
let new_rule' ?supports_repeated_rewrites name ?plurals_in_s doc ~prefilter f =
  new_rule name
    ?supports_repeated_rewrites
    ?plurals_in_s
    doc
    ~prefilter
    (fun rules ->
      let f1 = f () in
      { rules
      ; compute =
          fun row_and_search_res ->
          f1 { rules; accept = Fn.const true } row_and_search_res
      })

let erofa_prefilter' = lazy (
  let open Re in
  alt ([ str "œ"
       ; str "oe"
       ; seq [ str "x"; eos ]
       ; str "auxq"
       ; str "auxd"
       ; str "deux"
       ; seq [any; str "y"]
       ; str "h"
       ] @ List.map [ "b"; "n"; "m"; "l"; "t"; "p"; "f"; "r"; "c"; "d" ] ~f:(fun s -> str (s ^ s))
))

let bit_pattern_all_double_consonants = 0b11_1111_1111
let bit_ch = 10
let bit_ph = 11
let bit_h = 12
let bit_x = 13
let bit_oe = 14
let bit_y = 15
let find_relevant_patterns ortho phon =
  (* bit 0..9: double consonne, pour chaque consonne dans l'ordre au dessus
       bit 10: ch
       bit 11: ph
       bit 12: h
       bit 13: x
       bit 14: oe ou œ
       bit 15: y

       On utilise une hypothèse d'indépendence ici : les réécritures érofa
       ne crée pas d'opportunités d'autres réécritures. Avec des mots arbitraires,
       on pourrait avoir arhra, où une fois le h supprimé, une opportunité de
       simplifier une double consonne se présente. Mais on ignore ce genre de
       possibilités. Il serait peut-être possible de recalculer [find_relevant_patterns]
       après un changement, si on voulais éviter cette hypothèse (par exemple,
       si on voulait utiliser ce genre de calcul pour tous les prefilter). *)
  let bits = ref 0 in
  let prev = ref '\000' in
  for i = 0 to String.length ortho - 1 do
    let c = String.unsafe_get ortho i in
    (match c with
     | 'b' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 0)
     | 'n' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 1)
     | 'm' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 2)
     | 'l' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 3)
     | 't' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 4)
     | 'p' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 5)
     | 'f' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 6)
     | 'r' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 7)
     | 'c' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 8)
     | 'd' -> if Char.(=) !prev c then bits := !bits lor (1 lsl 9)
     | 'h' -> bits := !bits
                      lor ((Bool.to_int (Char.(=) !prev 'c')) lsl bit_ch)
                      lor ((Bool.to_int (Char.(=) !prev 'p')) lsl bit_ph)
                      lor (1 lsl bit_h)
     | 'x' -> bits := !bits lor (1 lsl bit_x)
     | 'e' -> if Char.(=) !prev 'o' then bits := !bits lor (1 lsl bit_oe)
     | '\147' -> if Char.(=) !prev '\197' then bits := !bits lor (1 lsl bit_oe)
     | 'y' -> bits := !bits lor (1 lsl bit_y)
     | _ -> ());
    prev := c;
  done;
  let bits = !bits in
  let bits =
    (* Il est couteux de considérer tous les ill prononcé /y/, donc on limite
       les faux positifs en vérifier qu'il y a au moins un /l/ quelque part. *)
    if bits land (1 lsl 3 (* l *)) <>$ 0
       && not (String.mem phon 'l')
    then bits lxor (1 lsl 3)
    else bits
  in
  let bits =
    (* Il est couteux de considérer tous les ch prononcé /S/, donc on limite
       les faux positifs en vérifier qu'il y a au moins un /k/ quelque part. *)
    if bits land (1 lsl bit_ch) <>$ 0
       && not (String.mem phon 'k')
    then bits lxor (1 lsl bit_ch)
    else bits
  in
  bits

let erofa_rule rules =
  let pattern_ph = String.Search_pattern.create "ph" in
  let pattern_mph = String.Search_pattern.create "mph" in
  let patterns_oe =
    List.map ~f:(fun (pattern, repl) ->
        String.Search_pattern.create pattern, repl)
    [ "cœu", "queu"
    ; "coeu", "queu"
    ; "œu", "eu"
    ; "oeu", "eu"
    ; "œ", "eu"
    ; "oe", "eu"
    ; "œ", "é"
    ; "oe", "é"
    ]
  in
  let pattern_auxq = String.Search_pattern.create "auxq" in
  let pattern_auxd = String.Search_pattern.create "auxd" in
  let pattern_y = String.Search_pattern.create "y" in
  let pattern_yn = String.Search_pattern.create "yn" in
  let pattern_ym = String.Search_pattern.create "ym" in
  let pattern_eh = String.Search_pattern.create "eh" in
  let patterns_ch =
    List.concat_map [ "ec", "é"; "ec", "è"; "c", ""; "", "" ] ~f:(fun (p1, p2) ->
        [ String.Search_pattern.create (p1 ^ "ch"), p2 ^ "c"
        ; String.Search_pattern.create (p1 ^ "ch"), p2 ^ "qu"
        ])
  in
  let patterns_double_consonants =
    List.mapi [ "b"; "n"; "m"; "l"; "t"; "p"; "f"; "r"; "c"; "d" ] ~f:(fun bit s ->
        bit, String.Search_pattern.create (s ^ s), s)
  in
  { rules
  ; compute =
      fun aligned_row ->
      let bits = find_relevant_patterns aligned_row.row.ortho aligned_row.row.phon in
      if bits =$ 0
      then aligned_row
      else
        let env =
          { rules; accept =
                     if erofa_preserve aligned_row.row.ortho
                     then erofa_preserve
                     else Fn.const true }
        in
        let aligned_row = ref aligned_row in
        if bits land (1 lsl bit_ch) <>$ 0 then
          List.iter patterns_ch ~f:(fun (target, repl) ->
              aligned_row := rewrite env !aligned_row ~target ~repl);
        if bits land (1 lsl bit_oe) <>$ 0 then
          List.iter patterns_oe ~f:(fun (target, repl) ->
              aligned_row := rewrite env !aligned_row ~target ~repl);
        if bits land (1 lsl bit_ph) <>$ 0 then (
          aligned_row := rewrite env !aligned_row ~target:pattern_mph ~repl:"nf";
          aligned_row := rewrite env !aligned_row ~target:pattern_ph ~repl:"f";
        );
        if bits land (1 lsl bit_x) <>$ 0 then (
          (match String.chop_suffix !aligned_row.row.ortho ~suffix:"x" with
           | Some rest ->
              (* on vérifie que x est bien silencieux, pas remplaceable par s, comme
                 dans coccyx *)
              (match keep_if_plausible_opt env !aligned_row rest with
               | Some _ -> aligned_row := keep_if_plausible env !aligned_row (rest ^ "s")
               | None -> ());
           | None -> ());
          List.iter [ pattern_auxq, "ausq"; pattern_auxd, "ausd" ]
            ~f:(fun (pattern, with_) ->
              (* les règles ont un cas particulier pour aux mais pas aus, et donc je crois
            que le changement est considéré comme surprenant par [keep_if_plausible]. *)
              (match string_search_pattern_replace_all_opt pattern
                       ~in_:!aligned_row.row.ortho ~with_ with
               | None -> ()
               | Some res -> aligned_row := keep_regardless env !aligned_row res));
          (match String.chop_prefix !aligned_row.row.ortho ~prefix:"deuxi" with
           | None -> ()
           | Some rest -> aligned_row := keep_if_plausible env !aligned_row ("deusi" ^ rest))
        );
        if bits land (1 lsl bit_y) <>$ 0 then (
          (* exclut des trucs du genre tramway -> tramwai *)
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_y, "i") ~to_:"i" ~start:1;
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_y, "j") ~to_:"i"
                     ~start:(Bool.to_int (!aligned_row.row.ortho <> "yeus"));
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_y, "ij") ~to_:"i" ~start:1;
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_yn, "in") ~to_:"in";
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_yn, "5") ~to_:"in";
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_ym, "im") ~to_:"im";
          aligned_row := rewrite_graphem env !aligned_row ~from:(pattern_ym, "5") ~to_:"im";
        );
        if bits land (1 lsl bit_h) <>$ 0 then (
          aligned_row := rewrite env !aligned_row ~target:pattern_eh ~repl:"é";
          aligned_row := rewrite env !aligned_row ~target:pattern_eh ~repl:"è";
          aligned_row := rewrite_graphem' env !aligned_row ~filter:(fun path_elt ->
                       (* asthme. Pas sûr qu'il soit nécessaire de matcher les graphèmes ici,
                      plutôt que d'utiliser rewrite. *)
                       match path_elt.graphem, path_elt.phonem with
                       | "th", "" -> Some ""
                       | _ -> None);
          let h_start =
            if String.is_prefix !aligned_row.row.ortho ~prefix:"déh"
            then String.length "déh" (* déhancher serait déshancher si le h n'était pas aspiré *)
            else Bool.to_int !aligned_row.row.h_aspire
          in
          (* On sélectionne les graphèmes directement car la plupart des ch en particulier sont
         prononcés, ce qui crée beaucoup d'erreurs. *)
          aligned_row := rewrite_graphem' ~start:h_start env !aligned_row ~filter:(fun path_elt ->
                       match path_elt.graphem, path_elt.phonem with
                       | "h", "" -> Some ""
                       | "th", ("" | "t") -> Some "t"
                       | "désh", "dez" -> Some "dés"
                       | _ -> None)
        );
        if bits land bit_pattern_all_double_consonants <>$ 0 then (
          aligned_row := rewrite_e_double_consonants env !aligned_row;
          List.iter patterns_double_consonants ~f:(fun (bit, target, repl) ->
              if bits land (1 lsl bit) <>$ 0 then
                aligned_row := rewrite env !aligned_row ~target ~repl);
        );
        !aligned_row
  }

let erofa =
  new_rule
    "erofa"
    "Les règles telles que décrites sur http://erofa.free.fr"
    ~prefilter:(fun () -> `Re (force erofa_prefilter'))
    erofa_rule

let qu__q =
  new_rule'
    "qu/q"
    "@aquatique -> @aquatiqe"
    ~prefilter:(fun () -> `Re (Re.str "qu"))
    (fun () ->
      let pattern_qu = String.Search_pattern.create "qu" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_qu ~repl:"q")

let qu__qou =
  new_rule'
    "qu/qou"
    "@aquatique -> @aqouatique"
    ~prefilter:(fun () -> `Re (Re.str "qu"))
    (fun () ->
      let pattern_qu = String.Search_pattern.create "qu" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_qu ~repl:"qou")

let _ : rule =
  new_rule'
    "ti/ci"
    "@nation -> @nacion, @patient -> @pacient, mais @question inchangé"
    ~prefilter:(fun () -> `Re (Re.str "ti"))
    (fun () ->
      let pattern_ti = String.Search_pattern.create "ti" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_ti ~repl:"ci")

let emment__ament =
  new_rule'
    "emment/ament"
    "@évidemment -> @évidament"
    ~prefilter:(fun () -> `Re (Re.str "emment"))
    (fun () ->
      let pattern_emment = String.Search_pattern.create "emment" in
      let pattern_cemment = String.Search_pattern.create "cemment" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_emment ~repl:"ament";
        aligned_row := rewrite env !aligned_row ~target:pattern_cemment ~repl:"çament";
        !aligned_row)

let _ : rule =
  new_rule'
    "oiement/oiment"
    "@aboiement -> @aboiment"
    ~prefilter:(fun () -> `Re (Re.str "oiement"))
    (fun () ->
      let pattern_oiement = String.Search_pattern.create "oiement" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_oiement ~repl:"oiment")

let _ : rule =
  new_rule'
    "cq/q"
    "@grecque -> @grèque"
    ~prefilter:(fun () -> `Re (Re.str "cq"))
    (fun () ->
      let pattern_cq = String.Search_pattern.create "cq" in
      let pattern_ecq = String.Search_pattern.create "ecq" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_ecq ~repl:"éq";
        aligned_row := rewrite env !aligned_row ~target:pattern_ecq ~repl:"èq";
        aligned_row := rewrite env !aligned_row ~target:pattern_cq ~repl:"q";
        !aligned_row)

let qua_o__ca_o =
  new_rule'
    "qua-o-u/ca-o-u"
    "@qualité -> @calité, @quotient -> @cotient, @piqure -> @picure"
    ~prefilter:(fun () -> `Re (Re.alt [ Re.str "qua"; Re.str "quo"; Re.str "qur"; Re.str "qûr" ]))
    (fun () ->
      let pattern_qua = String.Search_pattern.create "qua" in
      let pattern_quo = String.Search_pattern.create "quo" in
      let pattern_qur1 = String.Search_pattern.create "qur" in
      let pattern_qur2 = String.Search_pattern.create "qûr" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_qua ~repl:"ca";
        aligned_row := rewrite env !aligned_row ~target:pattern_quo ~repl:"co";
        aligned_row := rewrite env !aligned_row ~target:pattern_qur1 ~repl:"cur";
        aligned_row := rewrite env !aligned_row ~target:pattern_qur2 ~repl:"cur";
        !aligned_row)

let que__c =
  new_rule'
    "que/c"
    "@magique -> @magic, mais @publiquement, @communique inchangés"
    ~prefilter:(fun () -> `Re (Re.alt [ Re.str "que" ]))
    (fun () ->
      (* Crée des ambigüités pendant la réécriture : "plaque" ou "fabrique" peuvent
         être réécrits ou pas suivant leurs rôles syntaxiques. *)
      let pattern_que = String.Search_pattern.create "que" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        if not (String.is_suffix !aligned_row.row.lemme ~suffix:"quer") then
          aligned_row := rewrite env !aligned_row ~target:pattern_que ~repl:"c"
                           ~start:(String.length !aligned_row.row.ortho - 4);
        !aligned_row)

let _ : rule =
  new_rule'
    "sc-sch/c-ch"
    "@science -> @cience, @fasciste -> @fachiste, @schéma -> @chéma"
    ~prefilter:(fun () -> `Re (Re.str "sc"))
    (fun () ->
      let pattern_esc = String.Search_pattern.create "esc" in
      let pattern_sc = String.Search_pattern.create "sc" in
      let pattern_sch = String.Search_pattern.create "sch" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        (* problème : on autorise descend->decend. À investiguer. Peut-être
           que le découpage se fait d-e-sc-en-d au lieu de d-es-c-en-d et donc
           le e est déjà surprenant. *)
        aligned_row := rewrite env !aligned_row ~target:pattern_esc ~repl:"éc";
        aligned_row := rewrite env !aligned_row ~target:pattern_esc ~repl:"èc";
        aligned_row := rewrite env !aligned_row ~target:pattern_sc ~repl:"c";
        aligned_row := rewrite env !aligned_row ~target:pattern_sc ~repl:"ch";
        aligned_row := rewrite env !aligned_row ~target:pattern_sch ~repl:"ch";
        !aligned_row)

let _ : rule =
  new_rule'
    "en/an"
    "@enfant -> @anfant"
    ~prefilter:(fun () -> `Re (Re.str "en"))
    (fun () ->
      let pattern_en = String.Search_pattern.create "en" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_en ~repl:"an")

let _ : rule =
  new_rule'
    "um/ome"
    "@forum -> @forome"
    ~prefilter:(fun () -> `Re (Re.str "um"))
    (fun () ->
      let pattern_um = String.Search_pattern.create "um" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_um ~repl:"ome")

let _ : rule =
  new_rule'
    "gu/gh"
    "@guerre -> @gherre (et @aigüe -> @aigue en principe, mais non implémenté)"
    ~prefilter:(fun () -> `Re (Re.str "gu"))
    (fun () ->
      let pattern_gu = String.Search_pattern.create "gu" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_gu ~repl:"gh")

let _ : rule =
  new_rule'
    "g/j"
    "@mange -> @manje"
    ~prefilter:(fun () -> `Re (Re.str "g"))
    (fun () ->
      let pattern_g = String.Search_pattern.create "g" in
      let pattern_gea = String.Search_pattern.create "gea" in
      let pattern_geo = String.Search_pattern.create "geo" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_gea ~repl:"ja";
        aligned_row := rewrite env !aligned_row ~target:pattern_geo ~repl:"jo";
        aligned_row := rewrite env !aligned_row ~target:pattern_g ~repl:"j";
        !aligned_row)

let _ : rule =
  new_rule'
    "gn/ni"
    "@compagnon -> @companion, mais @compagne -> @companye"
    ~prefilter:(fun () -> `Re (Re.str "gn"))
    (fun () ->
      let pattern_gne = String.Search_pattern.create "gne" in
      let pattern_gni = String.Search_pattern.create "gni" in
      let pattern_gn = String.Search_pattern.create "gn" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_gne ~repl:"nye";
        aligned_row := rewrite env !aligned_row ~target:pattern_gni ~repl:"nyi";
        aligned_row := rewrite env !aligned_row ~target:pattern_gn
                         ~repl:(if String.is_suffix !aligned_row.row.lemme ~suffix:"gner"
                                then "ny"
                                else "ni");
        !aligned_row)

let _ : rule =
  new_rule'
    "ez/es"
    "@mangez -> @mangés"
    ~prefilter:(fun () -> `Re (Re.str "ez"))
    (fun () ->
      let pattern_ez = String.Search_pattern.create "ez" in
      fun env aligned_row ->
        rewrite env aligned_row ~target:pattern_ez ~repl:"és")

let _ : rule =
  new_rule'
    "ent/es"
    "@mangent -> @manges"
    ~prefilter:(fun () -> `Re (Re.str "ent"))
    (fun () ->
      let pattern_ient = String.Search_pattern.create "ient" in
      let pattern_ent = String.Search_pattern.create "ent" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_ient ~repl:"is";
        aligned_row := rewrite env !aligned_row ~target:pattern_ent ~repl:"es";
        !aligned_row)

let _ : rule =
  new_rule'
    "m-mbp/n-mbp"
    "@emmène -> @enmène, @nymphe -> @nynphe"
    ~prefilter:(fun () -> `Re (Re.seq [ Re.str "m"; Re.set "mbp" ]))
    (fun () ->
      let pattern_mp = String.Search_pattern.create "mp" in
      let pattern_mb = String.Search_pattern.create "mb" in
      let pattern_mm = String.Search_pattern.create "mm" in
      fun env aligned_row ->
        let aligned_row = ref aligned_row in
        aligned_row := rewrite env !aligned_row ~target:pattern_mp ~repl:"np";
        aligned_row := rewrite env !aligned_row ~target:pattern_mb ~repl:"nb";
        aligned_row := rewrite env !aligned_row ~target:pattern_mm ~repl:"nm";
        !aligned_row)

let _ : rule =
  new_rule'
    "aux/als"
    "@chevaux -> @chevals, @travaux -> @travails"
    ~prefilter:(fun () -> `Re (Re.alt [ Re.str "aux"; Re.set "aus" ]))
    (fun () ->
      fun env aligned_row ->
        if (String.is_suffix aligned_row.row.ortho ~suffix:"aux"
            || String.is_suffix aligned_row.row.ortho ~suffix:"aus")
           && (String.is_suffix aligned_row.row.lemme ~suffix:"al"
               || String.is_suffix aligned_row.row.lemme ~suffix:"ail")
        then
          match String.chop_suffix aligned_row.row.phon ~suffix:"o" with
          | None -> aligned_row
          | Some phon_prefix ->
             let row =
               { aligned_row.row
                 with ortho = aligned_row.row.lemme ^ "s"
                    ; phon = phon_prefix ^ (if String.is_suffix aligned_row.row.lemme ~suffix:"al"
                                            then "al"
                                            else "aj")
               }
             in
             keep_regardless_exn env.rules row
        else aligned_row)

let map_valid_utf_8 str ~f =
  let acc = ref [] in
  let i = ref 0 in
  while !i <$ String.length str; do
    let decode = Stdlib.String.get_utf_8_uchar str !i in
    acc := f (Stdlib.Uchar.utf_decode_uchar decode) :: !acc;
    i := !i + Stdlib.Uchar.utf_decode_length decode;
  done;
  List.rev !acc

let _ : rule =
  new_rule'
    "il/y"
     "@fille -> @fiye, mais @ville inchangé"
     ~prefilter:(fun () -> `Re (Re.str "il"))
     (fun () ->
       let graphem_by_phonem =
         let module Uchar = struct
             include Uchar
             let sexp_of_t t =
               if Stdlib.Uchar.is_char t
               then sexp_of_char (Stdlib.Uchar.to_char t)
               else sexp_of_t t
           end in
         Hashtbl.of_alist_exn (module Uchar)
           [ !!"i", "i"
           ; !!"j", "y"
           ; !!"E", "è"
           ; !!"e", "é"
           ]
       in
       fun env aligned_row ->
         let new_ortho =
           List.map aligned_row.alignment.path ~f:(fun p ->
             match p.graphem with
             | "il$" | "ils$" | "ill" | "illi" | "eil$" | "eils$" | "eill" | "eilli"
                  when not (String.mem p.phonem 'l')
               ->
                let suffix1 =
                  match p.graphem with
                  | "il$" | "ils$"  -> "e"
                  | _ -> ""
                in
                let suffix2 = if String.is_suffix p.graphem ~suffix:"s$" then "s" else "" in
                map_valid_utf_8 p.phonem ~f:(Hashtbl.find_exn graphem_by_phonem)
                |> String.concat
                |> (fun s -> s ^ suffix1 ^ suffix2)
             | _ -> p.graphem)
           |> String.concat
           |> String.chop_suffix_if_exists ~suffix:"$"
         in
         keep_if_plausible env aligned_row new_ortho)

let dummy_search_res : Rules.search_res = { path = []; surprise = 0 }
let _ : rule list =
  let pluriel_en_plus = true in
  let e_accente_unique =
    match `A with
    | `A -> "e\u{0304}"
    | `B -> "é"
    | `C -> "ê"
  in
  List.map [ false; true ] ~f:(fun e_unique ->
      new_rule'
        ~supports_repeated_rewrites:false
        ~plurals_in_s:false
        ("ortograf.net"
         ^ if e_unique then "-e-unique" else "")
        ("les règles de http://www.ortograf.net/"
         ^ if e_unique then ", mais sans distinction entre é et è" else "")
        ~prefilter:(fun () -> `All)
        (fun () ->
             (* problème :
                - trop d'accent grave sur les e en général. On pourrait au moins appliquer
                la même idée que les règles érofa pour améliorer ça.
              *)
             let graphem_by_phonem =
               Hashtbl.of_alist_exn (module Uchar)
                 [ !!"a", "a"
                 ; !!"e", (if e_unique then e_accente_unique else "é")
                 ; !!"E", (if e_unique then e_accente_unique else "è")
                 ; !!"2", "eu"
                 ; !!"9", "eu"
                 ; !!"°", "e"
                 ; !!"@", "an"
                 ; !!"5", "in"
                 ; !!"§", "on"
                 ; !!"1", "un"
                 ; !!"i", "i"
                 ; !!"y", "u"
                 ; !!"8", "u"
                 ; !!"u", "ou"
                 ; !!"o", "o"
                 ; !!"O", "o"
                 ; !!"b", "b"
                 ; !!"S", "ch"
                 ; !!"d", "d"
                 ; !!"f", "f"
                 ; !!"g", "g"
                 ; !!"N", "gn"
                 ; !!"G", "ng"
                 ; !!"Z", "j"
                 ; !!"k", "k"
                 ; !!"l", "l"
                 ; !!"m", "m"
                 ; !!"n", "n"
                 ; !!"p", "p"
                 ; !!"R", "r"
                 ; !!"s", "s"
                 ; !!"t", "t"
                 ; !!"v", "v"
                 ; !!"w", "ou"
                 ; !!"j", "y"
                 ; !!"z", "z"
                 ]
             in
             let reverse_mapping =
               Hashtbl.to_alist graphem_by_phonem
               |> List.map ~f:(fun (a, b) -> (b, a))
               |> Hashtbl.of_alist_multi (module String)
             in
             fun _env aligned_row ->
             let graphems =
               try
                 List.concat_map aligned_row.alignment.path ~f:(fun p ->
                     match p.graphem, p.phonem with
                     | "b", "p" -> [ "b" ]
                     | ("i" | "oi" | "ç" | "'" | "-" | " " | "ss"), _ -> [ p.graphem ]
                     | ("q" | "qu"), "k" -> [ "q" ]
                     | "qu", ("ku" | "kw") -> [ "qou" ]
                     | ("en" | "ent" | "ent$" | "em"), "@" -> [ "en" ]
                     | "en", "@n" -> [ "enn" ]
                     | "enn", "@n" -> [ "en" ]
                     | "sc", "sk" -> [ p.graphem ]
                     | "cc", "k" -> [ "c" ]
                     | "cc", "ks" -> [ "cç" ]
                     | "ch", "k" -> [ "c" ] (* la phase d'après déterminera si un k est nécessaire *)
                     | "c", "k" -> [ "c" ]
                     | "c", "s" -> [ "ç" ]
                     | "x", ("gz" | "ks") -> [ "x" ]
                     | "e", "" -> [ "e" ]
                     | _ -> map_valid_utf_8 p.phonem ~f:(Hashtbl.find_exn graphem_by_phonem)
                   )
                 |> Array.of_list
               with e -> raise_s [%sexp (e : exn), (aligned_row.row : Data.Lexique.row)]
             in
             let ortho =
               let last_uchar str = Rules.(#::) str (String.length str, -1) in
               let first_uchar str = Rules.(#::) str (0, 0) in
               Array.mapi graphems ~f:(fun i g ->
                   if g = "c"
                      && i + 1 <$ Array.length graphems
                      && Rules.in_ortho_weak_vowels (first_uchar graphems.(i + 1))
                   then "k" (* africain deviendrai afrincin sinon *)
                   else if i =$ 0
                   then g
                   else
                     if g = "n"
                        && Rules.in_ortho_vowels (last_uchar graphems.(i-1))
                     then
                       if i + 1 <$ Array.length graphems
                          && Rules.in_ortho_vowels (first_uchar graphems.(i+1))
                       then g
                       else
                         if i + 1 =$ Array.length graphems
                         then g ^ "e"
                         else "·" ^ g
                     else
                       (* prend le grapheme complet d'avant parce que dans «langue», le n
                          est déjà un graphème avec le «a», et donc il ne fait pas de
                          graphème avec le g *)
                       let digraph = graphems.(i-1) ^ Rules.str_of_uchar (first_uchar g) in
                       if Hashtbl.mem reverse_mapping digraph
                       then "·" ^ g
                       else g
                 )
               |> Array.to_list
               |> String.concat
             in
             let ortho =
               if pluriel_en_plus
               then
                 if String.is_suffix aligned_row.row.ortho ~suffix:"s"
                    && aligned_row.row.ortho = aligned_row.row.lemme ^ "s"
                 then ortho ^ "+"
                 else
                   (* le lemme de "les" est "les" et pas "le" ? Bizarre. *)
                   match aligned_row.row.ortho with
                   | "ces" | "des" | "les" | "mes" | "ses" | "tes"
                   | "nous" | "vous" | "ils" | "elles" | "aux" -> ortho ^ "+"
                   | _ -> ortho
               else
                 (* On ne peut pas gérer les liaisons avec une réécriture mot à mot comme on
                    fait. Comme les principales (ou seules ?) qui sont obligatoires sont
                    avec les articles, on écrit un z en exposant après les articles, pour
                    indiquer "z optionnel". *)
                 match aligned_row.row.ortho with
                 | "ces" | "des" | "les" | "mes" | "ses" | "tes"
                 | "nous" | "vous" | "ils" | "elles" | "aux" -> ortho ^ "\u{1DBB}"
                 | _ -> ortho
             in
             { row = { aligned_row.row with ortho }; alignment = dummy_search_res }))

let _ : rule =
  new_rule'
    ~supports_repeated_rewrites:false
    ~plurals_in_s:false
    "alfonic"
    "les règles de https://alfonic.org/"
    ~prefilter:(fun () -> `All)
    (fun () ->
       (* problème :
          - je ne vois de mention des liaisons dans les règles
        *)
      let graphem_by_phonem =
        Hashtbl.of_alist_exn (module Uchar)
          [ !!"a", "a" (* pas de â dans lexique *)
          ; !!"e", "é"
          ; !!"E", "è"
          ; !!"2", "œ\u{302}"
          ; !!"9", "œ"
          ; !!"°", "œ\u{302}"
          ; !!"@", "ä"
          ; !!"5", "ï"
          ; !!"§", "ö"
          ; !!"1", "ü"
          ; !!"i", "i"
          ; !!"y", "u"
          ; !!"8", "u"
          ; !!"u", "w"
          ; !!"o", "ô"
          ; !!"O", "o"
          ; !!"b", "b"
          ; !!"S", "h"
          ; !!"d", "d"
          ; !!"f", "f"
          ; !!"g", "g"
          ; !!"N", "ñ"
          ; !!"G", "¨g"
          ; !!"Z", "j"
          ; !!"k", "c"
          ; !!"l", "l"
          ; !!"m", "m"
          ; !!"n", "n"
          ; !!"p", "p"
          ; !!"R", "r"
          ; !!"s", "s"
          ; !!"t", "t"
          ; !!"v", "v"
          ; !!"w", "w"
          ; !!"j", "y"
          ; !!"z", "z"
          ]
      in
      fun _env aligned_row ->
        let ortho =
          try
            List.concat_map aligned_row.alignment.path ~f:(fun p ->
                match p.graphem, p.phonem with
                | "b", "p" -> [ "b" ]
                | ("i" | "'" | "-" | " "), _ -> [ p.graphem ]
                | _ ->
                   map_valid_utf_8 p.phonem ~f:(Hashtbl.find_exn graphem_by_phonem))
            |> String.concat
          with e -> raise_s [%sexp (e : exn), (aligned_row.row : Data.Lexique.row)]
        in
        { row = { aligned_row.row with ortho }; alignment = dummy_search_res })

let (_ : rule), ou__omega =
  let f w name =
    new_rule'
      ("ou/" ^ name)
      [%string "créer une lettre @ou en utilisant @%{w}, @loup -> @l%{w}p, @ouest -> @%{w}est, @aquatique -> @aq%{w}atique"]
      ~prefilter:(fun () ->
        `Re (Re.alt [ Re.str "ou"; Re.str "où"; Re.str "oû"; Re.str "qu"; Re.str "gu" ]))
      (fun () ->
        ();
        fun _env aligned_row ->
        let path =
          List.map aligned_row.alignment.path
            ~f:(fun path_elt ->
              match path_elt.graphem, path_elt.phonem with
              | ("ou" | "oû"), ("w" | "u") -> { path_elt with graphem = w }
              | "où", ("w" | "u") -> { path_elt with graphem = w ^ "\u{300}" }
              | "qu", "kw" -> { path_elt with graphem = "q" ^ w }
              | "gu", "gw" -> { path_elt with graphem = "g" ^ w }
              | _ -> path_elt)
        in
        let ortho =
          List.map path ~f:(fun p -> p.graphem)
          |> String.concat ~sep:""
          |> String.chop_suffix_if_exists ~suffix:"$"
        in
        { row = { aligned_row.row with ortho }
        ; alignment = { aligned_row.alignment with path }
        }
      )
  in
  let w = f "w" "w" in
  let omega = f "ω" "omega" in
  w, omega
    (* autres symboles dans unicode qui pourrait ressembler à des double-u,
       càd des w arrondi ɯ ш ѡ *)

let _ : rule option =
  if true then None else
  Some (new_rule
    "u-ou-io/y-u-ua"
    "créer une lettre @ou en utilisant @u comme en latin. Pour ce faire, remplace @y par @î, @u par @y, @ou par @u (et @oi par @ua)"
    ~prefilter:(fun () -> `All)
    (fun rules ->
      let pattern_icirc = String.Search_pattern.create "î" in
      let pattern_y = String.Search_pattern.create "y" in
      let accept = Fn.const true in
      { rules
      ; compute =
          fun aligned_row ->
          let aligned_row = ref aligned_row in
          aligned_row := rewrite { rules; accept }
                           !aligned_row ~target:pattern_icirc ~repl:"i";
          let path = !aligned_row.alignment.path in
          let path =
            List.map path
              ~f:(fun path_elt ->
                 match path_elt.graphem, path_elt.phonem with
                 | "ay", ("ej" | "Ej" | "ei" | "Ei") -> { path_elt with graphem = "aiî" }
                 | "oy", ("waj" | "wai") -> { path_elt with graphem = "oiî" }
                 | "ey", ("ej" | "Ej" | "ei" | "Ei") -> { path_elt with graphem = "eiî" }
                 | "y", "i" -> { path_elt with graphem = "i" }
                 | "yn", ("5" | "in") -> { path_elt with graphem = "in" }
                 | "ym", ("5" | "im") -> { path_elt with graphem = "im" }
                 | graphem, _ ->
                    { path_elt
                      with graphem =
                             String.Search_pattern.replace_all pattern_y ~in_:graphem ~with_:"î" }
              )
          in
          let path =
            (* Non seulement il faudrait changer les règles pour pouvoir combiner ce changement
               avec d'autres, mais en plus il faut changer l'ensemble de voyelle faibles pour
               enlever le y. *)
            List.map path
              ~f:(fun path_elt ->
                match path_elt.graphem, path_elt.phonem with
                | "u", ("y" | "8") -> { path_elt with graphem = "y" }
                | "un", ("ym" | "1") -> { path_elt with graphem = "yn" }
                | _ -> path_elt)
          in
          let path =
            let prev = ref "" in
            List.map path
              ~f:(fun path_elt ->
                let res =
                  match path_elt.graphem, path_elt.phonem with
                  | "ou", ("u" | "w") -> { path_elt with graphem = "u" }
                  | "oi", ("wa") -> { path_elt with graphem = "ua" }
                  | _ -> path_elt
                in
                prev := path_elt.graphem;
                res)
          in
          let ortho =
            List.map path ~f:(fun p -> p.graphem)
            |> String.concat ~sep:""
            |> String.chop_suffix_if_exists ~suffix:"$"
          in
          { row = { !aligned_row.row with ortho }; alignment = dummy_search_res }
    }))

let oe_pattern = lazy (String.Search_pattern.create "oe")
let respell_oe (aligned_row : aligned_row) =
  (* Lexique contient toujours œ écrit oe. On recolle les lettres, pour qu'on puisse
     réécrire à la fois cœur et coeur, par exemple. *)
  if List.exists aligned_row.alignment.path ~f:(fun p ->
         match p.graphem, p.phonem with
         | "oe", ("e" | "E" | "2" | "9")
         | "oeu", ("2" | "9")
         | "coe", "se" -> true
         | _ -> false)
  then
    let search_res =
      { aligned_row.alignment
        with path =
               List.map aligned_row.alignment.path ~f:(fun p ->
                   match p.graphem, p.phonem with
                   | "oe", ("e" | "E" | "2" | "9") -> { p with graphem = "œ" }
                   | "oeu", ("2" | "9") -> { p with graphem = "œu" }
                   | "coe", "se" -> { p with graphem = "cœ" }
                   | _ -> p)
      }
    in
    let ortho =
      List.map search_res.path ~f:(fun p -> p.graphem)
      |> String.concat
      |> String.chop_suffix_if_exists ~suffix:"$"
    in
    let lemme =
      (* On met à jour le lemme aussi, sinon œufs devient eu en ortograf.net, au lieu
         de eu+. *)
      if String.(=)
           (String.Search_pattern.replace_all (force oe_pattern)
              ~in_:aligned_row.row.ortho
              ~with_:"œ")
           ortho
      then String.Search_pattern.replace_all (force oe_pattern)
              ~in_:aligned_row.row.lemme
              ~with_:"œ"
      else aligned_row.row.lemme
    in
    { row = { aligned_row.row with ortho; lemme }; alignment = search_res }
  else aligned_row

let doc rule = rule.doc
let name rule = rule.name
let supports_repeated_rewrites rule = rule.supports_repeated_rewrites
let plurals_in_s rule = rule.plurals_in_s
let all = lazy (List.rev !all)

type stats =
  { total : int
  ; considered : int
  ; prefiltered_out : int
  ; failed : int
  }
[@@deriving sexp_of]

let compose_rules rules ~which_rules =
  let which_rules =
    let rank rule =
      if rule.name = emment__ament.name (* avant qua/ca car on crée des qua *)
      then -3
      else if rule.name = qua_o__ca_o.name (* avant qu__q sinon les qua ont été tranformés en qa *)
           || rule.name = que__c.name
      then -2
      else if rule.name = qu__q.name || rule.name = qu__qou.name
      then -1
      else if rule.name = ou__omega.name
      then 1 (* créer des caractéres qui n'existent pas en français *)
      else 0
    in
    List.stable_sort which_rules
      ~compare:(fun r1 r2 -> Int.compare (rank r1) (rank r2))
  in
  match which_rules with
  | [ rule ] when rule.name = erofa.name ->
     (erofa_rule rules).compute,
     (fun word phon -> find_relevant_patterns word phon <>$ 0)
  | _ ->
     let compute =
       match which_rules with
       | [] -> Fn.id
       | _ :: _ ->
          let rev_computes, _ =
            List.fold_left which_rules ~init:([], rules) ~f:(fun (acc, rules) rule ->
                let { rules; compute } = rule.f rules in
                (compute :: acc, rules))
          in
          let computes = List.rev rev_computes in
          fun row_search_res ->
          List.fold_left computes ~init:row_search_res ~f:(fun row_search_res compute ->
              compute row_search_res)
     in
     let prefilter =
       match
         List.map which_rules ~f:(fun r ->
             match r.prefilter () with
             | `All -> raise Stdlib.Exit
             | `Re re -> re)
       with
       | exception Stdlib.Exit -> (fun _ _ -> true)
       | res ->
          let re = Re.compile (Re.alt (Re.str "oe" :: res)) in
          fun word _ -> Re.execp re word
     in
     compute, prefilter

let staged_gen ?(fix_oe = false) ~rules:which_rules () =
  let rules = Rules.create () in
  let skip = load_skip () in
  let rule, prefilter = compose_rules rules ~which_rules in
  fun row ->
    if skip row
    then row.ortho
    else
      if not (prefilter row.ortho row.phon)
      then row.ortho
      else
        match Rules.search rules row.ortho row.phon with
        | Error _ -> row.ortho
        | Ok search_res ->
           let aligned_row =
             if fix_oe
             then respell_oe { row; alignment = search_res }
             else { row; alignment = search_res }
           in
           (rule aligned_row).row.ortho

let gen ?(fix_oe = false) ?(not_understood = `Ignore) ~rules:which_rules lexique f =
  let rules = Rules.create () in
  let skip = load_skip () in
  let rule, prefilter = compose_rules rules ~which_rules in
  let total = ref 0 in
  let considered = ref 0 in
  let prefiltered_out = ref 0 in
  let failed = ref 0 in
  List.iter lexique
    ~f:(fun row ->
      total := !total + 1;
      if not (skip row) then (
        considered := !considered + 1;
        (* The filtering phase makes the creation of dict-arpetani go from 1.95 to 1.725s
           or so.  But for smaller changes like cq/q + ti/ci, it goes from 1.25s to
           0.85s. Considering that the fixed cost is 0.58s (i.e. the cost of generation if
           the skip function returns true immediately), that's a fairly substantial
           decrease. *)
        if not (prefilter row.ortho row.phon)
        then (prefiltered_out := !prefiltered_out + 1; f row.ortho row.ortho)
        else
          match Rules.search rules row.ortho row.phon with
          | Error s ->
             failed := !failed + 1;
             (match not_understood with
              | `Raise -> raise_s s
              | `Call f -> f s
              | `Ignore -> ());
             f row.ortho row.ortho
          | Ok search_res ->
             (* Le oe est une correction du lexique, il s'applique donc à l'orthographe de
                départ, pas l'orthographe d'arrivée comme les calculs de changements
                d'orthographe. *)
             let aligned_row = { row; alignment = search_res } in
             let aligned_row_oe = respell_oe aligned_row in
             let { row = row_oe'; _ } = rule aligned_row_oe in
             if row.ortho <> aligned_row_oe.row.ortho then (
               let row' =
                 if fix_oe (* Quand fix_oe, on réécrit tous les oe qui survive à la transformation
                              en œ, sinon, on s'assure simplement qu'on réécrit les deux formes
                              soient reconnues.
                              Si on réécrit tous les /k/ en lettre «k», et si le lexique contient
                              seulement «coeur» mais pas «cœur», on aura en sortie :
                              - fix_oe=true : cœur -> kœur, coeur -> kœur
                              - fix_oe=false : cœur -> kœur, coeur -> koeur
                            *)
                 then row_oe'
                 else (rule aligned_row).row
               in
               f row.ortho row'.ortho
             );
             f aligned_row_oe.row.ortho row_oe'.ortho
    ));
  { total = !total; considered = !considered; prefiltered_out = !prefiltered_out; failed = !failed }
