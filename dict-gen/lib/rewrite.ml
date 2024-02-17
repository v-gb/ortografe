open Core

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

let rewrite ?(start = 0) env (row : Data_src.Lexique.t)
      (ortho, search_res) ~target ~repl =
  let pos = ref start in
  let ortho = ref (ortho, search_res) in
  while !pos <$ String.length (fst !ortho); do
    let ortho1, search_res1 = !ortho in
    let ortho2 = String.Search_pattern.replace_first target ~in_:ortho1 ~with_:repl ~pos:!pos in
    if phys_equal ortho2 ortho1
    then pos := String.length ortho1
    else (
       (match Rules.search env.rules ortho2 row.phon with
        | Ok search_res2 when snd search_res2 <=$ snd search_res1 && env.accept ortho2 ->
           ortho := (ortho2, search_res2)
       | _ -> ());
       pos := !pos + 1
    )
  done;
  !ortho

let keep_if_plausible env (row : Data_src.Lexique.t) (ortho, search_res) ortho2 =
  match Rules.search env.rules ortho2 row.phon with
  | Ok search_res2 when snd search_res2 <=$ snd search_res && env.accept ortho2 ->
     ortho2, search_res2
  | _ -> ortho, search_res

let keep_regardless env (row : Data_src.Lexique.t) ortho1 ortho2 =
  if phys_equal (fst ortho1) ortho2
  then ortho1
  else
    match Rules.search env.rules ortho2 row.phon with
    | Ok search_res2 when env.accept ortho2 -> ortho2, search_res2
    | _ -> ortho1

let keep_regardless_exn rules (row : Data_src.Lexique.t) =
  match Rules.search rules row.ortho row.phon with
  | Error s -> raise_s s
  | Ok v -> v

let _count_code_points str =
  (* would likely be cheaper to count bytes with high bit clear *)
  Uutf.String.fold_utf_8 (fun acc _ -> function
      | `Malformed _ -> assert false
      | `Uchar _ -> acc + 1
    ) 0 str

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
  fun env (row : Data_src.Lexique.t) (ortho, search_res) ->
    let ortho_phon = ref (ortho, search_res, row.phon) in
    let keep_going = ref true in
    while !keep_going; do
      keep_going := false;
      let (ortho, (path, _), phon) = !ortho_phon in
      let rec loop k : Rules.path_elt list -> _ = function
        | ({ graphem = "e"; phonem = ("e" | "E"); _ } as p1)
          :: ({ graphem = ("bb" | "nn" | "mm" | "ll" | "tt" | "pp" | "ff" | "rr" | "cc" | "dd")
              ; phonem
              ; _ } as p2)
          :: rest
             when String.length phonem =$ 1
          ->
           let ortho2, phon2 = rebuild_section ortho phon p1 p2 "" (String.prefix p2.graphem 1) in
           (match Rules.search env.rules ortho2 phon2 with
            | Error _ -> assert false
            | Ok search_res2 ->
               if env.accept ortho2
               then (
                 ortho_phon := (ortho2, search_res2, phon2);
                 keep_going := true)
               else loop (k + 2) rest)
        | ({ graphem = ("enn" | "emm")
           ; phonem = ("en" | "En" | "em" | "Em"); j; _ } as p1) :: rest ->
           let consonant = p1.graphem#:(1,2) in
           let left_phon = String.prefix phon j in
           let right_phon = consonant ^ String.drop_prefix phon (p1.j + String.length p1.phonem) in
           let e_phon, e_ortho = if Rules.accent_aigu right_phon then "e", "é" else "E", "è" in
           let ortho2 =
             String.concat
               [ String.prefix ortho p1.i
               ; e_ortho
               ; consonant
               ; String.drop_prefix ortho (p1.i + String.length p1.graphem)
               ]
           in
           let phon2 = String.concat [ left_phon; e_phon; right_phon ] in
           (match Rules.search env.rules ortho2 phon2 with
            | Error _ -> assert false
            | Ok search_res2 ->
               if env.accept ortho2
               then (
                 ortho_phon := (ortho2, search_res2, phon2);
                 keep_going := true)
               else loop (k + 1) rest)
        | _ :: rest -> loop (k + 1) rest
        | [] -> ()
      in
      loop 0 path
    done;
    !ortho_phon

let rewrite_graphem ?(start = 0) env row ortho ~from:(from_g, from_p) ~to_ =
  if not (String.Search_pattern.matches from_g (fst ortho))
  then ortho
  else (
    let keep_going = ref true in
    let ortho = ref ortho in
    while !keep_going do
      keep_going := false;
      let (ortho1, search_res1) = !ortho in
      let (None | Some ()) =
        List.find_mapi (fst search_res1) ~f:(fun k (path_elt : Rules.path_elt) ->
            if path_elt.graphem = String.Search_pattern.pattern from_g
            && path_elt.phonem = from_p
            && path_elt.i >=$ start
            then
              let ortho2 =
                List.mapi (fst search_res1) ~f:(fun k' elt ->
                    if k =$ k'
                    then to_
                    else elt.graphem)
                |> String.concat
                |> String.chop_suffix_if_exists ~suffix:"$"
              in
              let res = keep_if_plausible env row (ortho1, search_res1) ortho2 in
              if phys_equal (fst res) ortho2
              then (keep_going := true; ortho := res; Some ())
              else None
            else None)
      in
      ()
    done;
    !ortho
  )

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
    "bobbies"; "scrabble"; "sherry";
  ]) in
  fun old_ortho ->
  Hash_set.mem (force set) old_ortho
  || (match String.chop_suffix old_ortho ~suffix:"s" with
      | None -> false
      | Some prefix -> Hash_set.mem (force set) prefix)
  || Re.execp (force re) old_ortho

let load_skip () =
  let skip = Data_src.Lexique.not_usable_words () in
  fun (row : Data_src.Lexique.t) ->
    Hash_set.mem skip row.ortho
    || ((row.lemme ^ "s") = row.ortho && Hash_set.mem skip row.lemme)
    || String.length row.ortho =$ 1
    || String.mem row.ortho '-'

type rule =
  { name : string
  ; doc : string
  ; f : (Rules.t
         -> (Data_src.Lexique.t * Rules.search_res)
         -> Data_src.Lexique.t * Rules.search_res
        ) Lazy.t
  ; prefilter : unit -> [ `Re of Re.t | `All ]
  ; supports_repeated_rewrites : bool
  ; plurals_in_s : bool
  }

let all = ref []
let new_rule ?(supports_repeated_rewrites = true) ?(plurals_in_s = true) name doc ~prefilter f =
  all := { name; doc; f; prefilter; supports_repeated_rewrites; plurals_in_s } :: !all;
  name
let new_rule' ?supports_repeated_rewrites name ?plurals_in_s doc ~prefilter f =
  new_rule name
    ?supports_repeated_rewrites
    ?plurals_in_s
    doc
    ~prefilter
    (lazy (
      let f' = force f in
      fun rules row_and_search_res ->
        f' { rules; accept = const true } row_and_search_res))

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
let erofa_prefilter = lazy (Re.compile (force erofa_prefilter'))

let erofa_rule = lazy (
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
  let pattern_h = String.Search_pattern.create "h" in
  let pattern_eh = String.Search_pattern.create "eh" in
  let pattern_th = String.Search_pattern.create "th" in
  let patterns_ch =
    List.concat_map [ "ec", "é"; "ec", "è"; "c", ""; "", "" ] ~f:(fun (p1, p2) ->
        [ String.Search_pattern.create (p1 ^ "ch"), p2 ^ "c"
        ; String.Search_pattern.create (p1 ^ "ch"), p2 ^ "qu"
        ])
  in
  let patterns_double_consonants =
    List.map [ "b"; "n"; "m"; "l"; "t"; "p"; "f"; "r"; "c"; "d" ] ~f:(fun s ->
        String.Search_pattern.create (s ^ s), s)
  in
  fun rules ((row : Data_src.Lexique.t), search_res) ->
  if not (Re.execp (force erofa_prefilter) row.ortho)
  then (row, search_res)
  else
    let env = { rules; accept = if erofa_preserve row.ortho then erofa_preserve else const true } in
     let ortho = ref (row.ortho, search_res) in
     List.iter patterns_ch ~f:(fun (target, repl) ->
         ortho := rewrite env row !ortho ~target ~repl);
     List.iter patterns_oe ~f:(fun (target, repl) ->
         ortho := rewrite env row !ortho ~target ~repl);
     ortho := rewrite env row !ortho ~target:pattern_mph ~repl:"nf";
     ortho := rewrite env row !ortho ~target:pattern_ph ~repl:"f";
     (match String.chop_suffix (fst !ortho) ~suffix:"x" with
      | Some rest ->
         (* on vérifie que x est bien silencieux, pas remplaceable par s, comme
            dans coccyx *)
         if fst (keep_if_plausible env row !ortho rest) = rest
         then ortho := keep_if_plausible env row !ortho (rest ^ "s");
      | None -> ());
     List.iter [ pattern_auxq, "ausq"; pattern_auxd, "ausd" ]
       ~f:(fun (pattern, with_) ->
         (* les règles ont un cas particulier pour aux mais pas aus, et donc je crois
            que le changement est considéré comme surprenant par [keep_if_plausible]. *)
         ortho := keep_regardless env row !ortho
                    (String.Search_pattern.replace_all pattern ~in_:(fst !ortho) ~with_));
     (match String.chop_prefix (fst !ortho) ~prefix:"deuxi" with
      | None -> ()
      | Some rest -> ortho := keep_if_plausible env row !ortho ("deusi" ^ rest));
     (
       (* exclut des trucs du genre tramway -> tramwai *)
       ortho := rewrite_graphem env row !ortho ~from:(pattern_y, "i") ~to_:"i" ~start:1;
       ortho := rewrite_graphem env row !ortho ~from:(pattern_y, "j") ~to_:"i"
                  ~start:(Bool.to_int (fst !ortho <> "yeus"));
       ortho := rewrite_graphem env row !ortho ~from:(pattern_y, "ij") ~to_:"i" ~start:1;
       ortho := rewrite_graphem env row !ortho ~from:(pattern_yn, "in") ~to_:"in";
       ortho := rewrite_graphem env row !ortho ~from:(pattern_yn, "5") ~to_:"in";
       ortho := rewrite_graphem env row !ortho ~from:(pattern_ym, "im") ~to_:"im";
       ortho := rewrite_graphem env row !ortho ~from:(pattern_ym, "5") ~to_:"im";
     );
     ortho := rewrite env row !ortho ~target:pattern_eh ~repl:"é";
     ortho := rewrite env row !ortho ~target:pattern_eh ~repl:"è";
     ortho := rewrite env row !ortho ~target:pattern_th ~repl:""; (* asthme *)
     let h_start =
       if String.is_prefix (fst !ortho) ~prefix:"déh"
       then String.length "déh" (* déhancher serait déshancher si le h n'était pas aspiré *)
       else Bool.to_int row.h_aspire
     in
     ortho := rewrite env row !ortho ~target:pattern_h ~repl:"" ~start:h_start;
     let row =
       let ortho2, search_res2, phon2 = rewrite_e_double_consonants env row !ortho in
       ortho := (ortho2, search_res2);
       { row with phon = phon2 }
     in
     List.iter patterns_double_consonants ~f:(fun (target, repl) ->
         ortho := rewrite env row !ortho ~target ~repl);
     { row with ortho = fst !ortho }, snd !ortho
)
let _ : string =
  new_rule
    "erofa"
    "Les règles telles que décrites sur erofa.free.fr"
    ~prefilter:(fun () -> `Re (force erofa_prefilter'))
    erofa_rule

let qu__q =
  new_rule'
    "qu--q"
    "question -> qestion mais aquarium inchangé"
    ~prefilter:(fun () -> `Re (Re.str "qu"))
    (lazy (
      let pattern_qu = String.Search_pattern.create "qu" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_qu ~repl:"q";
        { row with ortho = fst !ortho }, snd !ortho))

let qu__qou =
  new_rule'
    "qu--qou"
    "aquatique -> aqouatique"
    ~prefilter:(fun () -> `Re (Re.str "qu"))
    (lazy (
      let pattern_qu = String.Search_pattern.create "qu" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_qu ~repl:"qou";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "ti--ci"
    "nation -> nacion, patient -> pacient, mais question inchangé"
    ~prefilter:(fun () -> `Re (Re.str "ti"))
    (lazy (
      let pattern_ti = String.Search_pattern.create "ti" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_ti ~repl:"ci";
        { row with ortho = fst !ortho }, snd !ortho))

let emment__ament =
  new_rule'
    "emment--ament"
    "évidemment -> évidament"
    ~prefilter:(fun () -> `Re (Re.str "emment"))
    (lazy (
      let pattern_emment = String.Search_pattern.create "emment" in
      let pattern_cemment = String.Search_pattern.create "cemment" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_emment ~repl:"ament";
        ortho := rewrite env row !ortho ~target:pattern_cemment ~repl:"çament";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "oiement--oiment"
    "aboiement -> aboiment"
    ~prefilter:(fun () -> `Re (Re.str "oiement"))
    (lazy (
      let pattern_oiement = String.Search_pattern.create "oiement" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_oiement ~repl:"oiment";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "cq--q"
    "grecque -> grèque"
    ~prefilter:(fun () -> `Re (Re.str "cq"))
    (lazy (
      let pattern_cq = String.Search_pattern.create "cq" in
      let pattern_ecq = String.Search_pattern.create "ecq" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_ecq ~repl:"éq";
        ortho := rewrite env row !ortho ~target:pattern_ecq ~repl:"èq";
        ortho := rewrite env row !ortho ~target:pattern_cq ~repl:"q";
        { row with ortho = fst !ortho }, snd !ortho))

let qua_o__ca_o =
  new_rule'
    "qua-o--ca-o"
    "qualité -> calité, quotient -> cotient"
    ~prefilter:(fun () -> `Re (Re.alt [ Re.str "qua"; Re.str "quo" ]))
    (lazy (
      let pattern_qua = String.Search_pattern.create "qua" in
      let pattern_quo = String.Search_pattern.create "quo" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_qua ~repl:"ca";
        ortho := rewrite env row !ortho ~target:pattern_quo ~repl:"co";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "sc-sch--c-ch"
    "science -> cience, fasciste -> fachiste, schéma -> chéma"
    ~prefilter:(fun () -> `Re (Re.str "sc"))
    (lazy (
      let pattern_esc = String.Search_pattern.create "esc" in
      let pattern_sc = String.Search_pattern.create "sc" in
      let pattern_sch = String.Search_pattern.create "sch" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        (* problème : on autorise descend->decend. À investiguer. Peut-être
           que le découpage se fait d-e-sc-en-d au lieu de d-es-c-en-d et donc
           le e est déjà surprenant. *)
        ortho := rewrite env row !ortho ~target:pattern_esc ~repl:"éc";
        ortho := rewrite env row !ortho ~target:pattern_esc ~repl:"èc";
        ortho := rewrite env row !ortho ~target:pattern_sc ~repl:"c";
        ortho := rewrite env row !ortho ~target:pattern_sc ~repl:"ch";
        ortho := rewrite env row !ortho ~target:pattern_sch ~repl:"ch";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "en--an"
    "enfant -> anfant"
    ~prefilter:(fun () -> `Re (Re.str "en"))
    (lazy (
      let pattern_en = String.Search_pattern.create "en" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_en ~repl:"an";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "gu--gh"
    "guerre -> gherre (et aigüe -> aigue en principe, mais pas fait)"
    ~prefilter:(fun () -> `Re (Re.str "gu"))
    (lazy (
      let pattern_gu = String.Search_pattern.create "gu" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_gu ~repl:"gh";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "g--j"
    "mange -> manje"
    ~prefilter:(fun () -> `Re (Re.str "g"))
    (lazy (
      let pattern_g = String.Search_pattern.create "g" in
      let pattern_gea = String.Search_pattern.create "gea" in
      let pattern_geo = String.Search_pattern.create "geo" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_gea ~repl:"ja";
        ortho := rewrite env row !ortho ~target:pattern_geo ~repl:"jo";
        ortho := rewrite env row !ortho ~target:pattern_g ~repl:"j";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "ez--es"
    "mangez -> mangés"
    ~prefilter:(fun () -> `Re (Re.str "ez"))
    (lazy (
      let pattern_ez = String.Search_pattern.create "ez" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_ez ~repl:"és";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "ent--es"
    "mangent -> manges"
    ~prefilter:(fun () -> `Re (Re.str "ent"))
    (lazy (
      let pattern_ient = String.Search_pattern.create "ient" in
      let pattern_ent = String.Search_pattern.create "ent" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_ient ~repl:"is";
        ortho := rewrite env row !ortho ~target:pattern_ent ~repl:"es";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "m-mbp--n-mbp"
    "compte -> conpte"
    ~prefilter:(fun () -> `Re (Re.seq [ Re.str "m"; Re.set "mbp" ]))
    (lazy (
      let pattern_mp = String.Search_pattern.create "mp" in
      let pattern_mb = String.Search_pattern.create "mb" in
      let pattern_mm = String.Search_pattern.create "mm" in
      fun env (row, search_res) ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_mp ~repl:"np";
        ortho := rewrite env row !ortho ~target:pattern_mb ~repl:"nb";
        ortho := rewrite env row !ortho ~target:pattern_mm ~repl:"nm";
        { row with ortho = fst !ortho }, snd !ortho))

let _ : string =
  new_rule'
    "aux--als"
    "chevaux -> chevals, travaux -> travails"
    ~prefilter:(fun () -> `Re (Re.alt [ Re.str "aux"; Re.set "aus" ]))
    (lazy (
      fun env (row, _ as row_search_res) ->
        if (String.is_suffix row.ortho ~suffix:"aux"
            || String.is_suffix row.ortho ~suffix:"aus")
         && (String.is_suffix row.lemme ~suffix:"al"
             || String.is_suffix row.lemme ~suffix:"ail")
        then
          match String.chop_suffix row.phon ~suffix:"o" with
          | None -> row_search_res
          | Some phon_prefix ->
             let row =
               { row with ortho = row.lemme ^ "s"
                        ; phon = phon_prefix ^ (if String.is_suffix row.lemme ~suffix:"al"
                                                then "al"
                                                else "aj")
               }
             in
             row, keep_regardless_exn env.rules row
        else row_search_res))

let _ : string =
  new_rule'
    "il--y"
     "fille -> fiye, mais ville inchangé"
     ~prefilter:(fun () -> `Re (Re.str "il"))
     (lazy (
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
       fun env (row, search_res) ->
       let ortho = ref (row.ortho, search_res) in
       let new_ortho =
         List.map (fst (snd !ortho)) ~f:(fun p ->
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
              Uutf.String.fold_utf_8 (fun acc _ -> function
                  | `Malformed s -> s :: acc
                  | `Uchar u -> Hashtbl.find_exn graphem_by_phonem u :: acc)
                [] p.phonem
              |> List.rev
              |> String.concat
              |> (fun s -> s ^ suffix1 ^ suffix2)
           | _ -> p.graphem)
         |> String.concat
         |> String.chop_suffix_if_exists ~suffix:"$"
       in
       ortho := keep_if_plausible env row !ortho new_ortho;
       { row with ortho = fst !ortho }, snd !ortho))

let dummy_search_res = [], 0
let _ : string list =
  let pluriel_en_plus = true in
  let e_accente_unique =
    match `A with
    | `A -> "e\u{0304}"
    | `B -> "é"
    | `C -> "ê"
  in
  List.map [ true; false ] ~f:(fun e_unique ->
      new_rule'
        ~supports_repeated_rewrites:false
        ~plurals_in_s:false
        ("ortograf.net"
         ^ if e_unique then "-e-unique" else "")
        ("les règles de http://www.ortograf.net/"
         ^ if e_unique then ", mais sans distinction entre é et è" else "")
        ~prefilter:(fun () -> `All)
        (lazy (
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
               |> List.map ~f:Tuple.T2.swap
               |> Hashtbl.of_alist_multi (module String)
             in
             fun _env (row, search_res) ->
             let graphems =
               try
                 List.concat_map (fst search_res) ~f:(fun p ->
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
                     | _ ->
                        Uutf.String.fold_utf_8 (fun acc _ -> function
                            | `Malformed s -> s :: acc
                            | `Uchar u -> Hashtbl.find_exn graphem_by_phonem u :: acc)
                          [] p.phonem
                        |> List.rev)
                 |> Array.of_list
               with e -> raise_s [%sexp (e : exn), (row.ortho : string), (row.phon : string)]
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
                 if String.is_suffix row.ortho ~suffix:"s"
                    && row.ortho = row.lemme ^ "s"
                 then ortho ^ "+"
                 else
                   (* le lemme de "les" est "les" et pas "le" ? Bizarre. *)
                   match row.ortho with
                   | "ces" | "des" | "les" | "mes" | "ses" | "tes"
                   | "nous" | "vous" | "ils" | "elles" | "aux" -> ortho ^ "+"
                   | _ -> ortho
               else
                 (* On ne peut pas gérer les liaisons avec une réécriture mot à mot comme on
                    fait. Comme les principales (ou seules ?) qui sont obligatoires sont
                    avec les articles, on écrit un z en exposant après les articles, pour
                    indiquer "z optionnel". *)
                 match row.ortho with
                 | "ces" | "des" | "les" | "mes" | "ses" | "tes"
                 | "nous" | "vous" | "ils" | "elles" | "aux" -> ortho ^ "\u{1DBB}"
                 | _ -> ortho
             in
             { row with ortho }, dummy_search_res)))

let _ : string =
  new_rule'
    ~supports_repeated_rewrites:false
    ~plurals_in_s:false
    "alfonic"
    "(pas super testé) les règles de https://alfonic.org/"
    ~prefilter:(fun () -> `All)
    (lazy (
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
       fun _env (row, search_res) ->
         let ortho =
           try
             List.concat_map (fst search_res) ~f:(fun p ->
                 match p.graphem, p.phonem with
                 | "b", "p" -> [ "b" ]
                 | ("i" | "'" | "-" | " "), _ -> [ p.graphem ]
                 | _ ->
                    Uutf.String.fold_utf_8 (fun acc _ -> function
                        | `Malformed s -> s :: acc
                        | `Uchar u -> Hashtbl.find_exn graphem_by_phonem u :: acc)
                      [] p.phonem
                    |> List.rev)
             |> String.concat
           with e -> raise_s [%sexp (e : exn), (row.ortho : string), (row.phon : string)]
         in
         { row with ortho }, dummy_search_res))

let respell_oe ((row : Data_src.Lexique.t), (search_res : Rules.search_res)) =
  (* Lexique contient toujours œ écrit oe. On recolle les lettres, pour qu'on puisse
     réécrire à la fois cœur et coeur, par exemple. *)
  if List.exists (fst search_res) ~f:(fun p ->
         match p.graphem, p.phonem with
         | "oe", ("e" | "E" | "2" | "9")
         | "oeu", ("2" | "9")
         | "coe", "se" -> true
         | _ -> false)
  then
    let search_res =
      List.map (fst search_res) ~f:(fun p ->
          match p.graphem, p.phonem with
          | "oe", ("e" | "E" | "2" | "9") -> { p with graphem = "œ" }
          | "oeu", ("2" | "9") -> { p with graphem = "œu" }
          | "coe", "se" -> { p with graphem = "cœ" }
          | _ -> p),
      snd search_res
    in
    let ortho =
      List.map (fst search_res) ~f:(fun p -> p.graphem)
      |> String.concat
      |> String.chop_suffix_if_exists ~suffix:"$"
    in
    { row with ortho }, search_res
  else row, search_res

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

let gen ?(fix_oe = false) ?(not_understood = `Ignore) ?rules:(which_rules=[]) lexique f =
  Sexp_with_utf8.linkme;
  let rules = Rules.create () in
  let skip = load_skip () in
  let rule, prefilter =
    let which_rules =
      let rank rule =
        if rule.name = emment__ament (* avant qua--ca car on crée des qua *)
        then -3
        else if rule.name = qua_o__ca_o (* avant qu__q sinon les qua ont été tranformés en qa *)
        then -2
        else if rule.name = qu__q || rule.name = qu__qou
        then -1
        else 0
      in
      List.stable_sort which_rules
        ~compare:(fun r1 r2 -> Int.compare (rank r1) (rank r2))
    in
    match which_rules with
    | [] -> force erofa_rule, const true
    | _ :: _ ->
       let rule =
         fun rules row_search_res ->
         List.fold_left which_rules ~init:row_search_res ~f:(fun row_search_res rule ->
             (Lazy.force rule.f) rules row_search_res)
       in
       let prefilter =
         match
           List.map which_rules ~f:(fun r ->
               match r.prefilter () with
               | `All -> raise Exit
               | `Re re -> re)
         with
         | exception Exit -> const true
         | res -> Re.execp (Re.compile (Re.alt (Re.str "oe" :: res)))
       in
       rule, prefilter
  in
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
           or so.  But for smaller changes like cq--q + ti--ci, it goes from 1.25s to
           0.85s. Considering that the fixed cost is 0.58s (i.e. the cost of generation if
           the skip function returns true immediately), that's a fairly substantial
           decrease. *)
        if not (prefilter row.ortho)
        then (prefiltered_out := !prefiltered_out + 1; f row.ortho row.ortho)
        else
          match Rules.search rules row.ortho row.phon with
          | Error s ->
             failed := !failed + 1;
             (match not_understood with
              | `Raise -> raise_s s
              | `Print -> prerr_endline (Sexp_with_utf8.to_string_hum s)
              | `Ignore -> ());
             f row.ortho row.ortho
          | Ok search_res ->
             (* Le oe est une correction du lexique, il s'applique donc à l'orthographe de
                départ, pas l'orthographe d'arrivée comme les calculs de changements
                d'orthographe. *)
             let row_oe, search_res_oe = respell_oe (row, search_res) in
             let row_oe', _ = rule rules (row_oe, search_res_oe) in
             if row.ortho <> row_oe.ortho then (
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
                 else fst (rule rules (row, search_res))
               in
               f row.ortho row'.ortho
             );
             f row_oe.ortho row_oe'.ortho
    ));
  { total = !total; considered = !considered; prefiltered_out = !prefiltered_out; failed = !failed }
