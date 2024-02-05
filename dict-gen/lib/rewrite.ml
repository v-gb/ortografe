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
  match Rules.search env.rules ortho2 row.phon with
  | Ok search_res2 when env.accept ortho2 -> ortho2, search_res2
  | _ -> ortho1

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
          :: ({ graphem = ("nn" | "mm" | "ll" | "tt" | "pp" | "ff" | "rr" | "cc" | "dd")
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
             ; str "phénic"
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
             ; seq [rep any; str "flux"] (* reflux, influx comme flux *)
             (* pour préserver le t dans wisigoth et quelques autres mots. Je
                ne comprends pas ce que fait le dico érofa d'ailleurs. ostrogoth
                est un nom de peuple, pourquoi le toucher ? *)
             ; seq [any; str "got"; opt (str "h"); opt (str "s"); eos ]
      ]))
  in
  let set = lazy (Hash_set.of_list (module String) [
    "sexy"; "jury"; "gay"; "papy"; "thomas"; "manhattan";
    "ecstasy"; "croyiez"; "antihéros"; "brandy"; "off"; "dandy";
    "bodhi"; "maharadjah"; "jockey"; "monopoly"; "zlotys"; "body";
    "sophie"; "crucifix"; "party"; "fanny";
    "gruyère"; "mylord"; "rotary"; "rochelle"; "lloyd";
    "vichy"; "afghan"; "mamy"; "pennies"; "carry"; "mary";
    "city"; "payiez"; "appuyiez"; "essuyions"; "hadji"; "yéyé"; "afghane"; "phi";
    "derby"; "till"; "nippon"; "cheikh"; "husky"; "anya"; "dharma";
    "oye"; "hodja";"mickeys";
    "bighorn"; "ottoman"; "kalachnikovs"; "roy"; "charybde"; "henry";
    "boukha"; "envoyiez"; "fuyions"; "grizzly"; "grizzlys"; "haggadah";
    "angleterre"; "tramway"; "tramways"; "bobsleigh"; "lobby"; "bobby";
    "ennuyiez"; "ennuyions"; "hickory"; "scottish"; "sikh"; "sikhs";
    "sulky"; "théo"; "cockney"; "sammy";
    "thrace"; "cosy"; "tommy"; "munichois"; "munichoise";
    "munichoises"; "navarrais"; "bytes"; "byte"; "hassidique"; "regency";
    "william"; "mach"; "thrill"; "dolby"; "fifty"; "graff"; "hun"; "huns";
    "maccarthysme"; "panty"; "bacchanal"; "chouya"; "moly";
    "gipsy"; "hitchcockien"; "élisabéthaine";
    "chaldéens"; "dey"; "goth"; "hambourgeois";
    "machmètre";"kényan"; "ohms";
    "thessalien"; "tyrienne"; "chaldéen"; "cypriotes"; "finnois";
    "fy"; "harpagon"; "harpagon"; "hertziens"; "hondurien";
    "hurrah"; "kabyle"; "lyonnaise"; "machiavélisme";
    "mahométan"; "moghol"; "nay"; "palladiennes"; "mycénien"; "perrier";
    "play"; "pouilly"; "royalty"; "thug"; "wallon"; "érythréenne";
    "érythréen"; "abkhaze"; "alhambra"; "antiallemands"; "bachaghas"; "banyuls";
    "bellevillois"; "bellevilloise"; "berrichon"; "bertha"; "bithynien";
    "bouzy";
    "béhémoth"; "cattleyas"; "chaboisseaux"; "chaix"; "chypre"; "chnord";
    "cinghalais"; "cinghalaises"; "cypriote"; "cécidomyies";
    "dahoméenne"; "dandysme"; "dieppoise"; "donquichottesque"; "galathée";
    "ganymèdes"; "gaulle"; "gaullisme"; "gaulliste"; "gengiskhanide";
    "ghât"; "golgotha"; "hambourgeoises"; "havanais"; "hawaïen"; "haïtienne";
    "helléniser"; "hellénisme"; "hèlénisme"; "helvète"; "helvétique"; "himalayens";
    "hippocratique"; "hittite"; "hosannah"; "jeannot"; "jerseys";
    "khazar"; "landwehr"; "lillois"; "lilloise"; "lorrain"; "lorraine";
    "lycien"; "lyciennes"; "lyonnais"; "mahométane"; "malherbe";
    "mallarméen"; "marennes"; "margay"; "mathurins"; "mercurey"; "mithriaque";
    "mithridatisé"; "mulhousien"; "munich"; "mycéniennes"; "ouighour"; 
    "panathénaïque"; "panathénées"; "parthique"; "phynances"; "proudhonisme";
    "préraphaélite"; "puy"; "ptyx"; "pyrrhonien"; "pythagoricien"; "quichotte";
    "pyrrhonisme"; "railway"; "raphaélesque"; "rennais"; "rennaises"; 
    "rhodanien"; "rhodia"; "rhénan"; "rhénane"; "rébecca"; "smyrniote";
    "sopha"; "sylvie"; "syriaque"; "sévillan"; "sévillane"; "taylorisme";
    "taylorisé"; "thatchériennes"; "thomisme"; "thomiste"; "tilbury";
    "tokay"; "tokharien"; "transylvain"; "varenne"; "vouvray"; "wilhelmien";
    "youyou"; "zurichois"; "zurichoise"; "wallace"; "chantilly"; "machiavélique";
    "berrichonne"; "marianne"; "stéphanois"; "stéphanoise"; "ardennais"; "joseph";
    "hégélienne"; "tommies"; "allemagne"; "apollon"; "letton"; "margaux"; "lettone";
    "nippo"; "crécy"; "élisabéthain"; "arthurien"; "cary"; "cayenne"; "chypriote";
    "nippone"; "ottomane"; "pythagoriques"; "trotskystes"; "antigaullistes";
    "ardennaise"; "brechtienne"; "chaldaïques"; "garrick"; "hébertisme";
    "hégélianisme"; "hégélien"; "héraclitienne"; "héraclitéen"; "héraclitéenne";
    "lettonne"; "narbonnais"; "ottomane"; "sarrois"; "sarroise"; "siennois";
    "siennoise"; "sorbonne"; "sorbonnarde"; "ferry"; "rallye"; "haseki";
  ]) in
  fun old_ortho ->
  Hash_set.mem (force set) old_ortho
  || (match String.chop_suffix old_ortho ~suffix:"s" with
      | None -> false
      | Some prefix -> Hash_set.mem (force set) prefix)
  || Re.execp (force re) old_ortho

let load_skip () =
  let skip = Data_src.Lexique.exceptions () in
  fun (row : Data_src.Lexique.t) ->
    Hash_set.mem skip row.ortho
    || (match String.chop_suffix row.ortho ~suffix:"s" with
        | None -> false
        | Some rest -> Hash_set.mem skip rest)
    || String.length row.ortho =$ 1
    || row.cgram = "ONO"
    || String.mem row.ortho '-'

let erofa_rule1 = lazy (
  let pattern_ph = String.Search_pattern.create "ph" in
  let pattern_mph = String.Search_pattern.create "mph" in
  let pattern_coeu = String.Search_pattern.create "coeu" in
  let pattern_oeu = String.Search_pattern.create "oeu" in
  let pattern_oe = String.Search_pattern.create "oe" in
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
    List.map [ "n"; "m"; "l"; "t"; "p"; "f"; "r"; "c"; "d" ] ~f:(fun s ->
        String.Search_pattern.create (s ^ s), s)
  in
  fun env (lazy wiki) (row : Data_src.Lexique.t) ->
  let h_aspire =
    String.is_prefix row.ortho ~prefix:"h"
    && (String.is_prefix row.ortho ~prefix:"haï"
        || String.is_prefix row.ortho ~prefix:"hai"
        || match Hashtbl.find wiki row.lemme with
           | None -> Option.value (Hashtbl.find wiki row.ortho) ~default:true
           | Some h_aspire -> h_aspire)
  in
  match Rules.search env.rules row.ortho row.phon with
  | Error _ as e -> e
  | Ok search_res ->
     let ortho = ref (row.ortho, search_res) in
     List.iter patterns_ch ~f:(fun (target, repl) ->
         ortho := rewrite env row !ortho ~target ~repl);
     ortho := rewrite env row !ortho ~target:pattern_coeu ~repl:"queu";
     ortho := rewrite env row !ortho ~target:pattern_oeu ~repl:"eu";
     ortho := rewrite env row !ortho ~target:pattern_oe ~repl:"eu";
     ortho := rewrite env row !ortho ~target:pattern_oe ~repl:"é";
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
       else Bool.to_int h_aspire
     in
     ortho := rewrite env row !ortho ~target:pattern_h ~repl:"" ~start:h_start;
     let row =
       let ortho2, search_res2, phon2 = rewrite_e_double_consonants env row !ortho in
       ortho := (ortho2, search_res2);
       { row with phon = phon2 }
     in
     List.iter patterns_double_consonants ~f:(fun (target, repl) ->
         ortho := rewrite env row !ortho ~target ~repl);
     Ok { row with ortho = fst !ortho }
)

type rule =
  { name : string
  ; doc : string
  ; f : (Rules.t ->
         (string, bool) Base.Hashtbl.t Lazy.t ->
         Data_src.Lexique.t
         -> (Data_src.Lexique.t, Sexp.t) result) Lazy.t
  }

let all = ref []
let new_rule name doc f =
  all := { name; doc; f  } :: !all;
  name
let new_rule' name doc f =
  new_rule name doc
    (lazy (
      let f' = force f in
      fun rules _wiki (row : Data_src.Lexique.t) ->
        let env = { rules; accept = const true } in
        match Rules.search env.rules row.ortho row.phon with
        | Error _ as e -> e
        | Ok search_res -> Ok (f' env row search_res)))

let erofa_rule = lazy (
  let erofa_rule1 = force erofa_rule1 in
  fun rules wiki (row : Data_src.Lexique.t) ->
  match erofa_rule1 { rules; accept = const true } wiki row with
  | Error _ as e -> e
  | Ok row' ->
     (* Le fait de ne passer le vrai accept que si le mot est réécrit est censé être
          une optimisation, mais je n'ai pas vérifié si ça aide ou pas. *)
     if row.ortho = row'.ortho
     then Ok row (* doesn't really matter which one we choose *)
     else
       if erofa_preserve row.ortho
       then (match erofa_rule1 { rules; accept = erofa_preserve } wiki row with
             | Error s -> raise_s s
             | Ok row'' -> Ok row'')
       else Ok row'
)
let _ : string = new_rule "erofa" "Les règles telles que décrites sur erofa.free.fr" erofa_rule

let qu__q =
  new_rule'
    "qu--q"
    "question -> qestion mais aquarium inchangé"
    (lazy (
      let pattern_qu = String.Search_pattern.create "qu" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_qu ~repl:"q";
        { row with ortho = fst !ortho }))

let qu__qou =
  new_rule'
    "qu--qou"
    "aquatique -> aqouatique"
    (lazy (
      let pattern_qu = String.Search_pattern.create "qu" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_qu ~repl:"qou";
        { row with ortho = fst !ortho }))

let _ : string =
  new_rule'
    "ti--ci"
    "nation -> nacion, patient -> pacient, mais question inchangé"
    (lazy (
      let pattern_ti = String.Search_pattern.create "ti" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_ti ~repl:"ci";
        { row with ortho = fst !ortho }))

let emment__ament =
  new_rule'
    "emment--ament"
    "évidemment -> évidament"
    (lazy (
      let pattern_emment = String.Search_pattern.create "emment" in
      let pattern_cemment = String.Search_pattern.create "cemment" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_emment ~repl:"ament";
        ortho := rewrite env row !ortho ~target:pattern_cemment ~repl:"çament";
        { row with ortho = fst !ortho }))

let _ : string =
  new_rule'
    "oiement--oiment"
    "aboiement -> aboiment"
    (lazy (
      let pattern_oiement = String.Search_pattern.create "oiement" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_oiement ~repl:"oiment";
        { row with ortho = fst !ortho }))

let _ : string =
  new_rule'
    "cq--q"
    "grecque -> grèque"
    (lazy (
      let pattern_cq = String.Search_pattern.create "cq" in
      let pattern_ecq = String.Search_pattern.create "ecq" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_ecq ~repl:"éq";
        ortho := rewrite env row !ortho ~target:pattern_ecq ~repl:"èq";
        ortho := rewrite env row !ortho ~target:pattern_cq ~repl:"q";
        { row with ortho = fst !ortho }))

let qua_o__ca_o =
  new_rule'
    "qua-o--ca-o"
    "qualité -> calité, quotient -> cotient"
    (lazy (
      let pattern_qua = String.Search_pattern.create "qua" in
      let pattern_quo = String.Search_pattern.create "quo" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        ortho := rewrite env row !ortho ~target:pattern_qua ~repl:"ca";
        ortho := rewrite env row !ortho ~target:pattern_quo ~repl:"co";
        { row with ortho = fst !ortho }))

let _ : string =
  new_rule'
    "sc-sch--c-ch"
    "science -> cience, fasciste -> fachiste, schéma -> chéma"
    (lazy (
      let pattern_esc = String.Search_pattern.create "esc" in
      let pattern_sc = String.Search_pattern.create "sc" in
      let pattern_sch = String.Search_pattern.create "sch" in
      fun env row search_res ->
        let ortho = ref (row.ortho, search_res) in
        (* problème : on autorise descend->decend. À investiguer. Peut-être
           que le découpage se fait d-e-sc-en-d au lieu de d-es-c-en-d et donc
           le e est déjà surprenant. *)
        ortho := rewrite env row !ortho ~target:pattern_esc ~repl:"éc";
        ortho := rewrite env row !ortho ~target:pattern_esc ~repl:"èc";
        ortho := rewrite env row !ortho ~target:pattern_sc ~repl:"c";
        ortho := rewrite env row !ortho ~target:pattern_sc ~repl:"ch";
        ortho := rewrite env row !ortho ~target:pattern_sch ~repl:"ch";
        { row with ortho = fst !ortho }))

let _ : string =
  new_rule'
    "aux--als"
    "chevaux -> chevals, travaux -> travails"
    (lazy (
      fun _env row _search_res ->
        if (String.is_suffix row.ortho ~suffix:"aux"
            || String.is_suffix row.ortho ~suffix:"aus")
         && (String.is_suffix row.lemme ~suffix:"al"
             || String.is_suffix row.lemme ~suffix:"ail")
        then
          match String.chop_suffix row.phon ~suffix:"o" with
          | None -> row
          | Some phon_prefix ->
             { row with ortho = row.lemme ^ "s"
                      ; phon = phon_prefix ^ (if String.is_suffix row.lemme ~suffix:"al"
                                              then "al"
                                              else "aj")
             }
        else row))

let _ : string =
  new_rule'
    "il--y"
     "(pas encore prêt) fille -> fiye, mais ville inchangé"
     (lazy (
       let pattern_illi = String.Search_pattern.create "illi" in
       let pattern_ill = String.Search_pattern.create "ill" in
       let pattern_il = String.Search_pattern.create "il" in
       fun env row search_res ->
         let ortho = ref (row.ortho, search_res) in
         ortho := rewrite env row !ortho ~target:pattern_illi ~repl:"iy";
         ortho := rewrite env row !ortho ~target:pattern_ill ~repl:"iy";
         ortho := rewrite env row !ortho ~target:pattern_il ~repl:"iy";
         ortho := rewrite env row !ortho ~target:pattern_illi ~repl:"y";
         ortho := rewrite env row !ortho ~target:pattern_ill ~repl:"y";
         ortho := rewrite env row !ortho ~target:pattern_il ~repl:"y";
         { row with ortho = fst !ortho }))

let doc rule = rule.doc
let name rule = rule.name
let all = lazy (List.rev !all)

let gen ~root ?(skip_not_understood = false) ?lexique ?rules:(which_rules=[]) f =
  Sexp_with_utf8.linkme;
  let rules = Rules.create () in
  let skip = load_skip () in
  let wiki = lazy (
    let l = Data_src.Wiki.load ~root in
    Hashtbl.of_alist_exn (module String)
      (List.map l ~f:(fun r -> r.word, r.h_aspire)))
  in
  let rule =
    let which_rules =
      let rank rule =
        if rule.name = emment__ament (* avant quo--co car on crée des quo *)
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
    | [] -> force erofa_rule
    | _ :: _ ->
       fun rules wiki row ->
       Ok (
         List.fold_left which_rules ~init:row ~f:(fun row rule ->
             match (Lazy.force rule.f) rules wiki row with
             | Error s ->
                if not skip_not_understood
                then raise_s s
                else row
             | Ok row' -> row'))
  in
  List.iter (lexique ||? Data_src.Lexique.load ~root ())
    ~f:(fun row ->
      if not (skip row) then (
        match rule rules wiki row with
        | Error s ->
           if not skip_not_understood
           then raise_s s
        | Ok row' -> f row.ortho row'.ortho
    ))
