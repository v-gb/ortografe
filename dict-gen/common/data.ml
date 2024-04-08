open Base

module Csv_header = struct
  type 'a t = (string, int) Hashtbl.t -> (string array -> 'a)
  let field name f =
    fun header ->
      match Hashtbl.find header name with
      | None -> failwith ("no header " ^ name)
      | Some i -> (fun a ->
        try f a.(i)
        with e -> raise_s [%sexp (e : exn), "in line", (a : string array)])
  let return a : _ t = fun _ -> fun _ -> a
  let _ = return
  let both t1 t2 =
    fun header ->
    let f1 = t1 header and f2 = t2 header in
    fun row ->
    f1 row, f2 row
  let map t f =
    fun header ->
    let f' = t header in
    fun row ->
    f (f' row)
  let (let+) = map
  let (and+) = both

  let parse_string t ~header ~separator str =
    (* The csvs we care about have no quoting, and this is lighter than compiler the csv
       library for javascript *)
    let rows = String.split_lines str in
    let header, rows =
      match header with
      | `In_string -> String.split ~on:separator (List.hd_exn rows), List.tl_exn rows
      | `Header h -> h, rows
    in
    let index_by_name =
      List.mapi header ~f:(fun i h -> h, i)
      |> Hashtbl.of_alist_exn (module String)
    in
    let f = t index_by_name in
    List.map rows ~f:(fun row ->
        f (Array.of_list (String.split row ~on:separator)))
end

module Lexique = struct
  type row =
    { ortho : string
    ; phon : string
    ; lemme : string
    ; h_aspire : bool
    }
  [@@deriving sexp_of]
  type t = row list

  let parse src =
    Csv_header.parse_string
      ~header:`In_string
      ~separator:'\t'
      Csv_header.(
      let+ ortho = field "ortho" Fn.id
      and+ phon = field "phon" Fn.id
      and+ lemme = field "lemme" Fn.id
      and+ h_aspire =
        field "h_aspire"
          (function "t" -> true
                  | "f" -> false
                  | s -> failwith ("unknown value of h_aspire: " ^ s))
      in
      { ortho; phon; lemme; h_aspire })
      src

  let not_usable_words () =
    Hash_set.of_list (module String) [
        "monsieur"; "gars"; "ok"; "messieurs"; "mme"; "mlle";
        "cool"; "hum"; "job"; "foot"; "führer"; "bye"; "pizza";
        "business"; "ciao"; "clef"; "yeah";
        "football"; "etc"; "toast"; "team"; "lady"; "baby"; "cm";
        "août"; "look"; "gentleman"; "leader"; "hall"; "dealer";
        "steak"; "deal"; "out"; "jet"; "blues"; "manager";
        "miles"; "jazz"; "coach"; "baseball"; "ranch"; "break";
        "yacht"; "hockey"; "svp"; "gin"; "surf"; "barbecue";
        "surfer"; "cookie"; "scoop"; "yankee"; "overdose";
        "house"; "clean"; "saloon"; "cacahuète"; "lutz"; "bacon";
        "ketchup"; "girl"; "puzzle"; "supporter"; "dealer";
        "jeep"; "hamburger"; "must"; "bunker"; "mm"; "light";
        "poney"; "punch"; "marijuana"; "jean"; "timing";
        "people"; "meeting"; "speed"; "scooter"; "spider";
        "junkie"; "milady"; "please"; "banjo"; "flirter"; "boom";
        "pudding"; "because"; "gentlemen"; "shoote"; "alléluia";
        "cake"; "pancake"; "ace"; "sniper"; "ranger";
        "shampooing"; "macho"; "audition"; "full"; "zoom";
        "jumper"; "mail"; "cheeseburger"; "duce"; "jogging";
        "boomer"; "boomerang"; "mister"; "noël"; "skate"; "bey";
        "sushi"; "masters"; "footballeur"; "pizzeria"; "flirt";
        "piazza"; "shooter"; "speech"; "jack"; "kibboutz";
        "cappuccino"; "reggae"; "country"; "fritz"; "ney";
        "building";"secundo"; "bookmaker"; "dolce"; "volley";
        "book"; "crackers"; "iceberg"; "messer"; "design";
        "putsch"; "flirte"; "challenge"; "drive"; "master";
        "gaspacho"; "chorizo"; "frisbee"; "kleenex"; "pool";
        "deale"; "walkman"; "freezer"; "skateboard"; "bretzels";
        "green"; "fuel"; "muchacho"; "dry"; "groom"; "poncho";
        "starter"; "burgers"; "fjord"; "shooté"; "speaker";
        "challenger"; "containers"; "ladies"; "mile"; "raider";
        "tee"; "underground"; "jingle"; "roller"; "rancho";
        "rocker"; "reichstag"; "businessman"; "looser";
        "pitbull"; "paparazzis"; "yakuza"; "smiley"; "creek";
        "hutter"; "new"; "flirté"; "rumba"; "samurai"; "jerez";
        "mixer"; "paparazzi"; "raiders"; "single"; "hacker";
        "remake"; "rockers"; "emails"; "jaguar"; "stoker";
        "game"; "mozzarella"; "soccer"; "hooligans"; "breakfast";
        "cruzeiros"; "moose"; "striptease"; "yuppies";
        "springer"; "dc"; "zoome"; "state"; "uruguayen";
        "mauser"; "palazzo"; "flirtait"; "igloos"; "thriller";
        "boots"; "igloo"; "lunch"; "muchachos"; "challengers";
        "huerta"; "rollers"; "booster"; "hotline"; "jumbo";
        "muchas"; "aztèque"; "flirtais"; "cheap"; "cinzano";
        "shaker"; "ginseng"; "quaker"; "tanker"; "maharajah";
        "bretzel"; "businessmen"; "pickles"; "boulder";
        "panzers"; "footing"; "quartz"; "razzia"; "shootée";
        "yuppie"; "outsider"; "single"; "cartoon"; "guacamole";
        "squatter"; "kabuki"; "pipeline"; "copyright";
        "pacemaker"; "skinheads"; "clipper"; "dm"; "freelance";
        "cuesta"; "docker"; "chum"; "dealait"; "deales";
        "leadership"; "machiste"; "skinhead"; "sleeping";
        "strudel"; "zoomer"; "bazookas"; "machisme"; "tender";
        "beeper"; "jonkheer"; "beatnik"; "cartoons"; "flood";
        "hackers"; "trader"; "flippers"; "shogun"; "brrr";
        "dealent"; "hanoukka"; "ripper"; "spoon"; "bulldog";
        "chatter"; "chihuahua"; "ph"; "squatters"; "flirtant";
        "pizzicato"; "fashion"; "gentry"; "jihad"; "pirojkis";
        "popaul"; "shiatsu"; "xième"; "zoomes"; "capuccino";
        "ground"; "jaguars"; "jazzman"; "kommandantur";
        "looping"; "loosers"; "putschiste"; "jingles";
        "stripteaseuse"; "zoomez"; "drakkar"; "driver";
        "emergency"; "jogger"; "mufti"; "panzer"; "bush";
        "leasing"; "yucca"; "crooner"; "cueva"; "driver";
        "feelings"; "jodler"; "punt"; "sprinter"; "gazpacho";
        "suerte"; "bucco"; "cherokee"; "ersatz"; "jumping";
        "augment"; "beefsteak"; "cruzeiro"; "kursaal";
        "pitbulls"; "rough"; "speeder"; "sutra"; "trecento";
        "underground"; "bulldogs"; "nunchakus"; "picker";
        "quakers"; "rock'n'roll"; "shootés"; "stripteaseuses";
        "borough"; "carpaccio"; "navajo"; "surbooké"; "cruiser";
        "drummer"; "retriever"; "seltz"; "skinhead"; "talkie";
        "toasteur"; "ber"; "chiricahuas"; "deadline"; "dealera";
        "freak"; "highlanders"; "joggeurs"; "merchandising";
        "outrigger"; "pc"; "pounds"; "shootait"; "speedés";
        "squatter"; "sunlight"; "talkies"; "cucaracha";
        "fahrenheit"; "pacemakers"; "rancher"; "shakespearien";
        "speakerine"; "yakusa"; "bcbg"; "bumper"; "clinker";
        "dogger"; "dôngs"; "event"; "flirtez"; "graben";
        "grooms"; "highlander"; "homuncule"; "jazzy";
        "kilohertz"; "loader"; "lumbago"; "lychee"; "muezzin";
        "peeling"; "revival"; "sampang"; "schuss"; "shootent";
        "speakerines"; "adagio"; "bootleggers"; "boxers";
        "cherokees"; "coolie"; "fung"; "geez"; "ragtime";
        "beatniks"; "chlamydiae"; "coaché"; "dealais";
        "eyeliner"; "mezzanine"; "traveller"; "tungstène";
        "acupuncteur"; "beatnik"; "boosters"; "caballero";
        "caesium"; "crumble"; "dreadlocks"; "fjords"; "jerk";
        "mixers"; "plum"; "reader"; "spleen"; "sprinters";
        "azulejo"; "bootlegger"; "breakdance"; "brooks";
        "chippendales"; "clownesque"; "coacher"; "cryogéniques";
        "dealé"; "feedback"; "flirteur"; "flirteur"; "hooligan";
        "intermezzo"; "loggia"; "mailé"; "outsiders";
        "praesidium"; "puncture"; "quattrocento"; "ranches";
        "razzias"; "seaborgium"; "sertao"; "slow"; "speedes";
        "squeeze"; "umbanda"; "welter"; "blitzkrieg"; "booster";
        "chintz"; "clippers"; "cocoon"; "dna"; "greens";
        "jamboree"; "joggers"; "navajos"; "show"; "strudels";
        "trigger"; "barracudas"; "beagle"; "freaks"; "guru";
        "lsd"; "mailer"; "marker"; "news"; "pipelines"; "shoots";
        "steeple"; "beagles"; "chihuahuas"; "designers";
        "flirtent"; "gigahertz"; "holster"; "impeachment";
        "jazzmen"; "joggeur"; "keiretsu"; "lobbying";
        "marihuana"; "peanuts"; "relooker"; "scrub"; "seppuku";
        "skateboards"; "toasté"; "vroom"; "waterproof";
        "yakuzas"; "yuccas"; "clown"; "dealant"; "drivers";
        "loopings"; "pattern"; "relooké"; "riser"; "shogunat";
        "sloop"; "vauxhall"; "afrikaans"; "afrikaner";
        "background"; "capharnaüm"; "chinook"; "chiricahua";
        "chiricahuas"; "clubhouse"; "fettucine"; "jam";
        "keepsake"; "munster"; "nicaraguayens"; "nikkei";
        "nunchaku"; "overdrive"; "plazza"; "pullman"; "quechua";
        "shakespearienne"; "shootais"; "speakeasy"; "steamer";
        "tipper"; "walkmans"; "zirconium"; "abc"; "adp";
        "brainstorming"; "cheap"; "clearing"; "coaches";
        "coolos"; "cubitainer"; "flirtiez"; "flood";
        "footballeuse"; "footeux"; "freesias"; "graal";
        "guanine"; "halloween"; "jigger"; "kipper"; "kugelhof";
        "lieder"; "mailing"; "navajo"; "ponchos"; "rutherford";
        "schnauzer"; "shootant"; "shootera"; "speedé";
        "steamboat"; "sticker"; "teaser"; "update";
        "winchesters"; "yachting"; "aguardiente"; "breakfasts";
        "caucasique"; "chiricahua"; "cluster"; "computers";
        "coolies"; "crown"; "dealez"; "drifters"; "engineering";
        "glockenspiel"; "joggeuse"; "jungien"; "knickers";
        "kraal"; "mezzo"; "mezzo"; "milonga"; "ml"; "moonistes";
        "mozzarelle"; "naevus"; "navajos"; "paparazzo"; "pound";
        "provider"; "reich"; "relookée"; "roadster"; "roof";
        "schooner"; "shampooiner"; "shooteuse"; "squires";
        "surbookée"; "teenagers"; "tees"; "thrillers";
        "trekking"; "turnover"; "tzar"; "woofers"; "beatniks";
        "bitters"; "brachiosaure"; "broker"; "brook"; "bulge";
        "bungalows"; "canter"; "contrapuntique"; "crooners";
        "cyberspace"; "dealeront"; "drifter"; "flirta";
        "flirtons"; "footballistique"; "gatte"; "gopher";
        "hunter"; "insight"; "jungien"; "kaiser"; "linguale";
        "machistes"; "maharaja"; "maharajahs"; "marae"; "ml";
        "mn"; "mungo"; "piu"; "pizzaiolo"; "putti"; "quakeresse";
        "rhythm'n'blues"; "roentgens"; "réinstaurer"; "röntgens";
        "seersucker"; "shooterai"; "skeet"; "skeets"; "skipper";
        "speakers"; "sprinkler"; "surbookées"; "teenager";
        "training"; "trimmer"; "weltanschauung"; "wildcat";
        "yachtman"; "afrikaans"; "airedale"; "akkadienne";
        "althaea"; "angström"; "appoggiature"; "autographié";
        "autoritairement"; "avunculaire"; "bantu"; "beefsteaks";
        "bloom"; "bloomer"; "bodybuilding"; "boer"; "bufo";
        "bungalow"; "bushi"; "bushido"; "caecum"; "challengeur";
        "chippendale"; "choppers"; "coache"; "countries";
        "creeks"; "dc"; "dealaient"; "drivée"; "dumper";
        "fading"; "feeder"; "flirtaient"; "flirtation";
        "flirteuse"; "flirteuse"; "footballistiques"; "footeuse";
        "freesia"; "guanaco"; "hakka"; "hamadryade"; "hamadryas";
        "hornblende"; "hovercraft"; "jaguarondi"; "jogger";
        "juan"; "jungiens"; "jungiens"; "km"; "knickerbockers";
        "linter"; "lumbagos"; "lychees"; "mezzanines"; "mn";
        "muftis"; "muskogee"; "ounce"; "più"; "pointers";
        "pullmans"; "punctiforme"; "périurbaines"; "reichsmark";
        "relookerai"; "rioja"; "sauri"; "schooners";
        "shakespearien"; "shakespearienne"; "shakespeariens";
        "shakespeariens"; "shooteraient"; "shooteras";
        "shootiez"; "shootons"; "skydome"; "speede"; "speedé";
        "speedée"; "speedée"; "spitz"; "squeezent"; "squeezer";
        "squire"; "steamers"; "steeples"; "steppers";
        "strelitzia"; "stripper"; "taon"; "thesaurus";
        "thésaurus"; "toffee"; "trekkeur"; "unguéal";
        "valpolicella"; "velux"; "winchester"; "wintergreen";
        "woofer"; "yearling"; "zaibatsu"; "adagio"; "aegipans";
        "anchoïade"; "antifading"; "antiskating";
        "appoggiatures"; "aulnaie"; "avunculat"; "ayuntamiento";
        "azulejos"; "bailliage"; "bignonias"; "bintje";
        "boskoop"; "bowling"; "boëttes"; "breitschwanz";
        "browning"; "brownings"; "bushman"; "bécabunga";
        "cancoillotte"; "capucino"; "chippendale";
        "clausewitziens"; "clownerie"; "clowneries";
        "clownesques"; "clowns"; "coaltar"; "columbarium";
        "conjungo"; "conjungos"; "contrapunctique"; "crataegus";
        "crawl"; "crawlait"; "crawlant"; "crawlé"; "cuadrilla";
        "cuadrillas"; "cuevas"; "dieffenbachia"; "down";
        "drakkars"; "drivait"; "drivant"; "drivé"; "dryade";
        "dundee"; "désignions"; "edelweiss"; "faena";
        "fashionable"; "fathom"; "fitzgéraldiennes";
        "flirteront"; "flirteuses"; "flirtèrent"; "foëne";
        "giocoso"; "goals"; "groumer"; "gruppetto";
        "guadeloupéenne"; "guarani"; "hawaiienne"; "hawaiiennes";
        "hiv"; "hollywoodien"; "hollywoodienne";
        "hollywoodiennes"; "hollywoodiens"; "ibm"; "impresarii";
        "input"; "interview"; "interviewaient"; "interviewais";
        "interviewait"; "interviewe"; "interviewer";
        "interviewer"; "interviewers"; "intervieweur";
        "interviews"; "interviewèrent"; "interviewé";
        "interviewé"; "interviewé"; "interviewée"; "interviewés";
        "interviewés"; "intervînt"; "jellaba"; "jinjin";
        "johannisberg"; "kandjar"; "kibboutzim"; "kilbus";
        "kippers"; "kreutzer"; "kriegspiel"; "lambswool";
        "landsturm"; "lardeuss"; "latifundistes"; "lazzi";
        "lazzis"; "leitmotive"; "loggias"; "lunches"; "lur";
        "maelström"; "maghzen"; "maintînt"; "mameluk";
        "mameluks"; "mezzos"; "moonisme"; "muezzins"; "muphti";
        "nuncupatifs"; "obtînt"; "oille"; "outlaw"; "outlaws";
        "pachinko"; "paddocker"; "patchwork"; "paulownias";
        "performer"; "piazzetta"; "pirojki"; "pouzzolane";
        "pronunciamiento"; "pupazzo"; "quipu"; "racingman";
        "razziais"; "razzier"; "reichswehr"; "rewrité"; "reître";
        "reîtres"; "rhumbs"; "rinforzando"; "rumbas"; "sampangs";
        "sandjak"; "sandow"; "sandows"; "sandwich"; "sandwiches";
        "sandwichs"; "scenarii"; "schampooing"; "scénarii";
        "sepuku"; "shantung"; "shipchandler"; "shirting";
        "shoota"; "shootai"; "shootions"; "showbiz"; "shows";
        "shôgun"; "skaal"; "skating"; "skunks"; "sleepings";
        "slows"; "sostenuto"; "speedés"; "spleens"; "sportswear";
        "sprechgesang"; "squaw"; "squaws"; "stayer"; "steward";
        "sunlights"; "sutémi"; "sweater"; "sweaters";
        "sweepstake"; "swinguaient"; "taenia"; "taifas";
        "tangerine"; "tomahawk"; "tomahawks"; "trustee";
        "trustees"; "trusteeship"; "trusteeships"; "tweed";
        "tweeds"; "tzarevitch"; "tzarine"; "tzars"; "vater";
        "weimarienne"; "wurtembergeois"; "wurtembergeois";
        "wurtembergeoise"; "yachtmen"; "yachtwoman"; "épiploon";
        "épiploons"; "beat"; "burger"; "goal"; "feeling";
        "wouah"; "bazooka"; "shoot"; "killer"; "reprogrammer";
        "gauleiter"; "pointer"; "club"; "foil"; "hydrofoil";
        "spoiler"; "gun"; "catgut"; "bégum"; "sagum"; "droico";
        "command"; "establishment"; "claim"; "piedmont"; "veldt";
        "cronstadt"; "borchtch"; "proteus"; "dominion"; "boston";
        "gon"; "canyon"; "cocktail"; "gail"; "hassidim"; "kilim";
        "toutim"; "bim"; "pourim"; "limbo"; "nim"; "junker";
        "brunch"; "brunches"; "junky"; "shunt"; "grunge"; "funky";
        "fun"; "fan"; "van"; "caméraman"; "superman"; "piranha";
        "barman"; "geisha"; "thanksgiving"; "pink"; "yin"; "gringo";
        "in"; "doberman"; "jerrycan"; "playmate"; "schillings";
        "schnaps"; "boy"; "spray"; "polaroid"; "punkette"; "burundais";
        "puncheur"; "westphalien"; "westphalienne"; "charleston";
        "gimmick"; "marimba"; "jerrican"; "gan"; "dan"; "drogman";
        "perchman"; "turcoman"; "khan"; "sprint"; "bingo"; "tom";
        "amen"; "revolver"; "chatterton"; "aureus"; "pesos"; "kumquat";
        "drumlin"; "rumsteck"; "humbug"; "humbug"; "triumvirat";
        "spinnaker"; "round"; "kid"; "gold"; "raid"; "hard"; "cold";
        "lord"; "goulasch"; "bischof"; "fez"; "ave"; "sixties"; "ribes";
        "dumping"; "macumba"; "passim"; "denim"; "verbatim"; "drum";
        "taximan"; "toman"; "gardian"; "shetland"; "land"; "stand";
        "tandoori"; "rodeo"; "maryland"; "portland"; "badminton";
        "forints"; "hindi"; "rink"; "pin's"; "skin"; "cricket";
        "racket"; "rocket"; "basket"; "internet"; "set"; "gadget";
        "soviet"; "kit"; "komsomol"; "sitcom"; "hom"; "stop"; "hop";
        "cap"; "top"; "group"; "bip"; "hip"; "trip"; "pesetas";
        "boghei"; "bortsch"; "groggy"; "haggis"; "tagger";
        "boggie"; "leggins"; "aggiornamento"; "christmas"; "leitmotiv";
        "barmaids"; "peppermint"; "spin"; "bugs"; "flush"; "trust";
        "media"; "sheriff"; "tequila"; "ego"; "veto"; "gretchen";
        "cameraman"; "marketing"; "diesel"; "euskera"; "piercing";
        "favela"; "miserere"; "torero"; "guérilleros"; "shekels";
        "credo"; "peso"; "cicero"; "guérillero"; "travelling"; "pedigree";
        "medium"; "kebab"; "field"; "placebo"; "mesa"; "barmaid"; "suspense";
        "nursery"; "vidéoclub"; "bluffe"; "rush"; "brushing"; "surfe"; "sambo";
        "rambla"; "bluffer"; "surfeurs"; "surfeurs"; "bug"; "puff"; "hurricane";
        "surfeur"; "bluffez"; "putter"; "buggy"; "antitrust"; "rushes"; "bluffé";
        "surfé"; "auburn"; "chutney"; "putt"; "bluffais"; "surfeuse"; "putting";
        "upgradée"; "slug"; "bluffeur"; "puddle"; "tub"; "burg"; "blush"; "surfais";
        "luffa"; "nursing"; "windsurf"; "kelvins"; "kelvin"; "parkinsonien";
        "bluffait"; "bluffait"; "smurf"; "truster"; "slush"; "updater"; "clubman";
        "clubman"; "zingaro"; "minbar"; "shintô"; "flint"; "mackintosh"; "pidgin";
        "pitchpin"; "seringueiros"; "khamsin"; "komintern"; "rinker"; "parkinson";
        "bluffée"; "bluffée"; "nurseries"; "trustais"; "parkinsonienne"; "sprinte";
        "bluffent"; "surfent"; "trustait"; "sprintant"; "bluffons"; "bluffons";
        "truste"; "bluffant"; "trusterait"; "surfez"; "bluffaient"; "macintosh";
        "bluffiez"; "blufferais"; "flamenco"; "larsen"; "polenta"; "yen"; "golden";
        "tempo"; "lego"; "yeti"; "esperanto"; "desperado"; "torera"; "pietà"; "cameramen";
        "homeland"; "blufferez"; "shit"; "groschen"; "kaddisch"; "mound"; "bluffa";
        "bluffeuses"; "djemââ"; "tremolos"; "pistoleros"; "corregidor"; "sombrero";
        "impresario"; "waterman"; "banderillero"; "tempera"; "zen"; "premium"; "dixieland";
        "hobbies"; "stokes"; "daltons"; "backgammon"; "yuan"; "hacienda"; "farniente";
        "haciendas"; "gestapo"; "nietzschéen"; "nietzschéenne"; "yogi"; "jersey";
        "magyar"; "caoua"; "login"; "hodgkinien"; "magyare"; "ouaouaron";
        "gestapiste"; "apartheid"; "reis"; "daiquiri"; "area"; "exeat";
        "confiteor"; "paseo"; "ipse"; "saccharomyces"; "kyrie"; "lied"; "menhaden";
        "shamisen"; "atabeg"; "kierkegaardienne"; "schbeb"; "open"; "maelstrom";
        "retsina"; "tell"; "blockhaus"; "hot"; "menhir"; "requiem"; "whiskey";
        "straight"; "thug"; "bulldozer"; "clergyman"; "handball"; "carry"; "penny";
        "pennies"; "pennys"; "mess"; "bluffes"; "bluffés"; "bluffées"; "westphaliennes";
        "flirtes"; "shootes"; "nietzschéennes"; "magyares"; "surfes"; "drums"; "quintuplés";
        "dominions"; "surfeuses"; "sen"; "basketball"; "callgirl"; "ladys"; "volleyball";
        "callgirls"; "ranchs"; "tennismans"; "cowboy"; "cowboys"; "rugbymans"; "hugh";
        "giorno"; "mamma"; "bluff"; "nurse"; "pub"; "drugstore"; "muffin"; "by"; "pueblo";
        "species";
        (* not real words for this purpose *)
        "min"; "com"; "zzz"; "ah"; "oh"; "eh"; "ha"; "peuh"; "pouh"; "houhou"; "hare";
        "hon"; "haha"; "meuh"; "pouah"; "pfft"; "pff"; "hi"; "ohé"; "ouh"; "ouah"; "ho";
        "bah"; "euh"; "hein"; "hep"; "zzzz";
         (* la phonetique est fausse pour ces mots (ou parfois
            la categories grammaticale) *)
        "égaillent"; "égailler"; "égaillèrent"; "égaillée"; "égaillées"; "égaillés";
        "violent"; "prévalent"; "corpsard";

        (* l'orthographe est ancienne *)
        "cuiller"; "cafeteria"; "cafeteria"; "artefact"; "bonneterie";
        "crematorium"; "acupuncture";
    ]
end

let read_key_value_comma_sep_file str =
  let lines = String.split_lines str in
  let h = Hashtbl.create (module String) ~size:(List.length lines) in
  List.iter lines ~f:(fun line ->
      match String.split line ~on:',' with
      | [key; data] -> ignore (Hashtbl.add h ~key ~data : [ `Ok | `Duplicate ])
      | _ -> raise_s [%sexp "bad line", (line : string)]);
  h

let parse_erofa = read_key_value_comma_sep_file
let parse_post90 = read_key_value_comma_sep_file
