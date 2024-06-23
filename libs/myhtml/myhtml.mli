module Markup := Markup_t
type node =
  { classes : (string * string) list
  ; scripts : string list
  ; html : node Markup.node
  }

val nodes_of_string : ?attrs:(string * string) list -> string -> node list

(** Various elements *)
type node_f := ?cl:string -> ?attrs:(string * string) list -> node list -> node

val elt :
  string ->
  node_f
val leafelt : string -> ?cl:string -> (string * string) list -> node
val comment : string -> node
val text : string -> node

val list' :
  [< `ol | `ul ] ->
  ?cl:string ->
  ?attrs:(string * string) list ->
  ((string * string) list * node list) list -> node
val list :
  [< `ol | `ul ] ->
  ?cl:string -> ?attrs:(string * string) list -> node list list -> node
val pseudo_list :
  ?cl:string ->
  ?attrs:(string * string) list ->
  (node * (node -> node list)) list -> node list

val br : node
val hr : ?cl:string -> unit -> node
val a : href:string -> node_f
val div : node_f
val h1 : node_f
val h2 : node_f
val h3 : node_f
val span : node_f
val p : node_f
val strong : node_f
val textarea : node_f
val section : node_f
val img : ?cl:string -> string -> (string * string) list -> node
val code : node_f
val table :
  header:node list list option -> ?cl:string -> node list list list -> node
val id : string -> string * (string * string)
val script : string -> defer:bool -> node
val style : string -> node
val html :
  lang:string ->
  head:node list -> ?body_style:string -> body:node list -> unit -> node
val details :
  ?cl:string ->
  ?summary_cl:string -> ?open_:bool -> node list -> node list -> node
val cite : ?cl:string -> node list -> node

(** Css *)
val flex_cl : ?wrap:bool -> [< `Column | `Row ] -> string
