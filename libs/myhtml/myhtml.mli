module Markup := Markup_t

module Css : sig
  type t
  type length

  val to_string : t list -> string
  val zero : length
  val em : float -> length
  val rem : float -> length
  val px : float -> length
  val percent : float -> length
  val size : length -> string
  val display : [< `block | `inline | `inline_block | `none ] -> t

  val prop_lrtb :
       string
    -> ?all:length
    -> ?lr:length
    -> ?tb:length
    -> ?l:length
    -> ?r:length
    -> ?t:length
    -> ?b:length
    -> unit
    -> t list

  val margin :
       ?all:length
    -> ?lr:length
    -> ?tb:length
    -> ?l:length
    -> ?r:length
    -> ?t:length
    -> ?b:length
    -> unit
    -> t list

  val margin_auto : t
  val width' : string -> t
  val width : length -> t
  val width_auto : t
  val width_fit_content : t
  val height' : string -> t
  val height : length -> t
  val height_auto : t

  val padding :
       ?all:length
    -> ?lr:length
    -> ?tb:length
    -> ?l:length
    -> ?r:length
    -> ?t:length
    -> ?b:length
    -> unit
    -> t list

  val font_family : string list -> t
  val pointer_events : [< `none ] -> t
  val color : string -> t
  val text_decoration : string -> t
  val text_decoration_line : [< `underline ] -> t
  val background_color : string -> t
  val max_width' : string -> t
  val max_width : length -> t
  val min_width' : string -> t
  val min_width : length -> t
  val min_height' : string -> t
  val min_height : length -> t
  val list_style_type : [< `None ] -> t
  val font_size' : string -> t
  val font_size : length -> t
  val font_weight : [< `Bold ] -> t

  val border :
       ?width:length
    -> ?style:[< `solid ]
    -> ?color:string
    -> ?radius:length
    -> unit
    -> t list

  val whitespace : [< `pre_wrap ] -> t

  val position :
       ?all:length
    -> ?lr:length
    -> ?tb:length
    -> ?l:length
    -> ?r:length
    -> ?t:length
    -> ?b:length
    -> [< `absolute | `relative ]
    -> t list

  val aspect_ratio : int -> int -> t
  val float : [< `Left | `Right ] -> t
  val hyphens : [< `auto ] -> t
  val flex_child : ?grow:int -> ?shrink:int -> ?basis:length -> unit -> t list

  val flex :
       ?wrap:[< `Wrap ]
    -> ?gap:length
    -> ?grow:int
    -> ?shrink:int
    -> ?basis:length
    -> ?row_gap:length
    -> ?column_gap:length
    -> [< `Column | `Row ]
    -> t list

  val justify_content : [< `center ] -> t
  val text_align : [< `center ] -> t
  val clear : [< `both | `left | `right ] -> t
  val selector : string -> t list -> t list
  val raw1 : string -> t
  val raw : string -> t list
end

type node =
  { classes : (string * string) list
  ; scripts : string list
  ; html : node Markup.node
  }

val nodes_of_string : ?attrs:(string * string) list -> string -> node list

type node_f := ?cl:string -> ?attrs:(string * string) list -> node list -> node
(** Various elements *)

val elt : string -> node_f
val leafelt : string -> ?cl:string -> (string * string) list -> node
val comment : string -> node
val text : string -> node

val list' :
     [< `ol | `ul ]
  -> ?cl:string
  -> ?attrs:(string * string) list
  -> ((string * string) list * node list) list
  -> node

val list :
     [< `ol | `ul ]
  -> ?cl:string
  -> ?attrs:(string * string) list
  -> node list list
  -> node

val pseudo_list :
     ?cl:string
  -> ?attrs:(string * string) list
  -> (node * (node -> node list)) list
  -> node list

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
val table : header:node list list option -> ?cl:string -> node list list list -> node
val id : string -> string * (string * string)
val script : string -> defer:bool -> node
val style : string -> node

val html :
  lang:string -> head:node list -> ?body_style:string -> body:node list -> unit -> node

val details :
     ?cl:string
  -> ?summary_cl:string
  -> ?attrs:(string * string) list
  -> ?open_:bool
  -> node list
  -> node list
  -> node

val cite : ?cl:string -> node list -> node

val flex_cl : ?wrap:bool -> [< `Column | `Row ] -> string
(** Css *)
