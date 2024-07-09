type selected_rules

val selected_rules : selected_rules Brrex.B.t
val selected_rules' : selected_rules Brrex.B.t'

val staged_generate :
     Jv.t
  -> ((selected_rules * string * bool) * Jstr.t * Jstr.t) option
  -> Brr.Blob.t option
  -> ((string -> string option) * Dict_gen_common.Dict_gen.metadata) Fut.or_error

val main : unit -> unit
