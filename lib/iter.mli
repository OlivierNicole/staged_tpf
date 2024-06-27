open Ppx_stage

val data1 : ('a, 'x) Core.data1 -> ('a code -> unit code) -> 'x code -> unit code

(*
val iter_option : ('a -> unit) -> 'a option -> unit

val iter_list : ('a -> unit) -> 'a list -> unit

val iter_option_list : ('a -> unit) -> 'a option list -> unit

val show : unit -> unit
*)
