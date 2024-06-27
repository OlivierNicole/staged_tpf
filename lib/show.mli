open Ppx_stage

val data1 : ('a, 'x) Core.data1 -> (Format.formatter -> 'a -> unit) code -> 'x code -> unit code
