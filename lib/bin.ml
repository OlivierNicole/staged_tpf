(*
open Core
open Ppx_stage
*)

module G = Bin_generic

module%code Dyn = struct
  module G = Bin_generic
end [@code]

(*
let bin' : type a. (a, G.p, unit) V.t -> (string -> unit) code -> a code -> unit code =
  fun view write a ->
    let rec go
      : type b. ((string -> unit) code -> a code -> unit code) option
                -> (b, a, G.p) V.spine
                -> unit code =
      fun fix spine ->

*)
