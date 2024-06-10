(* Not staged version, for exercise *)
open Core
open Ppx_stage
module G = Iterate_generic

module%code Dyn = struct
  module G = Iterate_generic
end [@code]

let staged_g_iter_aux : type a y. (a, G.p, unit) V.t -> a code -> unit code =
 fun view a_code ->
  let rec go : type b. (a -> unit) code option -> (b, _, G.p) V.spine -> unit code =
   fun fix spine ->
    match spine with
    | V.K _ -> [%code ()]
    | V.A (s, a_code, f_a) ->
        [%code
          [%e go fix s];
          Dyn.G.( !: ) [%e f_a] [%e a_code]]
    | R (s, sub_instance) ->
        [%code
          [%e go fix s];
          [%e
            match fix with
            | Some fix -> fix
            | None -> failwith "no fixpoint function when I needed it..."]
            [%e sub_instance]]
  in
  [%code
    let rec fix x = [%e view [%code x] (fun spine -> go (Some [%code fix]) spine)] in
    fix [%e a_code]]

let staged_g_iter : type a. (a, G.p, unit) V.t -> (a, G.p) app code =
 fun view -> [%code Dyn.G.( ! ) (fun v -> [%e staged_g_iter_aux view [%code v]])]

(* How to use a [data1]? *)

module Iter : sig
  val data1 : ('a, 'x) data1 -> ('a code -> unit code) -> 'x code -> unit code
end = struct
  let data1 data1 f_a v_code =
    [%code
      Dyn.G.( !: )
        [%e
          staged_g_iter (data1.expose [%code Dyn.G.( ! ) (fun v -> [%e f_a [%code v]])])]
        [%e v_code]]
end

let iter_option_aux : 'a. ('a -> unit) code -> 'a option code -> unit code =
 fun f -> Iter.data1 option (fun x -> [%code [%e f] [%e x]])

let iter_option : ('a -> unit) -> 'a option -> unit =
 fun f opt ->
  Ppx_stage.run [%code fun f opt -> [%e iter_option_aux [%code f] [%code opt]]] f opt

let iter_list_aux : 'a. ('a -> unit) code -> 'a list code -> unit code =
 fun f -> Iter.data1 list (fun x -> [%code [%e f] [%e x]])

let iter_list : ('a -> unit) -> 'a list -> unit =
 fun f l -> Ppx_stage.run [%code fun f l -> [%e iter_list_aux [%code f] [%code l]]] f l

let iter_option_list_aux : ('a -> unit) code -> 'a option list code -> unit code =
 fun f -> Iter.data1 list (Iter.data1 option (fun x -> [%code [%e f] [%e x]]))

let iter_option_list f l =
  Ppx_stage.run [%code fun f l -> [%e iter_option_list_aux [%code f] [%code l]]] f l

let show () =
  Ppx_stage.print
    Format.std_formatter
    [%code fun f l -> [%e iter_list_aux [%code f] [%code l]]];
  Ppx_stage.print
    Format.std_formatter
    [%code fun f opt -> [%e iter_option_aux [%code Format.printf "%d\n"] [%code opt]]];
  Ppx_stage.print
    Format.std_formatter
    [%code fun f l -> [%e iter_option_list_aux [%code Format.printf "%d\n"] [%code l]]]
