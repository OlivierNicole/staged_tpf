open Core
open Ppx_stage

let list : ('a, 'a list) data1 =
  let expose : type q x. ('a, q) app code -> ('a list, q, x) V.t =
   fun f_a a_code spine_consumer ->
    [%code
      match [%e a_code] with
      | [] -> [%e spine_consumer (V.K [%code []])]
      | x :: xs ->
          [%e
            let rem = V.K [%code List.cons] in
            spine_consumer (V.R (A (rem, [%code x], f_a), xs))]]
  in
  { expose }

let option : ('a, 'a option) data1 =
  let expose : type q x. ('a, q) app code -> ('a option, q, x) V.t =
   fun f_a a_code spine_consumer ->
    [%code
      match [%e a_code] with
      | None -> [%e spine_consumer (V.K [%code None])]
      | Some a ->
          [%e
            let remaining_spine = V.K [%code Option.some] in
            spine_consumer (V.A (remaining_spine, [%code a], f_a))]]
  in
  { expose }

