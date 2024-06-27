open Core
open Ppx_stage

module G = Show_generic

module%code Dyn = struct
  module G = Show_generic
end [@code]

let show' :
  type a y. (a, G.p, Format.formatter -> unit) V.t -> a code -> Format.formatter code -> unit code =
  fun view a fmt ->
    let rec go :
      type b. (Format.formatter -> a -> unit) code option
           -> Format.formatter code
           -> (b, a, G.p) V.spine
           -> unit code =
      fun fix fmt spine ->
        match spine with
        | V.K _ -> [%code () ]
        | A (s, a_code, f_a) ->
            [%code
              [%e go fix fmt s];
              Dyn.G.( !: ) [%e f_a ] [%e fmt] [%e a_code ]]
        | R (s, sub_instance) ->
            [%code
              [%e go fix fmt s];
              [%e Option.get fix] [%e fmt] [%e sub_instance]]
    in
    [%code
      let rec fix fmt x =
        [%e
          (view
            ([%code x] : a code)
            (fun spine ->
              [%code
                fun fmt ->
                  [%e go (Some [%code fix]) [%code fmt] spine]
              ]
            )
          : (Format.formatter -> unit) code)
         ]
           [%e fmt]
      in
      fix [%e fmt] [%e a]]

let data1 (data1 : ('a, 'x) data1) fmt_sub v =
  [%code
    Dyn.G.( !: )
      [%e
        show'
