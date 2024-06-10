open Core
open Ppx_stage

module G = Show_generic

module%code Dyn = struct
  module G = Show_generic
end [@code]

let show' :
  type a y. (a, G.p, Format.formatter -> unit) V.t -> a code -> Format.formatter code -> unit code =
  fun view a_code fmt_code ->
    let rec go :
      type b. (Format.formatter -> a -> unit) code option
           -> (b, a, G.p) V.spine
           -> Format.formatter code
           -> unit code =
      fun fix spine fmt_code ->
        match spine with
        | V.K _ -> [%code () ]
        | A (s, a_code, f_a) ->
            [%code
              [%e go fix s fmt_code];
              Dyn.G.( !: ) [%e f_a ] [%e fmt_code] [%e a_code ]]
        | R (s, sub_instance) ->
            [%code
              [%e go fix s fmt_code];
              [%e
                match fix with
                | Some fix ->
                    [%code fun fmt_code x_code ->
                      [%e fix] [%e fmt_code] [%e x_code]]
                | None -> assert false]
                [%e sub_instance]]
    in
    [%code
      let rec fix fmt_code x =
        [%e
          view
            [%code x]
            (fun spine ->
              [%code
                fun fmt ->
                  [%e go (Some [%code fix]) spine [%code fmt]]
              ]
            )
         ]
      in
      fix [%e fmt_code] [%e a_code]]
