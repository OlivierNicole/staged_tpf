(* Not staged version, for exercise *)

open Tpf
module Iterate_proxy = Both_phases.G

let rec g_iter : ('a, Iterate_proxy.p) view -> 'a -> unit =
  fun view x ->
    let rec go: 'a. ('a, _, _) V.spine -> unit =
      function
      | V.K _ -> ()
      | V.A (s, a, f_a) -> go s; Iterate_proxy.(!:)f_a a
      | V.R (s, a) -> go s; g_iter view a
    in
    go (spine view x)

include Iterate_proxy.View(struct
  type 'a r = 'a -> unit
  let gfun = g_iter
end)

let _list_iter : ('a -> unit) -> 'a list -> unit =
  fun iter_value ->
  data1 Tpf_std.list iter_value

(* Staged version *)

open Ppx_stage

module Staged = struct
  module V = struct
    type (+_, _, _) spine =
    | K : 'a code -> ('a, 'r, 'q) spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app code -> ('b, 'r, 'q) spine
    | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine

    type ('a, 'q, 'x) t =
         'a code
      -> (('a, 'a, 'q) spine -> 'x code)
      -> 'x code
  end

  type ('a, 'x) data1 =
    { explore : 'q 'y.
           ('a, 'q) app code
        -> ('x, 'q, 'y) V.t
    }
end

open Staged

module Iterate_proxy_staged = Tpf.Generic (struct
  type 'a q = 'a code -> unit code
end)

module%code Dyn = struct [@code]
  module Iterate_proxy = Both_phases.G
end

let staged_g_iter_aux
  : type a y.
       (a, Iterate_proxy.p, unit) V.t
    -> a code
    -> unit code =
  fun view a_code ->
    let rec go : type b. (a -> unit) code option -> (b, _, Iterate_proxy.p) V.spine -> unit code =
      fun fix spine ->
        match spine with
        | V.K _ -> [%code () ]
        | V.A (s, a_code, f_a) ->
            [%code
              [%e go fix s ];
              Dyn.Iterate_proxy.(!:) [%e f_a ] [%e a_code ]
            ]
        | R (s, sub_instance) ->
            [%code
              [%e go fix s ];
              [%e
                match fix with
                | Some fix -> fix
                | None -> failwith "no fixpoint function when I needed it..."
              ] [%e sub_instance ]
            ]
    in
    [%code
      let rec fix x =
        [%e
          view
            [%code x ]
            (fun spine ->
              go (Some [%code fix ]) spine)
        ]
      in
      fix [%e a_code ]
    ]

let staged_g_iter
  : type a.
        (a, Iterate_proxy.p, unit) V.t
     -> (a, Iterate_proxy.p) app code =
  fun view ->
    [%code
      Dyn.Iterate_proxy.(!)
        (fun v ->
          [%e staged_g_iter_aux view [%code v ] ])
    ]

let list : ('a, 'a list) Staged.data1 =
  let explore
    : type q x.
      ('a, q) app code
      -> ('a list, q, x) V.t =
    fun f_a a_code spine_consumer ->
      [%code
        match [%e a_code ] with
        | [] ->
            [%e
              spine_consumer
                (V.K [%code [] ])
            ]
        | x :: xs ->
            [%e
              let rem = V.K [%code List.cons ] in
              spine_consumer
                (V.R ( A (rem, [%code x ], f_a) , xs))
            ]
      ]
  in
  { explore }

let option : ('a, 'a option) Staged.data1 =
  let explore
    : type q x.
         ('a, q) app code
      -> ('a option, q, x) V.t =
    fun f_a a_code spine_consumer ->
      [%code
        match [%e a_code ] with
        | None ->
            [%e spine_consumer (V.K [%code None]) ]
        | Some a ->
            [%e
              let remaining_spine = V.K [%code Option.some ] in
              spine_consumer
                (V.A (remaining_spine, [%code a ], f_a))
            ]
      ]
  in
  { explore }

(* How to use a [data1]? *)

module Iter : sig
  val data1 : ('a, 'x) Staged.data1 -> ('a code -> unit code) -> 'x code -> unit code
end = struct
  let data1 data1 f_a v_code =
    [%code
      Dyn.Iterate_proxy.(!:)
        [%e
          staged_g_iter
            (data1.explore
              [%code Dyn.Iterate_proxy.(!) (fun v -> [%e f_a [%code v ] ]) ])
        ]
        [%e v_code ]
    ]
end

let iter_option_aux : 'a. ('a -> unit) code -> 'a option code -> unit code =
  fun f ->
    Iter.data1 option (fun x -> [%code [%e f ] [%e x ] ])

let iter_option : ('a -> unit) -> 'a option -> unit =
  fun f opt ->
    Ppx_stage.run
      [%code
        fun f opt -> [%e iter_option_aux [%code f ] [%code opt ] ]
      ]
      f
      opt

let iter_list_aux : 'a. ('a -> unit) code -> 'a list code -> unit code =
  fun f ->
    Iter.data1 list (fun x -> [%code [%e f ] [%e x ] ])

let iter_list : ('a -> unit) -> 'a list -> unit =
  fun f l ->
    Ppx_stage.run
      [%code
        fun f l -> [%e iter_list_aux [%code f ] [%code l ] ]
      ]
      f
      l

let iter_option_list_aux : ('a -> unit) code -> 'a option list code -> unit code =
  fun f ->
    Iter.data1 list (Iter.data1 option (fun x -> [%code [%e f ] [%e x ] ]))

let iter_option_list f l =
  Ppx_stage.run
    [%code fun f l -> [%e iter_option_list_aux [%code f ] [%code l ] ] ]
    f
    l

let show () =
  Ppx_stage.print
    Format.std_formatter
      [%code fun f l -> [%e iter_list_aux [%code f ] [%code l ] ] ];
  Ppx_stage.print
    Format.std_formatter
    [%code
      fun f opt -> [%e iter_option_aux [%code Format.printf "%d\n" ] [%code opt ] ] ];
  Ppx_stage.print
    Format.std_formatter
    [%code
      fun f l -> [%e iter_option_list_aux [%code Format.printf "%d\n" ] [%code l ] ] ];
