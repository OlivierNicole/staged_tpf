(* Not staged version, for exercise *)

open Tpf
module G = Both_phases.G

type iter_p = G.p

let rec g_iter : ('a, G.p) view -> 'a -> unit =
  fun view x ->
    let rec go: 'a. ('a, _, _) V.spine -> unit =
      function
      | V.K _ -> ()
      | V.A (s, a, f_a) -> go s; G.(!:)f_a a
      | V.R (s, a) -> go s; g_iter view a
    in
    go (spine view x)

include G.View(struct
  type 'a r = 'a -> unit
  let gfun = g_iter
end)

let _list_iter : ('a -> unit) -> 'a list -> unit =
  fun iter_value ->
  data1 Tpf_std.list iter_value

(* Staged version *)

open Ppx_stage

module Staged = struct
  module V : sig
    type (+_, _, +_, +_) stat_spine =
    | K : 'a code -> ('a, 'r, 'statq, 'dynq) stat_spine
    | A : ('a -> 'b, 'r, 'statq, 'dynq) spine * 'a code * ('a, 'statq) app
          -> ('b, 'r, 'statq, 'dynq) stat_spine
    (* Let's try without recursive types first *)
    (*| R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine*)

    and (+'a, 'r, +'statq, +'dynq) spine =
      { stat : ('a, 'r, 'statq, 'dynq) stat_spine option;
        dyn : ('a, 'r, 'dynq) V.spine code }

    type ('a, +'statq, +'dynq) t = 'a code -> ('a, 'a, 'statq, 'dynq) spine

    val spine : ('a, 'statq, 'dynq) t -> 'a code -> ('a, 'a, 'statq, 'dynq) spine
  end = struct
    type (+_, _, +_, +_) stat_spine =
    | K : 'a code -> ('a, 'r, 'statq, 'dynq) stat_spine
    | A : ('a -> 'b, 'r, 'statq, 'dynq) spine * 'a code * ('a, 'statq) app
          -> ('b, 'r, 'statq, 'dynq) stat_spine
    (* Let's try without recursive types first *)
    (*| R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine*)

    and (+'a, 'r, +'statq, +'dynq) spine =
      { stat : ('a, 'r, 'statq, 'dynq) stat_spine option;
        dyn : ('a, 'r, 'dynq) V.spine code }

    type ('a, +'statq, +'dynq) t = 'a code -> ('a, 'a, 'statq, 'dynq) spine

    let spine view = view
  end

  type ('q, 'res) app0 = 'res

  type ('a, 'q, 'res) app1 = ('a, 'q) app code -> ('q, 'res) app0

  type ('a, 'x) data1 =
    (* Only ['dynq] here I think??? *)
    { view : 'statq 'dynq. ('a, 'dynq, ('x, 'statq, 'dynq) V.t) app1;
    }
end

open Staged

module G_staged = Tpf.Generic (struct
  type 'a q = 'a code -> unit code
end)

module%code Dyn = struct [@code]
  module G = Both_phases.G
  let rec iter_dyn_spine : 'a. ('a, 'r, G.p) Tpf.V.spine -> unit =
    function
    | Tpf.V.K _ -> ()
    | Tpf.V.A (s, v, f) -> iter_dyn_spine s; G.(!:) f v
    | Tpf.V.R _ -> failwith "unsupported"
end

let staged_g_iter_aux
  : ('a, G_staged.p, G.p) V.t -> 'a code -> unit code =
  fun view v_code ->
    let rec go : 'a. ('a, _, G_staged.p, G.p) V.spine -> unit code =
      fun spine ->
        match spine with
        | V.{ stat = Some (V.K _); dyn = _ } -> [%code () ]
        | { stat = Some (V.A (s, a_code, f_a)); dyn = _ } ->
            [%code
              [%e go s ];
              [%e G_staged.(!:) f_a a_code ]
            ]
        | { stat = None; dyn = spine_code } ->
            [%code Dyn.iter_dyn_spine [%e spine_code] ]
        (*
        | V.R (s, a_code) ->
            [%code
              [%e go s ];
              [%e staged_g_iter_aux view a_code ]
            ]
        *)
    in
    go ((V.spine view : 'a code -> ('a, _, _, _) V.spine) v_code)

let staged_g_iter : ('a, G_staged.p, G.p) V.t -> ('a -> unit) code =
  fun view ->
    [%code (fun v -> [%e staged_g_iter_aux view [%code v] ]) ]

let staged_option_data1 : ('a, 'a option) Staged.data1 =
  let view
    : type statq dynq. ('a, dynq) app code -> 'a option code -> ('a option, 'a option, statq, dynq) V.spine =
    fun f_a ->
      fun option_code ->
        V.{ stat = None;
            dyn =
              (
              [%code
                match [%e option_code ] with
                | None -> Tpf.V.K None
                | Some a ->
                    Tpf.V.A (K Option.some, a, [%e f_a ])
              ]
              : ('a option, 'a option, dynq) Tpf.V.spine code
              );
          }
  in
  (*
  ('a, 'a option) Staged.data1
  =
  { view : 'statq 'dynq. ('a, 'dynq, ('a option, 'statq, 'dynq) V.t) app1 }
  =
  { view : 'statq 'dynq. ('a, 'dynq, ('a option, 'statq, 'dynq) V.t) app1 }
  =
  { view : 'statq 'dynq. ('a, 'dynq) app -> ('a option, 'statq, 'dynq) V.t }
  =
  { view : 'statq 'dynq. ('a, 'dynq) app -> 'a code -> ('a option, 'a option, 'statq, 'dynq) V.spine }
  *)
  { view }

(* How to use a [data1]? *)

let iter_option_code_aux : 'a. (('a -> unit) code) -> ('a option -> unit) code =
  fun f ->
  staged_g_iter
    (staged_option_data1.view
      ([%code Dyn.G.(!) [%e f ] ] : ('a, G.p) app code)
    )

let iter_option : 'a. ('a -> unit) -> ('a option -> unit) =
  fun f x ->
    Ppx_stage.run
      [%code
        fun f x ->
          [%e
            staged_g_iter
              (staged_option_data1.view
                ([%code Dyn.G.(!) f ] : ('a, G.p) app code)
              )
          ]
          x
      ]
      f
      x

let show () =
  Ppx_stage.print Format.std_formatter (iter_option_code_aux [%code fun _ -> ()])
