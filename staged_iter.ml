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
    type (_, _, +_, _) stat_spine =
    | K : 'a code -> ('a, 'r, 'statq, 'dynq) stat_spine
    | A : ('a -> 'b, 'r, 'statq, 'dynq) spine * 'a code * ('a, 'dynq) app code
          -> ('b, 'r, 'statq, 'dynq) stat_spine
    | ComputeResult : ('r, 'dynq) app code -> ('a, 'r, 'statq, 'dynq) stat_spine
    (*| DynExplore : ('a -> ('a, 'dynq) app) code -> ('a, 'r, 'statq, 'dynq) stat_spine*)
    (* Let's try without recursive types first *)
    (*| R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine*)

    and ('a, 'r, +'statq, 'dynq) spine =
      { stat : ('a, 'r, 'statq, 'dynq) stat_spine option;
        dyn : ('a, 'r, 'dynq) V.spine code }

    type ('a, +'statq, 'dynq) t = 'a code -> ('a, 'a, 'statq, 'dynq) spine

    let spine : ('a, 'statq, 'dynq) t -> 'a code -> ('a, 'a, 'statq, 'dynq) spine =
      fun view -> view
      [@@warning "-unused-value-declaration"]
  end

  (*
  type ('a, 'x) data1 =
    (* Only ['dynq] here I think??? *)
    { view : 'statq 'dynq.
        ('a, 'dynq) app code
        -> (('x, 'x, 'statq, 'dynq) V.spine -> ('x, 'dynq) app code)
        -> 'x code
        -> ('x, 'dynq) app code
    }
  *)
  type ('a, 'x) data1 =
    { explore : 'statq 'dynq.
           ('a, 'dynq) app code
        -> (('a option, 'statq, 'dynq) V.t -> ('a option, 'dynq) app code)
        -> ('a option, 'dynq) app code
    }
end

open Staged

module Iterate_proxy_staged = Tpf.Generic (struct
  type 'a q = 'a code -> unit code
end)

module%code Dyn = struct [@code]
  module Iterate_proxy = Both_phases.G
  let rec iter_dyn_spine : 'a. ('a, 'r, Iterate_proxy.p) Tpf.V.spine -> unit =
    function
    | Tpf.V.K _ -> ()
    | Tpf.V.A (s, v, f) -> iter_dyn_spine s; Iterate_proxy.(!:) f v
    | Tpf.V.R _ -> failwith "unsupported"
end

let staged_g_iter_aux
  : type statq dynq a.
       (a, Iterate_proxy_staged.p, Iterate_proxy.p) V.t
    -> a code
    -> unit code =
  fun view a_code ->
    let rec go : type b. (b, _, Iterate_proxy_staged.p, Iterate_proxy.p) V.spine -> unit code =
      fun spine ->
        match spine with
        | V.{ stat = Some (V.K _); dyn = _ } -> [%code () ]
        | { stat = Some (V.A (s, a_code, f_a)); dyn = _ } ->
            [%code
              [%e go s ];
              Dyn.Iterate_proxy.(!:) [%e f_a ] [%e a_code ]
            ]
        | { stat = Some (ComputeResult code); dyn = _ } ->
            [%code Dyn.Iterate_proxy.(!:) [%e code ] [%e a_code ] ]
        | { stat = None; dyn = spine_code } ->
            [%code Dyn.iter_dyn_spine [%e spine_code ] ]
        (*
        | V.R (s, a_code) ->
            [%code
              [%e go s ];
              [%e staged_g_iter_aux view a_code ]
            ]
        *)
    in
    go (view a_code)

let staged_g_iter
  : ('a, Iterate_proxy_staged.p, Iterate_proxy.p) V.t -> ('a, Iterate_proxy.p) app code =
  fun view ->
    [%code Dyn.Iterate_proxy.(!) (fun v -> [%e staged_g_iter_aux view [%code v] ]) ]

let staged_option_data1 : ('a, 'a option) Staged.data1 =
  let explore
    : type statq dynq.
         ('a, dynq) app code
      -> (('a option, statq, dynq) V.t -> ('a option, dynq) app code)
      -> ('a option, dynq) app code =
      (*-> ('a option, 'a option, statq, dynq) V.spine =*)
    fun f_a view_consumer ->
      view_consumer (fun a_code ->
        { stat = Some (V.ComputeResult
            [%code
            match [%e a_code ] with
            | None ->
                [%e
                  view_consumer
                    (fun _ ->
                      V.{ stat = Some (K [%code None]);
                          dyn = [%code Tpf.V.K None ]
                        }
                    )
                ]
            | Some a ->
                [%e
                  let remaining_spine =
                    V.{ stat = Some (K [%code Option.some ]);
                        dyn = [%code Tpf.V.K Option.some ]
                      }
                  in
                  view_consumer
                    (fun _ ->
                      V.{ stat =
                            Some (A (remaining_spine, [%code a ], f_a));
                          dyn =
                            [%code Tpf.V.A ([%e remaining_spine.dyn ], a, [%e f_a ]) ];
                        }
                    )
                ]
            ]);
          dyn = [%code
            match [%e a_code ] with
            | None -> Tpf.V.K None
            | Some a ->
                  let remaining_spine = Tpf.V.K Option.some in
                   Tpf.V.A (remaining_spine, a, [%e f_a ])
            ]
        }
      )
  in
  (*
  ('a, 'a option) Staged.data1
  =
  { view : 'statq 'dynq. ('a, 'dynq, ('a option, 'statq, 'dynq) V.t) app1 }
  =
  { view : 'statq 'dynq. ('a, 'dynq) app code -> ('a option, 'statq, 'dynq) V.t }
  =
  { view : 'statq 'dynq. ('a, 'dynq) app code
                         -> (('a option, 'a option, 'statq, 'dynq) V.spine -> ('a option, 'dynq) app code)
                         -> 'a option code
                         -> ('a option, 'dynq) app code }
  *)
  { explore }

(* How to use a [data1]? *)

let iter_option_code_aux : ('a -> unit) code -> ('a option, Iterate_proxy.p) app code =
  fun f ->
  staged_option_data1.explore
    ([%code Dyn.Iterate_proxy.(!) [%e f ] ] : ('a, Iterate_proxy.p) app code)
    (staged_g_iter)

let iter_option : 'a. ('a -> unit) -> 'a option -> unit =
  fun f opt ->
  Ppx_stage.run (
    [%code
      fun f opt -> Dyn.Iterate_proxy.(!:) [%e iter_option_code_aux [%code f ] ] opt
    ]
  ) f opt

let show () =
  Ppx_stage.print
    Format.std_formatter
    (iter_option_code_aux [%code fun v -> Format.printf "%d\n" v])
