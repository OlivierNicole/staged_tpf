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
    type (+_, _, +_) stat_spine =
    | K : 'a code -> ('a, 'r, 'q) stat_spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app code
          -> ('b, 'r, 'q) stat_spine
    (* | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) stat_spine *)
    | ComputeResult : ('r, 'q) app code -> ('a, 'r, 'q) stat_spine

    and (+'a, 'r, 'q) spine =
      { stat : ('a, 'r, 'q) stat_spine option;
        dyn : ('a, 'r, 'q) V.spine code }

    type ('a, 'q) t = 'a code -> ('a, 'a, 'q) spine

    let spine : ('a, 'q) t -> 'a code -> ('a, 'a, 'q) spine =
      fun view -> view
      [@@warning "-unused-value-declaration"]
  end

  type ('a, 'x) data1 =
    { explore : 'q.
           ('a, 'q) app code
        -> (('a option, 'q) V.t -> ('a option, 'q) app code)
        -> ('a option, 'q) app code
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

let (* rec *) staged_g_iter_aux
  : type statq dynq a.
       (a, Iterate_proxy.p) V.t
    -> a code
    -> unit code =
  fun view a_code ->
    let rec go : type b. (b, _, Iterate_proxy.p) V.spine -> unit code =
      fun spine ->
        match spine with
        | V.{ stat = Some (V.K _); dyn = _ } -> [%code () ]
        | { stat = Some (V.A (s, a_code, f_a)); dyn = _ } ->
            [%code
              [%e go s ];
              Dyn.Iterate_proxy.(!:) [%e f_a ] [%e a_code ]
            ]
        (*
        | { stat = Some (R (s, a_code)); dyn = _ } ->
            [%code
              [%e go s ];
              [%e staged_g_iter_aux view a_code ]
            ]
        *)
        | { stat = Some (ComputeResult code); dyn = _ } ->
            [%code Dyn.Iterate_proxy.(!:) [%e code ] [%e a_code ] ]
        | { stat = None; dyn = spine_code } ->
            [%code Dyn.iter_dyn_spine [%e spine_code ] ]
    in
    go (view a_code)

let staged_g_iter
  : ('a, Iterate_proxy.p) V.t -> ('a, Iterate_proxy.p) app code =
  fun view ->
    [%code Dyn.Iterate_proxy.(!) (fun v -> [%e staged_g_iter_aux view [%code v] ]) ]

(*
let list_data1 : ('a, 'a list) Staged.data1 =
  let explore
    : type q.
      ('a, q) app code
      -> (('a list, q) V.t -> ('a list, q) app code)
      -> ('a list, q) app code =
    fun f_a view_consumer ->
      view_consumer (fun (a_code : 'a list code) ->
        { stat = Some (V.ComputeResult
            [%code
            match [%e a_code ] with
            | [] ->
                [%e
                  view_consumer
                    (fun _ ->
                      V.{ stat = Some (K [%code [] ]);
                          dyn = [%code Tpf.V.K [] ]
                        }
                    )
                ]
            | x :: xs ->
                [%e
                  let remaining_spine =
                    V.{ stat = Some (K [%code List.cons ]);
                        dyn = [%code Tpf.V.K List.cons ]
                      }
                  in
                  view_consumer
                    (fun _ ->
                      V.{ stat = Some (R (remaining_spine, [%code xs ]));
                          dyn = [%code
                            Tpf.V.R ([%e remaining_spine.dyn ], xs) ]
                        }
                    )
                ]
            ]);
          dyn =
            [%code
              match [%e a_code ] with
              | [] -> Tpf.V.K []
              | x :: xs -> Tpf.V.R (K x, xs)
            ]
        })
  in
  { explore }
*)

let staged_option_data1 : ('a, 'a option) Staged.data1 =
  let explore
    : type dynq.
         ('a, dynq) app code
      -> (('a option, dynq) V.t -> ('a option, dynq) app code)
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
  { view : 'dynq. ('a, 'dynq, ('a option, 'dynq) V.t) app1 }
  =
  { view : 'dynq. ('a, 'dynq) app code -> ('a option, 'dynq) V.t }
  =
  { view : 'dynq. ('a, 'dynq) app code
                         -> (('a option, 'a option, 'dynq) V.spine -> ('a option, 'dynq) app code)
                         -> 'a option code
                         -> ('a option, 'dynq) app code }
  *)
  { explore }

(* How to use a [data1]? *)

let iter_option_code_aux : 'a. ('a -> unit) code -> ('a option, Iterate_proxy.p) app code =
  fun f ->
  staged_option_data1.explore
    ([%code Dyn.Iterate_proxy.(!) [%e f ] ] : ('a, Iterate_proxy.p) app code)
    (staged_g_iter)

let iter_option : ('a -> unit) -> 'a option -> unit =
  fun f opt ->
  Ppx_stage.run (
    [%code
      fun f opt -> Dyn.Iterate_proxy.(!:) [%e iter_option_code_aux [%code f ] ] opt
    ]
  )
  f
  opt

let show () =
  Ppx_stage.print
    Format.std_formatter
    (iter_option_code_aux [%code fun v -> Format.printf "%d\n" v])
