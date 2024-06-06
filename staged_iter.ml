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
    type (+_, _, _) stat_spine =
    | K : 'a code -> ('a, 'r, 'q) stat_spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app code
          -> ('b, 'r, 'q) stat_spine
    | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) stat_spine
    | DynSum : ('r code -> ('r, 'q) app code -> ('r, 'q) app code) -> ('a, 'r, 'q) stat_spine

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
        -> (('x, 'q) V.t -> ('x, 'q) app code option -> ('x, 'q) app code)
        -> ('x, 'q) app code
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
  : type a.
       (a, Iterate_proxy.p) V.t
    -> (a, Iterate_proxy.p) app code option (* Fixpoint function *)
    -> a code
    -> unit code =
  fun view fix a_code ->
    let rec go : type b. (a -> unit) code option -> (b, _, Iterate_proxy.p) V.spine -> unit code =
      fun fix spine ->
        match spine with
        | V.{ stat = Some (V.K _); dyn = _ } -> [%code () ]
        | { stat = Some (V.A (s, a_code, f_a)); dyn = _ } ->
            [%code
              [%e go fix s ];
              Dyn.Iterate_proxy.(!:) [%e f_a ] [%e a_code ]
            ]
        | { stat = Some (R (s, sub_instance)); dyn = _ } ->
            [%code
              [%e go fix s ];
              [%e
                match fix with
                | Some fix -> fix
                | None -> failwith "no fixpoint function when I needed it..."
              ] [%e sub_instance ]
            ]
        | { stat = Some (DynSum sum_iter); dyn = _ } ->
            [%code
              let rec fix x =
                Dyn.Iterate_proxy.(!:)
                  [%e sum_iter [%code x ] [%code Dyn.Iterate_proxy.(!) fix ] ]
                  x
              in
              fix [%e a_code ]
            ]
        | { stat = None; dyn = spine_code } ->
            [%code Dyn.iter_dyn_spine [%e spine_code ] ]
    in
    let spine = view a_code in
    go
      (Option.map (fun f -> [%code fun x -> Dyn.Iterate_proxy.(!:) [%e f ] x ]) fix)
      spine

let staged_g_iter
  : type a.
        (a, Iterate_proxy.p) V.t
     -> (a, Iterate_proxy.p) app code option (* Fixpoint function *)
     -> (a, Iterate_proxy.p) app code =
  fun view fix ->
    [%code
      Dyn.Iterate_proxy.(!)
        (fun v ->
          [%e staged_g_iter_aux view fix [%code v ] ])
    ]

let list_data1 : ('a, 'a list) Staged.data1 =
  let explore
    : type q.
      ('a, q) app code
      -> (('a list, q) V.t -> ('a list, q) app code option -> ('a list, q) app code)
      -> ('a list, q) app code =
    fun f_a view_consumer ->
      view_consumer
        (fun (a_code : 'a list code) ->
        { stat = Some (V.DynSum (fun a_code fix ->
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
                    (Some fix)
                ]
            | x :: xs ->
                [%e
                  let rem =
                    V.{ stat = Some (V.K [%code List.cons ]);
                        dyn = [%code Tpf.V.K List.cons ];
                      }
                  in
                  view_consumer
                    (fun _ ->
                      V.{ stat =
                            Some (
                              V.R (
                                V.{ stat = Some (A (rem, [%code x ], f_a));
                                    dyn = [%code Tpf.V.A ([%e rem.dyn ], x, [%e f_a ]) ] }
                              , xs));
                          dyn = [%code Tpf.V.R (A (K List.cons, x, [%e f_a ]), xs) ]
                        })
                    (Some fix)
                ]
            ]));
          dyn =
            [%code
              match [%e a_code ] with
              | [] -> Tpf.V.K []
              | x :: xs ->
                  Tpf.V.R (A (K List.cons, x, [%e f_a ]), xs)
            ]
        })
        None
  in
  { explore }

let staged_option_data1 : ('a, 'a option) Staged.data1 =
  let explore
    : type dynq.
         ('a, dynq) app code
      -> (('a option, dynq) V.t -> ('a option, dynq) app code option -> ('a option, dynq) app code)
      -> ('a option, dynq) app code =
      (*-> ('a option, 'a option, statq, dynq) V.spine =*)
    fun f_a view_consumer ->
      view_consumer (fun a_code ->
        { stat = Some (V.DynSum (fun a_code _ ->
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
                    None
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
                    None
                ]
            ]));
          dyn = [%code
            match [%e a_code ] with
            | None -> Tpf.V.K None
            | Some a ->
                  let remaining_spine = Tpf.V.K Option.some in
                   Tpf.V.A (remaining_spine, a, [%e f_a ])
            ]
        }
      )
      None
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

let iter_list_aux : 'a. ('a -> unit) code -> ('a list, Iterate_proxy.p) app code =
  fun f ->
    list_data1.explore
      [%code Dyn.Iterate_proxy.(!) [%e f ] ]
      staged_g_iter

let iter_list : ('a -> unit) -> 'a list -> unit =
  fun f l ->
    Ppx_stage.run
      [%code
        fun f l -> Dyn.Iterate_proxy.(!:) [%e iter_list_aux [%code f ] ] l
      ]
      f
      l

let iter_option_list_aux : ('a -> unit) code -> ('a option list, Iterate_proxy.p) app code =
  fun f ->
    list_data1.explore
      [%code Dyn.Iterate_proxy.(!)
        (
          Dyn.Iterate_proxy.(!:)
            [%e staged_option_data1.explore
              [%code Dyn.Iterate_proxy.(!) [%e f ] ]
              staged_g_iter
            ]
        )
      ]
      staged_g_iter

let iter_option_list f l =
  Ppx_stage.run
    [%code fun f -> Dyn.Iterate_proxy.(!:) [%e iter_option_list_aux [%code f ] ] ]
    f
    l

let show () =
  Ppx_stage.print
    Format.std_formatter
    (iter_list_aux [%code fun v -> Format.printf "%d\n" v]);
  Ppx_stage.print
    Format.std_formatter
    (iter_option_code_aux [%code Format.printf "%d\n" ]);
  Ppx_stage.print
    Format.std_formatter
    (iter_option_list_aux [%code Format.printf "%d\n" ]);
