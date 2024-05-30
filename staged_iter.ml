(* Not staged version, for exercise *)

open Tpf
module G = Tpf.Generic (struct type 'a q = 'a -> unit end)

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
    type (+_, _, +_) spine =
    | K : 'a code -> ('a, 'r, 'q) spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app
          -> ('b, 'r, 'q) spine
    | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine

    type ('a, +'q) t = 'a code -> ('a, 'a, 'q) spine

    val spine : ('a, 'q) t -> 'a code -> ('a, 'a, 'q) spine
  end = struct
    type (+_, _, +_) spine =
    | K : 'a code -> ('a, 'r, 'q) spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app
          -> ('b, 'r, 'q) spine
    | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine

    type ('a, +'q) t = 'a code -> ('a, 'a, 'q) spine

    let spine view = view
  end

  type ('a, 'x) data1 =
    { view : 'q. ('a, 'q, ('x, 'q) V.t) app1;
    }
end

open Staged

module G_staged = Tpf.Generic (struct
  type 'a q = 'a code -> unit code
end)

let rec staged_g_iter_aux
  : ('a, G_staged.p) V.t -> 'a code -> unit code =
  fun view v_code ->
    let rec go : 'a. ('a, _, _) V.spine -> unit code =
      fun spine ->
        match spine with
        | V.K _ -> [%code () ]
        | V.A (s, a_code, f_a) ->
            [%code
              [%e go s ];
              [%e G_staged.(!:) f_a a_code ]
            ]
        | V.R (s, a_code) ->
            [%code
              [%e go s ];
              [%e staged_g_iter_aux view a_code ]
            ]
    in
    go ((V.spine view : 'a code -> ('a, _, _) V.spine) v_code)

let staged_g_iter : ('a, G_staged.p) V.t -> ('a -> unit) code =
  fun view ->
    [%code (fun v -> [%e staged_g_iter_aux view [%code v] ]) ]

let staged_list_view : ('a, 'a list) Staged.data1 =
  { view =
    (fun (type q) (f_a : ('a, q) app) ->
      let v : ('a list, q) V.t =
         fun a_code ->
           assert false
      in
      v
    );
  }
