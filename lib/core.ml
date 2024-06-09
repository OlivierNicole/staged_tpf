open Ppx_stage

type (+'a, +'f) app

module V = struct
  type (+_, _, _) spine =
    | K : 'a code -> ('a, 'r, 'q) spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app code -> ('b, 'r, 'q) spine
    | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine

  type ('a, 'q, 'x) t = 'a code -> (('a, 'a, 'q) spine -> 'x code) -> 'x code
end

type ('q, 'res) app0 = 'res

type ('a, 'q, 'res) app1 = ('a, 'q) app code -> ('q, 'res) app0

type 'x data0 = { expose : 'q 'y. ('x, 'q, 'y) V.t }

type ('a, 'x) data1 = { expose : 'q 'y. ('a, 'q, ('x, 'q, 'y) V.t) app1 }

module Generic (Q : sig
  type 'a q
end) =
struct
  type 'a q = 'a Q.q

  type p (* The "brand". *)

  external ( ! ) : 'a q -> ('a, p) app = "%identity"

  external ( !: ) : ('a, p) app -> 'a q = "%identity"
end
