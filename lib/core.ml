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

module type Data = sig

  type 'a q

  type 'a r

  val data0 : 'x data0 ->
              'x r
  val data1 : ('a, 'x) data1 ->
              'a q -> 'x r
  (*
  val data2 : ('a, 'b, 'x) data2 ->
              'a q -> 'b q -> 'x r
  val data3 : ('a, 'b, 'c, 'x) data3 ->
              'a q -> 'b q -> 'c q -> 'x r
  val data4 : ('a, 'b, 'c, 'd, 'x) data4 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'x r
  val data5 : ('a, 'b, 'c, 'd, 'e, 'x) data5 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'x r
  val data6 : ('a, 'b, 'c, 'd, 'e, 'f, 'x) data6 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'x r
  val data7 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'x) data7 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'x r
  val data8 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'x) data8 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q ->
              'x r
  val data9 : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'x) data9 ->
              'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q ->
              'i q -> 'x r
  *)
end


module Generic (Q : sig
  type 'a q
end) =
struct
  type 'a q = 'a Q.q

  type p (* The "brand". *)

  external ( ! ) : 'a q -> ('a, p) app = "%identity"

  external ( !: ) : ('a, p) app -> 'a q = "%identity"
end
