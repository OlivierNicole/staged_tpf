open Ppx_stage

(** {1:core Core types and utility functions} *)

type (+'a, +'f) app
(** Type representing type application of [f] to [a] in the style of {{:
    https://github.com/ocamllabs/higher}higher}. *)

(** {2:view Views} *)

module V : sig
  type (+_, _, _) spine =
    | K : 'a code -> ('a, 'r, 'q) spine
    | A : ('a -> 'b, 'r, 'q) spine * 'a code * ('a, 'q) app code -> ('b, 'r, 'q) spine
    | R : ('r -> 'b, 'r, 'q) spine * 'r code -> ('b, 'r, 'q) spine

  type ('a, 'q, 'x) t = 'a code -> (('a, 'a, 'q) spine -> 'x code) -> 'x code
end

(** {2:appn App} *)

(** [(t1, ..., q, res) app[n]] is an alias for [n]-ary functions
    [(t1, q) app -> ... -> res]. *)

type ('q, 'res) app0 = 'res

type ('a, 'q, 'res) app1 = ('a, 'q) app code -> ('q, 'res) app0

(** {1:data Data} *)

(** [data[n]] packages up the {!view} for a single type which contains [n]
    other types. This is the easiest generic representation to handle, but it
    is not necessary.

    Generically representable types should export their [data].

    Generic functions should export a [data]-based interface, together with a
    naked function that operates directly on a {!view}. *)

type 'x data0 = { expose : 'q 'y. ('x, 'q, 'y) V.t }

type ('a, 'x) data1 = { expose : 'q 'y. ('a, 'q, ('x, 'q, 'y) V.t) app1 }

(** Interface between the outside world and a [spine].

    It contains only three necessary symbols:

    {ul
    {- {e users} need [p] and [!], to inject from ['a Q.q] to [('a, p) app];
       while}
    {- {e implementors} need [p] and [!:], to project from [('a, p) app] to
       ['a Q.q].}}

    The rest of this module is provided for the implementor's convenience.

    Minimal complete interface to a generic function consists of [p], [!], and
    a function that looks like one of
{[val f: ('a, p) view -> ... -> 'a -> ...
val g: ('a, p) schema -> ...]}

    A more complete interface adds a family of functions like

{[val f0 : 'x data0 -> ...
val f1 : ('a, 'x) data1 -> 'a Q.q -> ...
val f2 : ('a, 'b, 'x) data2 -> 'a Q.q -> 'b Q.q -> ...
...
]}

    These can be

    {ul
    {- produced with the {!View} and {!Schema} functors, which have pre-canned
       module types, but fixed names; or}
    {- constructed manually, perhaps by using the functions {{!app0}[app[n]]},
       with their signature spelled out by hand.}}
*)
module Generic (Q : sig
  type 'a q
end) : sig
  type 'a q = 'a Q.q
  (** Query type for this (group of) function(s). It gives the action to be done
      for each constructor argument. *)

  type p
  (** Proxy representing [Q.q].

      [p]s exists only to embed ['a Q.q] in a [spine].

      The only possible operations involving [p] are the two below. *)

  external ( ! ) : 'a q -> ('a, p) app = "%identity"
  (** [!x] injects into the proxy. *)

  external ( !: ) : ('a, p) app -> 'a q = "%identity"
  (** [!:x] projects from the proxy. *)

  (*
  module P: P with type p = p and type 'a q := 'a Q.q
  (** Groups {!p} and {!(!)}, above, for easy export. *)

  (** Functors generating a [data[n]] interface.

      {b Note.} They {e do not include} types [q] and [r] from {!Data}; when
      describing their output type in signatures using {!Data}, you must eliminate
      [q] and [r]. *)

  (** [View] equips a generic consumer [gfun] with the
      {{!Tpf.data}[data[n]]} interface, for easy export. *)
  module View (F: sig
    type 'a r
    val gfun: ('a, p) view -> 'a r
  end) : Data with type 'a q := 'a Q.q and type 'a r := 'a F.r

  (** [Schema] equips a generic producer [gfun] the the
      {{!Tpf.data}[data[n]]} interface, for easy export. *)
  module Schema (F: sig
    type 'a r
    val gfun: ('a, p) schema -> 'a r
  end) : Data with type 'a q := 'a Q.q and type 'a r := 'a F.r

  (** Helpers for manually exporting generic functions.

      [app[n] k f] converts [f: ('a, p) app -> ...] into {{!q}['a q -> ...]} and
      applies [k] to it.

      For instance, {!View} is given by
{[let data0 (d: _ data0) = app0 gfun d.view
let data1 (d: _ data1) = app1 gfun d.view
...
]}
   *)

  val app0 : ('cont -> 'res) -> (p, 'cont) app0 ->
             'res
  val app1 : ('cont -> 'res) -> ('a, p, 'cont) app1 ->
             'a q -> 'res
  val app2 : ('cont -> 'res) -> ('a, 'b, p, 'cont) app2 ->
             'a q -> 'b q -> 'res
  val app3 : ('cont -> 'res) -> ('a, 'b, 'c, p, 'cont) app3 ->
             'a q -> 'b q -> 'c q -> 'res
  val app4 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, p, 'cont) app4 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'res
  val app5 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, p, 'cont) app5 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'res
  val app6 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, p, 'cont) app6 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'res
  val app7 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, p, 'cont) app7 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'res
  val app8 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, p, 'cont) app8 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'res
  val app9 : ('cont -> 'res) -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, p, 'cont) app9 ->
             'a q -> 'b q -> 'c q -> 'd q -> 'e q -> 'f q -> 'g q -> 'h q -> 'i q -> 'res
   *)
end
