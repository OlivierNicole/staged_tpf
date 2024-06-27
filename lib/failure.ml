module%code T : sig [@code]
  type 'a t
end = struct [@code]
  type 'a t
end

module type M = sig
  val x : 'a T.t
end
