include module type of Core.Generic(struct
  type 'a q = (string -> unit) -> 'a -> unit
end)
