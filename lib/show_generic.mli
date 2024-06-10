include module type of Core.Generic (struct
  type 'a q = Format.formatter -> 'a -> unit
end)
