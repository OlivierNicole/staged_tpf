(** Instance of {!Core.Generic} for an iterator request ['a -> unit]. This needs
    to be in a separate module to be usable in both compile-time and run-time
    phases; this is a limitation of [ppx_stage]. *)
include Core.Generic (struct type 'a q = 'a -> unit end)
