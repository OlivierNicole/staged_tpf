(** Some administrative proxy type for the iteration query type ['a ->
    unit]. *)
type iter_p

val g_iter : ('a, iter_p) Tpf.view -> 'a -> unit

val list_iter : ('a -> unit) -> 'a list -> unit
