(** Some administrative proxy type for the iteration query type ['a ->
    unit]. *)
type iter_p

val g_iter : ('a, iter_p) Tpf.view -> 'a -> unit

val iter_option : ('a -> unit) -> 'a option -> unit

val show : unit -> unit
