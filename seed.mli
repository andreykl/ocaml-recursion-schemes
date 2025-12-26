type t

val split : t -> t
val int_in_range : seed:t -> min:int -> max:int -> int
val inc : t -> t
val is_min : t -> bool
val is_max : t -> bool
val init : ?height:int -> ?init_rnd:int -> unit -> t
