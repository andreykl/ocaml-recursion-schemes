module Plant = struct
  module F = struct
    type 'a t = Root of 'a | Stalk of 'a | Fork of 'a * 'a * 'a | Bloom

    let map f = function
      | Root a -> Root (f a)
      | Stalk a -> Stalk (f a)
      | Fork (l, c, r) -> Fork (f l, f c, f r)
      | Bloom -> Bloom
  end

  include F
  module R = Recursion.Schemes.Make (F)
end

module Action = struct
  type t = Flower | Upwards | Branch

  let of_int_in_range (f : min:int -> max:int -> int) : t =
    let v = f ~min:1 ~max:5 in
    if v < 2 then Flower else if v < 3 then Branch else Upwards
end

module Seed : sig
  type t

  val split : t -> t
  val int_in_range : seed:t -> min:int -> max:int -> int
  val inc : t -> t
  val is_min : t -> bool
  val is_max : t -> bool
  val init : unit -> t
end = struct
  module RS = Random.State

  type t = { height : int; rnst : RS.t }

  let split seed = { seed with rnst = RS.split seed.rnst }
  let int_in_range ~seed ~min ~max = RS.int_in_range seed.rnst ~min ~max
  let inc seed = { seed with height = seed.height + 1 }
  let is_min seed = seed.height < 1
  let is_max seed = seed.height > 9
  let init () = { height = 0; rnst = RS.make (Array.make 1 1) }
end

let grow seed : Action.t * Seed.t =
  let act = Action.of_int_in_range (Seed.int_in_range ~seed) in
  (act, Seed.inc seed)

let sow : Seed.t Plant.R.cvcoalgebra =
 fun seed ->
  let act, next = grow seed in
  let open Plant in
  if Seed.is_min seed then Root (R.Automatic next)
  else if Seed.is_max seed then Bloom
  else
    let open Action in
    match act with
    | Flower -> Bloom
    | Upwards -> Stalk (R.Automatic next)
    | Branch ->
        Fork
          ( R.Manual (Stalk (R.Automatic next)),
            R.Manual Bloom,
            R.Manual (Stalk (R.Automatic (Seed.split next))) )

let _ = sow (Seed.init ())
let _ = Format.print_string "hello\n"
