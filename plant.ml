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
