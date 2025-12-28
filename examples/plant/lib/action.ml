type t = Flower | Upwards | Branch

let of_int_in_range (int_in_range : min:int -> max:int -> int) : t =
  let v = int_in_range ~min:1 ~max:5 in
  if v < 2 then Flower else if v < 3 then Branch else Upwards
