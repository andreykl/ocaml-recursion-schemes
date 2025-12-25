module RS = Random.State

type t = { height : int; rnst : RS.t }

let split seed = { seed with rnst = RS.split seed.rnst }
let int_in_range ~seed ~min ~max = RS.int_in_range seed.rnst ~min ~max
let inc seed = { seed with height = seed.height + 1 }
let is_min seed = seed.height < 1
let is_max seed = seed.height > 9
let init () = { height = 0; rnst = RS.make (Array.make 1 1) }
