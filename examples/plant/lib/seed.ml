module RS = Random.State

type t = { height : int; rnst : RS.t; height_max : int }

let split seed = { seed with rnst = RS.split seed.rnst }
let int_in_range ~seed ~min ~max = RS.int_in_range seed.rnst ~min ~max
let inc seed = { seed with height = seed.height + 1 }
let is_min seed = seed.height < 1
let is_max seed = seed.height >= seed.height_max

let init ?(height_max = 9) ?(init_rnd = 1) () =
  { height = 0; rnst = RS.make (Array.make 1 init_rnd); height_max }

let self_init ?(min_possible_height_max = 1) ?(max_possible_height_max = 9) () =
  let rnst = RS.make_self_init () in
  {
    height = 0;
    rnst;
    height_max =
      RS.int_in_range rnst ~min:min_possible_height_max
        ~max:max_possible_height_max;
  }
