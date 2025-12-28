module Seed = Plant_internal_lib.Seed
module Plant = Plant_internal_lib.Plant

let main () =
  let open Plant in
  let p = R.futu sow (Seed.self_init ~max_possible_height_max:24 ()) in
  to_string p |> print_endline

let _ = main ()
