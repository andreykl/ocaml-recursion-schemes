module Seed = Plant_internal_lib.Seed
module Plant = Plant_internal_lib.Plant

let main () =
  let open Plant in
  let p = R.futu sow (Seed.self_init ()) in
  to_string p |> print_endline

let _ = main ()

(*
let p1 : 'a R.term = R.W (F.Root (R.W F.Bloom))
let p2 : 'a R.term = R.W (F.Root (R.W (F.Stalk (R.W F.Bloom))))

let p3 : 'a R.term =
  R.W
    (F.Root
       (R.W
          (F.Stalk
             (R.W
                (F.Fork
                   ( R.W (F.Stalk (R.W F.Bloom)),
                     R.W F.Bloom,
                     R.W (F.Stalk (R.W F.Bloom)) ))))))

let _ = print_endline
let _ = print_endline
let _ = print_endline "------------------- P1 ---------------"
let _ = print p1
let _ = print_endline
let _ = print_endline "------------------- P2 ---------------"
let _ = print p2
let _ = print_endline
let _ = print_endline "------------------- P3 ---------------"
let _ = print p3

(*
open Pretty_expressive

let cf = Printer.default_cost_factory ~page_width:80 ()
module P = Printer.Make (val cf)
open P
 *)
 *)
