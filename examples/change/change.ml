module Change = Change_internal_lib.Change

let main () = Printf.printf "amount for 16 is %i\n" (Change.change 16)
let () = main ()
