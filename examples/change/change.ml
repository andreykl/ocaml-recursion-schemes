module Change = Change_internal_lib.Change

let main () =
  (* let _ = Printf.printf "amount for 0 is %i\n" (Change.change 0) in *)
  (* let _ = *)
  Printf.printf "amount for 16 is %i\n" (Change.change 16)
(* in *)

(* let _ = Printf.printf "amount for 2 is %i\n" (Change.change 2) in *)
(* let _ = Printf.printf "amount for 3 is %i\n" (Change.change 3) in *)
(* Printf.printf "amount for 4 is %i\n" (Change.change 4) *)

let () = main ()
