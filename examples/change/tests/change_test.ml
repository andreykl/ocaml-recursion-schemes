open Change_internal_lib
open Alcotest

let test_change_0 () = check int "trying to change 0 cents" 1 @@ Change.change 0
let test_change_1 () = check int "trying to change 1 cent" 1 @@ Change.change 1
let test_change_2 () = check int "trying to change 2 cents" 1 @@ Change.change 2
let test_change_3 () = check int "trying to change 3 cents" 1 @@ Change.change 3
let test_change_4 () = check int "trying to change 4 cents" 1 @@ Change.change 4
let test_change_5 () = check int "trying to change 5 cents" 2 @@ Change.change 5
let test_change_6 () = check int "trying to change 6 cents" 2 @@ Change.change 6
let test_change_9 () = check int "trying to change 9 cents" 2 @@ Change.change 9

let test_change_10 () =
  check int "trying to change 10 cents" 4 @@ Change.change 10

let test_change_11 () =
  check int "trying to change 11 cents" 4 @@ Change.change 11

let test_change_12 () =
  check int "trying to change 12 cents" 4 @@ Change.change 12

let test_change_13 () =
  check int "trying to change 13 cents" 4 @@ Change.change 13

let test_change_15 () =
  check int "trying to change 15 cents" 6 @@ Change.change 15

let test_change_16 () =
  check int "trying to change 16 cents" 6 @@ Change.change 16

let test_change_20 () =
  check int "trying to change 20 cents" 9 @@ Change.change 20

let test_change_25 () =
  check int "trying to change 25 cents" 13 @@ Change.change 25

let test_change_40 () =
  check int "trying to change 40 cents" 31 @@ Change.change 40

let suite =
  [
    test_case "change 0" `Quick test_change_0;
    test_case "change 1" `Quick test_change_1;
    test_case "change 2" `Quick test_change_2;
    test_case "change 3" `Quick test_change_3;
    test_case "change 4" `Quick test_change_4;
    test_case "change 5" `Quick test_change_5;
    test_case "change 6" `Quick test_change_6;
    test_case "change 9" `Quick test_change_9;
    test_case "change 10" `Quick test_change_10;
    test_case "change 11" `Quick test_change_11;
    test_case "change 12" `Quick test_change_12;
    test_case "change 13" `Quick test_change_13;
    test_case "change 15" `Quick test_change_15;
    test_case "change 16" `Quick test_change_16;
    test_case "change 20" `Quick test_change_20;
    test_case "change 25" `Quick test_change_25;
    test_case "change 40" `Quick test_change_40;
  ]

let () = run "Change Tests" [ ("Tests for changing amounts", suite) ]
