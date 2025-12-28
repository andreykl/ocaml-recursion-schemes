open Plant_internal_lib.Plant
open Alcotest

let test_height_1 () =
  check (list string) "tree of height 1" [ "^"; "*" ]
    (to_string_list (R.W (F.Root (R.W F.Bloom))))

let test_height_2_simple () =
  check (list string) "tree of height 2 simple" [ "^"; "|"; "*" ]
    (to_string_list (R.W (F.Root (R.W (F.Stalk (R.W F.Bloom))))))

let test_height_2_fork () =
  check (list string) "tree of height 2 fork"
    [ "  ^  "; "+-+-+"; "| * |"; "*   *" ]
    (to_string_list
       (R.W
          (F.Root
             (R.W
                (F.Fork
                   ( R.W (F.Stalk (R.W F.Bloom)),
                     R.W F.Bloom,
                     R.W (F.Stalk (R.W F.Bloom)) ))))))

let suite =
  [
    test_case "Test tree output for height 1" `Quick test_height_1;
    test_case "Test tree output for height 2 (simple)" `Quick
      test_height_2_simple;
    test_case "Test tree output for height 2 (fork)" `Quick test_height_2_fork;
  ]

let () = run "Plant Tests" [ ("Tests for correct tree printing", suite) ]
