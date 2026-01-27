open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "deriving ppx_nat adds nat right after the type" =
  print_string (expand_impl "type t = int [@@deriving base_functor]\n");
  [%expect
    {|
    type t = int[@@deriving base_functor]
    include struct let _ = fun (_ : t) -> ()
                   type nat =
                     | Z
                     | S of nat  end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]

let%expect_test "no deriving -> no nat generated" =
  print_string (expand_impl "type t = int\n");
  [%expect {|
    type t = int |}]

let%expect_test "two usages -> nat generated twice (would clash if compiled)" =
  print_string
    (expand_impl
       {|
        type a = int [@@deriving base_functor]
        type b = string [@@deriving base_functor]
       |});
  [%expect
    {|
    type a = int[@@deriving base_functor]
    include struct let _ = fun (_ : a) -> ()
                   type nat =
                     | Z
                     | S of nat  end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type b = string[@@deriving base_functor]
    include struct let _ = fun (_ : b) -> ()
                   type nat =
                     | Z
                     | S of nat  end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
