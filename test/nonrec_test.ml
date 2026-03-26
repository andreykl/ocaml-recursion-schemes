open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "non-recursive type raises error" =
  print_string
    (expand_impl
       {|
    type t = A | B of int [@@deriving recursion_schemes.ppx]
  |});
  [%expect {| |}]
