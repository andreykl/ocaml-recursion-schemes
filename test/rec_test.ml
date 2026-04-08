open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'


let%expect_test
    "Ptyp_constr: parameterized type constructor (option) recursively mapped" =
  print_string
    (expand_impl
       {|
    type t = Leaf | Node of t option [@@deriving recursion_schemes]
  |});
  [%expect.unreachable]
    [@@expect.uncaught_exn {|
    (* this failure is expected since recursive positions are not supported yet *)     
  |}]
