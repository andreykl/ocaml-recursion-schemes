open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "non-recursive type raises error? no flag - no error.." =
  print_string
    (expand_impl
       {|
    type t = A | B of int [@@deriving recursion_schemes]
  |});
  [%expect {|
    type t =
      | A
      | B of int [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : t) -> ()
        module RS =
          struct
            module Base =
              struct
                type nonrec 'a t =
                  | A
                  | B of int
                let map f = function | A -> A | B a -> B a
                let _ = map
              end
            module Project =
              struct
                module Base = Base
                type nonrec t = t
                let project = function | A -> Base.A | B a -> Base.B a
                let _ = project
              end
            module Embed =
              struct
                module Base = Base
                type nonrec t = t
                let embed = function | Base.A -> A | Base.B a -> B a
                let _ = embed
              end
            include (((Recursion_schemes.Make)(Base))(Project))(Embed)
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
