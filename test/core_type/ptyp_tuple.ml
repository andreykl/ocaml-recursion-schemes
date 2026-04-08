open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'


let%expect_test "Ptyp_tuple: tuple elements are recursively mapped" =
  print_string
    (expand_impl
       {|
    type t = Leaf | Branch of (t * t) [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type t =
      | Leaf
      | Branch of (t * t) [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : t) -> ()
        module RS =
          struct
            module Base =
              struct
                type nonrec 'a t =
                  | Leaf
                  | Branch of ('a * 'a)
                let map f =
                  function
                  | Leaf -> Leaf
                  | Branch (a, b) -> Branch ((f a), (f b))
                let _ = map
              end
            module Project =
              struct
                module Base = Base
                type nonrec t = t
                let project =
                  function
                  | Leaf -> Base.Leaf
                  | Branch (a, b) -> Base.Branch (a, b)
                let _ = project
              end
            module Embed =
              struct
                module Base = Base
                type nonrec t = t
                let embed =
                  function
                  | Base.Leaf -> Leaf
                  | Base.Branch (a, b) -> Branch (a, b)
                let _ = embed
              end
            include (((Recursion_schemes.Make)(Base))(Project))(Embed)
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
