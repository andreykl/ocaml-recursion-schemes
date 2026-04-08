open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test
    "type ('k,'v) t = Leaf | Node of ('k,'v) t * 'k * 'v * ('k,'v) t: \
     generates MakeRS functor" =
  print_string
    (expand_impl
       {|
    type ('k, 'v) t =
      | Leaf
      | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t
    [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type ('k, 'v) t =
      | Leaf
      | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : ('k, 'v) t) -> ()
        module Make_RS(Elem:sig type nonrec k
                                and v end) =
          struct
            module Base =
              struct
                type nonrec 'a t =
                  | Leaf
                  | Node of 'a * Elem.k * Elem.v * 'a
                let map f =
                  function
                  | Leaf -> Leaf
                  | Node (a, b, c, d) -> Node ((f a), b, c, (f d))
                let _ = map
              end
            module Project =
              struct
                module Base = Base
                type nonrec t = (Elem.k, Elem.v) t
                let project =
                  function
                  | Leaf -> Base.Leaf
                  | Node (a, b, c, d) -> Base.Node (a, b, c, d)
                let _ = project
              end
            module Embed =
              struct
                module Base = Base
                type nonrec t = (Elem.k, Elem.v) t
                let embed =
                  function
                  | Base.Leaf -> Leaf
                  | Base.Node (a, b, c, d) -> Node (a, b, c, d)
                let _ = embed
              end
            include (((Recursion_schemes.Make)(Base))(Project))(Embed)
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
