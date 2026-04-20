open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test
    "type t = Lit of int | Add of t * t | Neg of t: map applies f to recursive \
     positions" =
  print_string
    (expand_impl
       {|
    type t = Lit of int | Add of t * t | Neg of t [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type t =
      | Lit of int
      | Add of t * t
      | Neg of t [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : t) -> ()
        module RS =
          struct
            module Base =
              struct
                type nonrec 'a t =
                  | Lit of int
                  | Add of 'a * 'a
                  | Neg of 'a
                let map f =
                  function
                  | Lit a -> Lit a
                  | Add (a, b) -> Add ((f a), (f b))
                  | Neg a -> Neg (f a)
                let _ = map
              end
            module Project =
              struct
                module Base = Base
                type nonrec t = t
                let project =
                  function
                  | Lit a -> Base.Lit a
                  | Add (a, b) -> Base.Add (a, b)
                  | Neg a -> Base.Neg a
                let _ = project
              end
            module Embed =
              struct
                module Base = Base
                type nonrec t = t
                let embed =
                  function
                  | Base.Lit a -> Lit a
                  | Base.Add (a, b) -> Add (a, b)
                  | Base.Neg a -> Neg a
                let _ = embed
              end
            include (((Recursion_schemes.Make)(Base))(Project))(Embed)
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
