open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "type 'a t = Nil | Cons of 'a * 'a t: generates MakeRS functor"
    =
  print_string
    (expand_impl
       {|
    type 'a t = Nil | Cons of 'a * 'a t [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type 'a t =
      | Nil
      | Cons of 'a * 'a t [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : 'a t) -> ()
        module Make_RS(Elem:sig type nonrec a end) =
          struct
            module Base =
              struct
                type nonrec 'b t =
                  | Nil
                  | Cons of Elem.a * 'b
                let map f =
                  function | Nil -> Nil | Cons (a, b) -> Cons (a, (f b))
                let _ = map
              end
            module Project =
              struct
                module Base = Base
                type nonrec t = Elem.a t
                let project =
                  function | Nil -> Base.Nil | Cons (a, b) -> Base.Cons (a, b)
                let _ = project
              end
            module Embed =
              struct
                module Base = Base
                type nonrec t = Elem.a t
                let embed =
                  function | Base.Nil -> Nil | Base.Cons (a, b) -> Cons (a, b)
                let _ = embed
              end
            include (((Recursion_schemes.Make)(Base))(Project))(Embed)
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
