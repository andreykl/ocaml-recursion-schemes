open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "Ptyp_constr: nullary type constructor (int) passed through" =
  print_string
    (expand_impl
       {|
    type t = Zero | Succ of t | WithVal of int [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type t =
      | Zero
      | Succ of t
      | WithVal of int [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : t) -> ()
        module RS =
          struct
            module Base =
              struct
                type nonrec 'a t =
                  | Zero
                  | Succ of 'a
                  | WithVal of int
                let map f =
                  function
                  | Zero -> Zero
                  | Succ a -> Succ (f a)
                  | WithVal a -> WithVal a
                let _ = map
              end
            module Project =
              struct
                module Base = Base
                type nonrec t = t
                let project =
                  function
                  | Zero -> Base.Zero
                  | Succ a -> Base.Succ a
                  | WithVal a -> Base.WithVal a
                let _ = project
              end
            module Embed =
              struct
                module Base = Base
                type nonrec t = t
                let embed =
                  function
                  | Base.Zero -> Zero
                  | Base.Succ a -> Succ a
                  | Base.WithVal a -> WithVal a
                let _ = embed
              end
            include (((Recursion_schemes.Make)(Base))(Project))(Embed)
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
