open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "type t = Lit of int | Add of t * t | Neg of t: map applies f to recursive positions" =
  print_string (expand_impl {|
    type t = Lit of int | Add of t * t | Neg of t [@@deriving base_functor]
  |});
  [%expect {|
    type t = Lit of int | Add of t * t | Neg of t [@@deriving base_functor]
    include struct
      module Base = struct
        type 'a t = Lit of int | Add of 'a * 'a | Neg of 'a
        let map f = function
          | Lit i -> Lit i
          | Add (l, r) -> Add (f l, f r)
          | Neg a -> Neg (f a)
      end
      module Project = struct
        module Base = Base
        type t = t
        let project = function
          | Lit i -> Base.Lit i
          | Add (l, r) -> Base.Add (l, r)
          | Neg a -> Base.Neg a
      end
      module Embed = struct
        module Base = Base
        type t = t
        let embed = function
          | Base.Lit i -> Lit i
          | Base.Add (l, r) -> Add (l, r)
          | Base.Neg a -> Neg a
      end
      include Recursion_schemes.Make (Base) (Project) (Embed)
    end
  |}]
