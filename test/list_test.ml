open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "type 'a t = Nil | Cons of 'a * 'a t: generates MakeRS functor" =
  print_string (expand_impl {|
    type 'a t = Nil | Cons of 'a * 'a t [@@deriving recursion_schemes]
  |});
  [%expect {|
    type 'a t = Nil | Cons of 'a * 'a t [@@deriving recursion_schemes]
    include struct
      module MakeRS (Elem : sig type a end) = struct
        module Base = struct
          type 'b t = Nil | Cons of Elem.a * 'b
          let map f = function
            | Nil -> Nil
            | Cons (e, tl) -> Cons (e, f tl)
        end
        module Project = struct
          module Base = Base
          type t = Elem.a t
          let project = function
            | Nil -> Base.Nil
            | Cons (x, xs) -> Base.Cons (x, xs)
        end
        module Embed = struct
          module Base = Base
          type t = Elem.a t
          let embed = function
            | Base.Nil -> Nil
            | Base.Cons (x, xs) -> Cons (x, xs)
        end
        include Recursion_schemes.Make (Base) (Project) (Embed)
      end
    end
  |}]

let%expect_test "type 'a mylist = Nil | Cons of 'a * 'a mylist: generates MakeRSMylist functor" =
  print_string (expand_impl {|
    type 'a mylist = Nil | Cons of 'a * 'a mylist [@@deriving recursion_schemes]
  |});
  [%expect {|
    type 'a mylist = Nil | Cons of 'a * 'a mylist [@@deriving recursion_schemes]
    include struct
      module MakeRSMylist (Elem : sig type a end) = struct
        module Base = struct
          type 'b t = Nil | Cons of Elem.a * 'b
          let map f = function
            | Nil -> Nil
            | Cons (e, tl) -> Cons (e, f tl)
        end
        module Project = struct
          module Base = Base
          type t = Elem.a mylist
          let project = function
            | Nil -> Base.Nil
            | Cons (x, xs) -> Base.Cons (x, xs)
        end
        module Embed = struct
          module Base = Base
          type t = Elem.a mylist
          let embed = function
            | Base.Nil -> Nil
            | Base.Cons (x, xs) -> Cons (x, xs)
        end
        include Recursion_schemes.Make (Base) (Project) (Embed)
      end
    end
  |}]
