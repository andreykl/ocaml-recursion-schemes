open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test "type ('k,'v) t = Leaf | Node of ('k,'v) t * 'k * 'v * ('k,'v) t: generates MakeRS functor" =
  print_string (expand_impl {|
    type ('k, 'v) t =
      | Leaf
      | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t
    [@@deriving recursion_schemes.ppx]
  |});
  [%expect {|
    type ('k, 'v) t = Leaf | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t
    [@@deriving recursion_schemes.ppx]
    include struct
      module MakeRS (Elem : sig type k type v end) = struct
        module Base = struct
          type 'a t = Leaf | Node of 'a * Elem.k * Elem.v * 'a
          let map f = function
            | Leaf -> Leaf
            | Node (l, k, v, r) -> Node (f l, k, v, f r)
        end
        module Project = struct
          module Base = Base
          type t = (Elem.k, Elem.v) t
          let project = function
            | Leaf -> Base.Leaf
            | Node (l, k, v, r) -> Base.Node (l, k, v, r)
        end
        module Embed = struct
          module Base = Base
          type t = (Elem.k, Elem.v) t
          let embed = function
            | Base.Leaf -> Leaf
            | Base.Node (l, k, v, r) -> Node (l, k, v, r)
        end
        include Recursion_schemes.Make (Base) (Project) (Embed)
      end
    end
  |}]

let%expect_test "type ('k,'v) tree = Leaf | Node of ('k,'v) tree * 'k * 'v * ('k,'v) tree: generates MakeRSTree functor" =
  print_string (expand_impl {|
    type ('k, 'v) tree =
      | Leaf
      | Node of ('k, 'v) tree * 'k * 'v * ('k, 'v) tree
    [@@deriving recursion_schemes.ppx]
  |});
  [%expect {|
    type ('k, 'v) tree = Leaf | Node of ('k, 'v) tree * 'k * 'v * ('k, 'v) tree
    [@@deriving recursion_schemes.ppx]
    include struct
      module MakeRSTree (Elem : sig type k type v end) = struct
        module Base = struct
          type 'a t = Leaf | Node of 'a * Elem.k * Elem.v * 'a
          let map f = function
            | Leaf -> Leaf
            | Node (l, k, v, r) -> Node (f l, k, v, f r)
        end
        module Project = struct
          module Base = Base
          type t = (Elem.k, Elem.v) tree
          let project = function
            | Leaf -> Base.Leaf
            | Node (l, k, v, r) -> Base.Node (l, k, v, r)
        end
        module Embed = struct
          module Base = Base
          type t = (Elem.k, Elem.v) tree
          let embed = function
            | Base.Leaf -> Leaf
            | Base.Node (l, k, v, r) -> Node (l, k, v, r)
        end
        include Recursion_schemes.Make (Base) (Project) (Embed)
      end
    end
  |}]
