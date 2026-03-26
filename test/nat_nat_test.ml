open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

let%expect_test
    "type nat = Zero | Succ of nat: generates BaseNat, ProjectNat, EmbedNat, \
     module RSNat" =
  print_string
    (expand_impl
       {|
    type nat = Zero | Succ of nat [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type nat = Zero | Succ of nat [@@deriving recursion_schemes]
    include struct
      module BaseNat = struct
        type 'a t = Zero | Succ of 'a
        let map f = function Zero -> Zero | Succ a -> Succ (f a)
      end
      module ProjectNat = struct
        module Base = BaseNat
        type t = nat
        let project = function Zero -> BaseNat.Zero | Succ a -> BaseNat.Succ a
      end
      module EmbedNat = struct
        module Base = BaseNat
        type t = nat
        let embed = function BaseNat.Zero -> Zero | BaseNat.Succ a -> Succ a
      end
      module RSNat = Recursion_schemes.Make (BaseNat) (ProjectNat) (EmbedNat)
    end
  |}]
