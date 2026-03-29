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
    type nat =
      | Zero
      | Succ of nat [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : nat) -> ()
        module BaseNat =
          struct
            type nonrec 'a t =
              | Zero
              | Succ of 'a
            let map f = function | Zero -> Zero | Succ a -> Succ (f a)
            let _ = map
          end
        module ProjectNat =
          struct
            module Base = BaseNat
            type nonrec t = nat
            let project =
              function | Zero -> BaseNat.Zero | Succ a -> BaseNat.Succ a
            let _ = project
          end
        module EmbedNat =
          struct
            module Base = BaseNat
            type nonrec t = nat
            let embed =
              function | BaseNat.Zero -> Zero | BaseNat.Succ a -> Succ a
            let _ = embed
          end
        module RSNat =
          (((Recursion_schemes.Make)(BaseNat))(ProjectNat))(EmbedNat)
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
