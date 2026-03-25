open Ppxlib

let expand_impl (src : string) : string =
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf "test.ml";
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in
  Format.asprintf "%a" Pprintast.structure ast'

(* --- Кейс 1: Ptyp_var — рекурсивная позиция (param_name), f применяется --- *)
let%expect_test "Ptyp_var: recursive position gets f applied" =
  print_string
    (expand_impl {|
    type t = Z | S of t [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type t =
      | Z
      | S of t [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : t) -> ()
        module Base =
          struct
            type nonrec 'a t =
              | Z
              | S of 'a
            let map f = function | Z -> Z | S a -> S (f a)
            let _ = map
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]

(* --- Кейс 1b: Ptyp_var — нерекурсивный параметр типа, f НЕ применяется --- *)
let%expect_test "Ptyp_var: non-recursive type param is passed through" =
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
        module Base =
          struct
            type nonrec ('b, 'a) t =
              | Nil
              | Cons of 'a * 'b
            let map f = function | Nil -> Nil | Cons (a, b) -> Cons (a, (f b))
            let _ = map
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]

(* --- Кейс 2: Ptyp_tuple — кортежный тип рекурсивно обходится --- *)
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
        module Base =
          struct
            type nonrec 'a t =
              | Leaf
              | Branch of (t * t)
            let map f = function | Leaf -> Leaf | Branch (t, t) -> Branch (t, t)
            let _ = map
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]

(* --- Кейс 3: Ptyp_constr без аргументов (int) — fallback через пустой args --- *)
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
              | WithVal (int) -> WithVal int
            let _ = map
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]

(* --- Кейс 3b: Ptyp_constr с аргументами (t option) — рекурсивная обработка args --- *)
let%expect_test
    "Ptyp_constr: parameterized type constructor (option) recursively mapped" =
  print_string
    (expand_impl
       {|
    type t = Leaf | Node of t option [@@deriving recursion_schemes]
  |});
  [%expect
    {|
    type t =
      | Leaf
      | Node of t option [@@deriving recursion_schemes]
    include
      struct
        [@@@ocaml.warning "-60"]
        let _ = fun (_ : t) -> ()
        module Base =
          struct
            type nonrec 'a t =
              | Leaf
              | Node of t option
            let map f =
              function | Leaf -> Leaf | Node (option (t)) -> Node (option t)
            let _ = map
          end
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    |}]
