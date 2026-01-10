module Expr = struct
  type 'a t = Literal of string | Binary of 'a * string * 'a
  [@@deriving show, eq]

  let map f = function
    | Literal _ as l -> l
    | Binary (lhs, s, rhs) -> Binary (f lhs, s, f rhs)
end

module ExprR = Recursion.Schemes.Make (Expr)

let f : int ExprR.attr Expr.t -> int = function
  | Expr.Literal _ -> 1
  | Expr.Binary (lhs, _, rhs) -> max lhs.attribute rhs.attribute + 1

let t =
  ExprR.W
    (Binary
       ( ExprR.W (Binary (ExprR.W (Literal "1"), "+", ExprR.W (Literal "2"))),
         "+",
         ExprR.W (Literal "3") ))

module MyListR = Recursion.Schemes.Make (List)

let fib (s : int MyListR.attr list) : int =
  let open List in
  match s with
  | [] -> 1
  | [ x ] -> (
      match x.hole with
      | [] -> 1
      | [ x' ] -> x.attribute + x'.attribute
      | _ -> 0)
  | _ -> 0

let rec gen (n : int) : int MyListR.term =
  if n <= 0 then MyListR.W [] else MyListR.W [ gen (n - 1) ]

let fib1 = MyListR.histo' fib (MyListR.W [])
let fib2 = MyListR.histo' fib (MyListR.W [ MyListR.W [] ])
let fib3 = MyListR.histo' fib (MyListR.W [ MyListR.W [ MyListR.W [] ] ])

let fib4 =
  MyListR.histo' fib (MyListR.W [ MyListR.W [ MyListR.W [ MyListR.W [] ] ] ])

let fib5 =
  MyListR.histo' fib
    (MyListR.W [ MyListR.W [ MyListR.W [ MyListR.W [ MyListR.W [] ] ] ] ])

let fib6 =
  MyListR.histo' fib
    (MyListR.W
       [ MyListR.W [ MyListR.W [ MyListR.W [ MyListR.W [ MyListR.W [] ] ] ] ] ])

(*
1 1 2 3 5
 *)
let fib' : int ExprR.attr Expr.t -> int = function
  | Literal _ -> 1
  | Binary (lhs, _, rhs) -> lhs.attribute + rhs.attribute

let t'1 = ExprR.W (Literal "1")
let fib'1 = ExprR.histo fib' t'1
let t'2 = ExprR.W (Binary (t'1, "_", t'1))
let fib'2 = ExprR.histo fib' t'2

let fib4 =
  MyListR.histo' fib
    (MyListR.W
       [
         MyListR.W
           [
             MyListR.W [ MyListR.W []; MyListR.W [] ];
             MyListR.W [ MyListR.W []; MyListR.W [] ];
           ];
       ])

let main =
  let fld = ExprR.histo f t in
  Format.printf "%d\n" fld
