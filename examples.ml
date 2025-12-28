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

module Change = struct
  type cent = int

  module type MyNatS = sig
    type 'a t = S of 'a | Z

    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module MyNat : MyNatS = struct
    type 'a t = S of 'a | Z

    let map (f : 'a -> 'b) : 'a t -> 'b t = function S a -> S (f a) | Z -> Z
  end

  module NatR = struct
    include Recursion.Schemes.Make (MyNat)

    let rec expand (n : int) : 'a term =
      if n <= 0 then W MyNat.Z else W (MyNat.S (expand (n - 1)))

    let rec compress (W n : 'a term) =
      match n with MyNat.Z -> 0 | MyNat.S wn -> compress wn + 1
  end

  let coins : cent list = [ 50; 25; 10; 5; 1 ]
  (*
  let change (amt : cent) : int =
    let rec take_cnt_minus (n : int) : int NatR.attr MyNat.t -> int = function
      | MyNat.Z -> 0
      | MyNat.S {NatR.hole = hole; NatR.attribute = cnt} ->
         if n <= 1 then cnt else take_cnt_minus (n - 1) hole 
    in
  
    let go = function
      | MyNat.Z -> 1
      | MyNat.S {NatR.hole = h; NatR.attr = cnt1} as curr ->
         let given = NatR.compress curr in
         List.fold_left (fun acc coin -> acc + if n < coin then 0 else take_cnt_minus c curr) 0 coins 
    in

    NatR.histo' go amt 
 *)
end

let main =
  let fld = ExprR.histo f t in
  Format.printf "%d\n" fld

(* val fib4 : int MyListR.attr = *)
(*   {MyListR.attribute = 2; *)
(*    hole = List.(::) *)
(*             ({MyListR.attribute = 4; *)
(*               hole = List.(::) *)
(*                        ({MyListR.attribute = 2; *)
(*                          hole = *)
(*                            List.(::) ({MyListR.attribute = 1; hole = List.[]}, *)
(*                                       [{MyListR.attribute = 1; hole = List.[]}])}, *)
(*                         [{MyListR.attribute = 2; *)
(*                           hole = *)
(*                             List.(::) ({MyListR.attribute = 1; hole = List.[]}, *)
(*                                        [{MyListR.attribute = 1; hole = List.[]}])}])}, *)
(*              [])} *)

(*
ExprR.W (Binary
   (ExprR.W (Binary 
       (ExprR.W (Literal "1"), "+", ExprR.W (Literal "2"))
   ), "+", 
   ExprR.W (Literal "3")))

F.map worker x
Binary
  worker
    (ExprR.W (Binary 
      (ExprR.W (Literal "1"), "+", ExprR.W (Literal "2"))
     ), "+", 
  worker
    ExprR.W (Literal "3"))

F.map worker x
* Binary
   ** worker
    (ExprR.W (Binary 
      (ExprR.W (Literal "1"), "+", ExprR.W (Literal "2"))
       ), "+",
     ***
       {
         attribute = histo f (ExprR.W (Binary (ExprR.W (Literal "1"),
                                               "+",
                                               ExprR.W (Literal "2"))))
         *****
           attribute = F.map worker (Binary (ExprR.W (Literal "1"),
                                               "+",
                                               ExprR.W (Literal "2"))) |> f
           attribute = Binary (worker ExprR.W (Literal "1")) "+" (worker (ExprR.W (Literal 2))) |> f
           attribute = Binary { 
                           attribute = 1; 
                           hole = Literal "1" 
                         } "+"
                         {
                           attribute = 1;
                           hole = Literal 2
                         }
         hole = F.map worker Binary (ExprR.W (Literal "1"),
                                               "+",
                                               ExprR.W (Literal "2"))
         hole = Binary (worker (ExprR.W (Literal "1", "+", worker ExprR.W (Literal "2"))))
         hole = Binary { attribute = 1;
                         hole = Literal "1"; } + 
                  { attribute = 1; hole = Literal "2" }
       }
     ****
   ** worker
    ExprR.W (Literal "3"))
    *** 
    { attribute = histo f (ExprR.W Literal "3");
      hole = F.map worker (Literal "3");
    }
      { attribute = 1;
        hole = Literal 3 }

 *)
(*
let main =
  let pp_s fmt s = Format.fprintf fmt "%s" s in
  let str = show_tree pp_s t in
  Format.printf "tree: %s\n" str
 *)

(*
let rec pp_tree' fmt = function
  | Literal s -> Format.fprintf fmt "%s" s
  | Binary (lhs, s, rhs) -> Format.fprintf fmt "%s %s %s" (pp_tree' fmt lhs) s (pp_tree' fmt rhs)
 *)
(*
let rec show_tree' show_value = function
  | Literal s -> "Literal " ^ show_value s
  | Binary (lhs, s, rhs) -> "Binary (" ^ (show_tree' show_value lhs) ^ ", " ^ s ^ ", " ^ (show_tree' show_value rhs) ^ ")"
 *)
(*  let main =   Format.printf "tree: %s\n" (show_tree' Fun.id t); *)
