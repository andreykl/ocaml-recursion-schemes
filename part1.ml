open Core

type lit
  = StrLit of string
  | IntLit of int
  | Ident of string
  [@@deriving show, eq]

type expr
  = Index of expr * expr
  | Call of expr * expr list
  | Unary of string * expr
  | Binary of expr * string * expr
  | Paren of expr
  | Literal of lit
  [@@deriving show, eq]

let rec flatten = function
  | Literal l -> Literal l
  | Paren e -> flatten e
  | Index (e, i) -> Index (flatten e, flatten i)
  | Call (e, args) -> Call (flatten e, List.map ~f:flatten args)
  | Unary (s, e) -> Unary (s, flatten e)
  | Binary (e1, s, e2) -> Binary (flatten e1, s, flatten e2)

let applyExpr f = function
  | Literal l -> Literal l
  | Paren e -> Paren (f e)
  | Index (e, i) -> Index (f e, f i)
  | Call (e, args) -> Call (f e, List.map ~f args)
  | Unary (s, e) -> Unary (s, f e)
  | Binary (e1, s, e2) -> Binary (flatten e1, s, flatten e2)

let rec flatten' = function
  | Paren e -> flatten' e
  | e -> applyExpr flatten' e

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module ExprF : FUNCTOR = struct
  type 'a t
    = IndexF of 'a * 'a
    | CallF of 'a * 'a list
    | UnaryF of string * 'a
    | BinaryF of 'a * string * 'a
    | ParenF of 'a
    | LiteralF of lit
  [@@deriving map, show, eq]
end
(*
module ExprFFunctor : FUNCTOR with type 'a t = 'a ExprF.t = struct
  type 'a t = 'a ExprF.t
  let map = ExprF.map
(*  let map f = function
    | IndexF (e, i) -> IndexF (f e, f i)
    | CallF (e, args) -> CallF (f e, List.map ~f args)
    | UnaryF (s, e) -> UnaryF (s, f e)
    | BinaryF (l, s, r) -> BinaryF (f l, s, f r)
    | ParenF e -> ParenF (f e)
    | LiteralF l -> LiteralF l
 *)
end
 *)
                           (* type 'a term = In of 'a term 'a *)
