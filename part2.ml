module Expr = struct
  type 'a t =
    | Literal of { intVal : int }
    | Ident of { name : string }
    | Index of { target : 'a; idx : 'a }
    | Unary of { op : string; target : 'a }
    | Binary of { lhs : 'a; op : string; rhs : 'a }
    | Call of { func : 'a; args : 'a list }
    | Paren of { target : 'a }
  [@@deriving map, show, eq]

  (*
    let rec map f = function
    | Literal _ | Ident _ as v -> v
    | Index { target; idx } -> Index { target = f target; idx = f idx }
    | Unary { op; target } -> Unary { op; target = f target }
    | Binary { lhs; op; rhs } -> Binary { lhs = f lhs; op; rhs = f rhs }
    | Call 
   *)

  (* type 'a term = In of ('a term as term) *)

  let rec cata (f : 'a t -> 'a) (x : 'b t as 'b) = map (cata f) x |> f
  let rec ana (f : 'a -> 'a t) (x : 'b) = f x |> map (ana f)
end

(* let rec lcata (f : 'a list -> 'a) (x : 'b list) = List.map (lcata f) x |> f *)

let depth : 'a Expr.t -> int =
  Expr.cata (function
    | Literal _ | Ident _ -> 1
    | Index { target; idx } -> target + idx + 1
    | Unary { target } -> target + 1
    | Binary { lhs; rhs } -> lhs + rhs + 1
    | Call { func; args } ->
        func + Core.List.fold_left ~f:(fun a v -> a + v) ~init:0 args + 1
    | Paren { target } -> target + 1)
