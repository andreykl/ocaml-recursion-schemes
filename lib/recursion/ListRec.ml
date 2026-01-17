module Make (Elem : sig
  type t
end) =
struct
  module F = struct
    type 'a t = Cons of Elem.t * 'a | Nil

    let map f = function Nil -> Nil | Cons (e, b) -> Cons (e, f b)
  end

  module Project = struct
    module Base = F

    type t = Elem.t List.t

    let project = function x :: xs -> F.Cons (x, xs) | [] -> F.Nil
  end

  module Embed = struct
    module Base = F

    type t = Elem.t List.t

    let embed = function F.Cons (x, xs) -> x :: xs | F.Nil -> []
  end

  module R = Recursion.Make (F) (Project)
  module CR = Corecursion.Make (F) (Embed)
end
