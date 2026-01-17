type cent = int

module RS = Recursion_schemes.Schemes

module NatR = struct
  module NatF = struct
    type 'a t = S of 'a | Z

    let map (f : 'a -> 'b) : 'a t -> 'b t = function S a -> S (f a) | Z -> Z
  end

  include NatF

  module Nat = struct
    module Base = NatF

    type t = S of t | Z

    let project = function Z -> Base.Z | S n -> Base.S n
  end

  include RS.Recursion.Make (NatF) (Nat)

  let rec expand (n : int) : Nat.t = if n <= 0 then Z else S (expand (n - 1))
end

let coins : cent list = [ 50; 25; 10; 5; 1 ]

(** Subtract [n] levels from [given] in the cached structure.

    This function navigates [n] levels deep into the [NatR.hole] field of the
    given attribute structure. It's used to read cached data from a specific
    depth level.

    @param given The starting attribute structure of type [_ NatR.attr NatR.t]
    @param n The number of levels to descend (must be non-negative)
    @return
      The attribute structure [n] levels deeper than [given]. If [given] is
      [NatR.Z], returns [NatR.Z] unchanged. If [n ≤ 0], returns [given]
      unchanged. *)
let rec sub (given : _ NatR.attr NatR.t) (n : int) : _ NatR.attr NatR.NatF.t =
  match given with
  | NatR.Z -> given
  | NatR.S { NatR.hole = h; _ } ->
      if n < 1 then given else if n = 1 then h else sub h (n - 1)

(** Extract cached change-making result from the attribute structure.

    This function reads precomputed results stored in a [NatR.attr] structure.
    The cache contains tuples [(amount, num_ways, ways)] where:
    - [amount] is the monetary amount in cents
    - [num_ways] is the number of ways to give a change for that amount
    - [ways] is a list of [(num_coins_used, num_ways)] pairs

    @param coins Available coin denominations in cents
    @param attr The cached attribute structure
    @return
      Number of ways to make change for the amount in [attr], or 1 if no cached
      result matches the available coins *)
let get_change ~(coins : cent list) :
    (int * int * (int * int) list) NatR.attr NatR.t -> int = function
  | NatR.Z -> 1
  | NatR.S { NatR.attribute = n, _, xs; _ } ->
      let given = n + 1 in
      let coins = List.filter (fun c -> c <= given) coins in
      let num_coins = List.length coins in
      let xs = List.filter (fun (ncoins, _) -> num_coins = ncoins) xs in
      if List.is_empty xs then 1 else snd @@ List.hd xs

(** Count the number of ways to give change for a given amount in cents.

    @param amt Amount to give change for, in cents
    @return The number of ways to give change *)
let change (amt : cent) : int =
  let go : (int * int * (int * int) list) NatR.cvalgebra = function
    | NatR.Z -> (0, 1, [])
    | NatR.S { NatR.attribute = n1, _, _; _ } as curr ->
        (* Our current amount in cents is the previous amount plus 1 *)
        let given = n1 + 1 in
        let coins = List.filter (fun c -> c <= given) coins in
        (* 
           [res] is a list of pairs [(n, ws) : (int * int)] where 
           [n] is the count of coin types (for the list [[50; 25; 10; 5; 1]], [n] is 5)
           [ws] is the count of ways to give change for the [curr] amount (=[given]) 
           using the provided list of coin types
         *)
        let n, res =
          match coins with
          | [] -> (1, [ (0, 1) ])
          | coins ->
              let module ListF = RS.Predef.List.Make (struct
                type t = cent
              end) in
              let go = function
                | ListF.F.Nil -> (0, [], [])
                | ListF.F.Cons (c, (cnt, ways, coins)) ->
                    let coins = c :: coins in
                    let n =
                      if c = 1 then 1
                      else
                        (* 
                         c - 1 is used because we need information stored at
                         the level above what we actually need
                       *)
                        get_change ~coins (sub curr (c - 1))
                    in
                    let cnt = cnt + n in
                    (cnt, (List.length coins, cnt) :: ways, coins)
              in
              let n, res, _ = ListF.R.cata go coins in
              (n, res)
        in
        (*
           [given] is the amount to change
           [n] is the count of ways to give change (with the full set of coins)
           [res] is a list of pairs [(ncoins, nways)] where:
             [ncoins] is the count of coin types available to give change 
             (i.e., when we have 2 types of coins, namely "one cent" and "five cents",
             this value is 2)
             [nways] is the count of ways to give change with that number of coin types
         *)
        (given, n, res)
  in
  let _, n, _ = NatR.histo go (NatR.expand amt) in
  n
