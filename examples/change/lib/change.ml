type cent = int

module MyNat = struct
  type 'a t = S of 'a | Z

  let map (f : 'a -> 'b) : 'a t -> 'b t = function S a -> S (f a) | Z -> Z
end

module NatR = struct
  include Recursion.Schemes.Make (MyNat)

  let rec expand (n : int) : 'a term =
    if n <= 0 then W MyNat.Z else W (MyNat.S (expand (n - 1)))

  let rec compress (W n : 'a term) : int =
    match n with MyNat.Z -> 0 | MyNat.S wn -> compress wn + 1
end

let coins : cent list = [ 50; 25; 10; 5; 1 ]

let change (amt : cent) : int =
  let rec sub (given : _ NatR.attr MyNat.t) (n : int) : _ NatR.attr MyNat.t =
    match given with
    | MyNat.Z -> given
    | MyNat.S { NatR.hole = h; _ } ->
        if n < 1 then given else if n = 1 then h else sub h (n - 1)
  in
  let get_change ~(coins : cent list) :
      (int * int * (int * int) list) NatR.attr MyNat.t -> int = function
    | MyNat.Z -> 1
    | MyNat.S { NatR.attribute = n, _, xs; _ } ->
        let given = n + 1 in
        let coins = List.filter (fun c -> c <= given) coins in
        let num_coins = List.length coins in
        let xs = List.filter (fun (ncoins, _) -> num_coins = ncoins) xs in
        if List.is_empty xs then 1 else snd @@ List.hd xs
  in
  let go : (int * int * (int * int) list) NatR.cvalgebra = function
    | MyNat.Z -> (0, 1, [])
    | MyNat.S { NatR.attribute = n1, _, _; _ } as curr ->
        let given =
          n1 + 1
          (* our current amount of cents is previous one + 1 *)
        in
        let coins = List.filter (fun c -> c <= given) coins in
        (* 
           `res` is a list of pairs `(n, ws) : (int * int)` where 
           `n` is count of coin types (for list [50 ; 25; 10 ; 5 ; 1] `n` is 5)
           `ws` is count ways to give a change for `curr` (=`given`) amount by
           provided list of coin types
         *)
        let n, res =
          match List.rev coins with
          | [] -> (1, [ (0, 1) ])
          | coins ->
              let n, res, _ =
                List.fold_left
                  (fun (cnt, ways, coins) c ->
                    let coins = c :: coins in
                    let n =
                      if c = 1 then 1
                      else
                        (* 
                      c - 1 is because we need information which is stored at
                      the level above then we actually need 
                    *)
                        get_change ~coins (sub curr (c - 1))
                    in
                    let cnt = cnt + n in
                    (cnt, (List.length coins, cnt) :: ways, coins))
                  (0, [], []) coins
              in
              (n, res)
        in
        (*
           `given` is an amount to change
           `n` is a count ways to give a change (with a fullset of coins)
           `res` is a list with pairs (ncoins, nways) where
             `ncoins` is count of coin types which we have to give a change 
             (i.e. when we have 2 types of coins, namely "one cent", and "5 cents" 
             this value is 2)
             `nways` is count ways to give a change with the amount of coins
         *)
        (given, n, res)
  in
  let _, n, _ = NatR.histo go (NatR.expand amt) in
  n
