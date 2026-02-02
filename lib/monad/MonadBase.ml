module type MonadBase = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  include MonadBase

  val bind' : 'a t -> 'b t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val sequence : 'a t list -> 'a list t
  val sequence_ : 'a t list -> unit t
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val mapM_ : ('a -> 'b t) -> 'a list -> unit t

  module Syntax : sig
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Make
    (B : MonadBase)
(* : sig *)
(*   include Monad with type 'a t = 'a B.t *)
(* end *) =
struct
  include B

  let bind' ma mb = bind ma (fun _ -> mb)
  let map f m = bind m (fun x -> return (f x))

  let sequence xs =
    List.fold_right
      (fun ma acc -> bind ma (fun x -> bind acc (fun xs -> return (x :: xs))))
      xs (return [])

  let sequence_ xs = List.fold_right bind' xs (return ())
  let mapM f xs = sequence (List.map f xs)
  let mapM_ f xs = sequence_ (List.map f xs)

  module Syntax = struct
    let ( >>= ) = bind
    let ( >> ) = bind'
    let ( <$> ) f m = map f m
    let ( let* ) x f = bind x f
    let ( let+ ) x f = map f x
  end
end
