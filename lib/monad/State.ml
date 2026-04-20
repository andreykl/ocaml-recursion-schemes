module type State = sig
  type state

  include MonadBase.Monad (* with type 'a t = state -> 'a * state *)

  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val runState : 'a t -> state -> 'a * state
end

module Make (S : sig
  type state
end) : State with type state = S.state = struct
  module M = struct
    type state = S.state
    type 'a t = state -> 'a * state

    let return v s = (v, s)

    let bind m k s =
      let v, s' = m s in
      k v s'
  end

  include M
  include MonadBase.Make (M)

  let get s = (s, s)
  let modify f s = ((), f s)
  let put s _ = ((), s)
  let runState m init = m init
end
