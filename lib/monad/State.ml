module type State = sig
  type state

  include MonadBase.Monad (* with type 'a t = state -> state * 'a *)

  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val runState : 'a t -> state -> state * 'a
end

module Make (S : sig
  type state
end) : State with type state = S.state = struct
  module M = struct
    type state = S.state
    type 'a t = state -> state * 'a

    let return v s = (s, v)

    let bind m k s =
      let s', v = m s in
      k v s'
  end

  include M
  include MonadBase.Make (M)

  let get s = (s, s)
  let modify f s = (f s, ())
  let put s _ = (s, ())
  let runState m init = m init
end
