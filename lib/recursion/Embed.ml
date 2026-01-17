module type S = sig
  module Base : Functor.S

  type t

  val embed : t Base.t -> t
end
