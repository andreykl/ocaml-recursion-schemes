module type S = sig
  module Base : Functor.S

  type t

  val project : t -> t Base.t
end
