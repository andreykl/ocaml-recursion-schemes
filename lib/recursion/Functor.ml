module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  (*  [@@deriving show, eq] *)
end
