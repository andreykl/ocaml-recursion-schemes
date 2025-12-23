module Plant = struct
  type 'a plant = 
    Root of 'a 
  | Stalk of 'a
  | Fork of ('a * 'a * 'a)
  | Bloom
  [@@deriving show, eq]

  module Action = struct
    type t = Flower | Upwards | Branch

  type seed = { height : int ; rng : Random.State.t }

  grow ({ height = h ; rng = s } : seed) : action * seed * seed
end
