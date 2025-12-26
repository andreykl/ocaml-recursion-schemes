module Functor = struct
  module type S = Functor.Functor
end

module Schemes = struct
  module type S = Schemes.S

  module Make = Schemes.Make
end
