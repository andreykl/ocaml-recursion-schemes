module Monad = struct
  module type S = MonadBase.Monad

  module Make = MonadBase.Make
end

module State = struct
  module type S = State.State

  module Make = State.Make
end
