module Functor = struct
  module type S = Functor.S
end

module Recursion = struct
  (* module type S = Schemes.S *)

  module Make = Recursion.Make
end

module Corecursion = struct
  module Make = Corecursion.Make
end

module Project = struct
  module type S = Project.S
end

module Embed = struct
  module type S = Embed.S
end
