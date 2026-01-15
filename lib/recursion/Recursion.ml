module Functor = struct
  module type S = Functor.Functor
end

module Rec = struct
  (* module type S = Schemes.S *)

  module Make = Schemes.Recursion
end

module CoRec = struct
  
  module Make = Schemes2.Corecursion
end
