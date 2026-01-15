module type Project = sig
  module Base : Functor.Functor

  type t

  val project : t -> t Base.t
end

module type Embed = sig
  module Base : Functor.Functor

  type t

  val embed : t Base.t -> t
end

module type S = functor (F : Functor.Functor) -> sig
  module type Recursion = functor (P : Project) -> sig
    type 'a algebra = 'a F.t -> 'a

    val cata : 'a algebra -> P.t -> 'a
  end

  (*
  type 'a term = W of 'a term F.t
  type 'a algebra = 'a F.t -> 'a
  type ('b, 'a) ralgebra = 'b term -> 'a F.t -> 'a

  type 'a attr = { attribute : 'a; hole : 'a attr F.t }
  type 'a cvalgebra = 'a attr F.t -> 'a
  type 'a coattr = Automatic of 'a | Manual of 'a coattr F.t
  type 'a cvcoalgebra = 'a -> 'a coattr F.t

  val cata : 'a algebra -> 'b term -> 'a
  val para : ('b, 'a) ralgebra -> 'b term -> 'a

  val histo : 'a cvalgebra -> 'b term -> 'a
  val histo' : 'a cvalgebra -> 'b term -> 'a attr
  val apo : ('a -> ('b term, 'a) Either.t F.t) -> 'a -> 'b term
  val futu : 'a cvcoalgebra -> 'a -> 'b term
 *)
end

module Recursion =
functor
  (F : Functor.Functor)
  (P : Project with module Base = F)
  ->
  struct
    type 'a algebra = 'a F.t -> 'a
    type 'a ralgebra = P.t F.t -> 'a F.t -> 'a
    type 'a attr = { attribute : 'a; hole : 'a attr F.t }
    type 'a cvalgebra = 'a attr F.t -> 'a

    let rec cata (alg : 'a algebra) (x : P.t) : 'a =
      alg @@ F.map (cata alg) @@ P.project x

    let rec para (alg : 'a ralgebra) (x : P.t) : 'a =
      alg (P.project x) @@ F.map (para alg) @@ P.project x

    let histo (f : 'a cvalgebra) (x : P.t) : 'a =
      let rec worker x =
        F.map worker @@ P.project x |> fun res ->
        { attribute = f res; hole = res }
      in
      (worker x).attribute

    let histo' (f : 'a cvalgebra) (x : P.t) : 'a attr =
      let rec worker x =
        F.map worker @@ P.project x |> fun res ->
        { attribute = f res; hole = res }
      in
      worker x

    [@@@ocaml.warning "-32"]

    let rec para' f x : 'a =
      let funout t = (t, para' f t) in
      F.map funout @@ P.project x |> f

    let cata' f x = para' (Fun.const f) x

    let rec histo'' (f : 'a cvalgebra) (x : P.t) : 'a =
      let rec worker x =
        { attribute = histo'' f x; hole = F.map worker @@ P.project x }
      in
      F.map worker @@ P.project x |> f

    let cata'' (f : 'a F.t -> 'a) (t : P.t) : 'a =
      histo (fun aaf -> F.map (fun { attribute = a; _ } -> a) aaf |> f) t
  end

module Corecursion =
functor
  (F : Functor.Functor)
  (E : Embed with module Base = F)
  ->
  struct
    type 'a coattr = Automatic of 'a | Manual of 'a coattr F.t
    type 'a cvcoalgebra = 'a -> 'a coattr F.t

    let rec ana (f : 'a -> 'a F.t) (a : 'a) : E.t =
      f a |> F.map (ana f) |> E.embed

    let rec apo (f : 'a -> (E.t, 'a) Either.t F.t) (a : 'a) : E.t =
      let funin = Either.fold ~left:Fun.id ~right:(apo f) in
      f a |> F.map funin |> E.embed

    let rec futu (f : 'a cvcoalgebra) (a : 'a) : E.t =
      let rec worker ca =
        match ca with
        | Automatic a -> futu f a
        | Manual g -> F.map worker g |> E.embed
      in
      f a |> F.map worker |> E.embed
  end
