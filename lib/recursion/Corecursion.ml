module Make =
functor
  (F : Functor.S)
  (E : Embed.S with module Base = F)
  ->
  struct
    type 'a coattr = Automatic of 'a | Manual of 'a coattr F.t
    type 'a cvcoalgebra = 'a -> 'a coattr F.t

    let rec ana (f : 'a -> 'a F.t) (a : 'a) : E.t =
      F.map (ana f) @@ f a |> E.embed

    let rec apo (f : 'a -> (E.t, 'a) Either.t F.t) (a : 'a) : E.t =
      let funin = Either.fold ~left:Fun.id ~right:(apo f) in
      F.map funin @@ f a |> E.embed

    let rec futu (f : 'a cvcoalgebra) (a : 'a) : E.t =
      let rec worker ca =
        match ca with
        | Automatic a -> futu f a
        | Manual g -> F.map worker g |> E.embed
      in
      F.map worker @@ f a |> E.embed
  end
