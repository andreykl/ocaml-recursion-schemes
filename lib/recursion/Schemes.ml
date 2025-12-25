module Schemes (F : Functor.Functor) = struct
  type 'a term = W of 'a term F.t
  type 'a algebra = 'a F.t -> 'a
  type ('b, 'a) ralgebra = 'b term -> 'a F.t -> 'a
  type 'a attr = { attribute : 'a; hole : 'a attr F.t }
  type 'a cvalgebra = 'a attr F.t -> 'a
  type 'a coattr = Automatic of 'a | Manual of 'a coattr F.t
  type 'a cvcoalgebra = 'a -> 'a coattr F.t

  let rec cata (f : 'a algebra) (W x) : 'a = F.map (cata f) x |> f

  let rec para f (W x) : 'a =
    let funout t = (t, para f t) in
    F.map funout x |> f

  let rec para' (f : ('b, 'a) ralgebra) (W x : 'b term) : 'a =
    F.map (para' f) x |> f (W x)

  let cata' f x = para' (Fun.const f) x

  let rec apo (f : 'a -> ('b term, 'a) Either.t F.t) (a : 'a) : 'b term =
    let funin = Either.fold ~left:Fun.id ~right:(apo f) in
    f a |> F.map funin |> fun x -> W x

  let rec histo (f : 'a cvalgebra) (W x : 'b term) : 'a =
    let rec worker (W x') =
      { attribute = histo f (W x'); hole = F.map worker x' }
    in
    F.map worker x |> f

  let histo' (f : 'a cvalgebra) (wx : 'b term) : 'a =
    let rec worker (W x) =
      F.map worker x |> fun res -> { attribute = f res; hole = res }
    in
    (worker wx).attribute

  let histo'' (f : 'a cvalgebra) (wx : 'b term) =
    let rec worker (W x) =
      F.map worker x |> fun res -> { attribute = f res; hole = res }
    in
    worker wx

  let cata'' (f : 'a F.t -> 'a) (bt : 'b term) : 'a =
    histo' (fun aaf -> F.map (fun { attribute = a; _ } -> a) aaf |> f) bt

  let para'' (f : 'b term -> 'a F.t -> 'a) (bt : 'b term) : 'a =
    histo'
      (fun aaf ->
        let af = F.map (fun { attribute = a; _ } -> a) aaf in
        let rec worker aaf =
          W (F.map (fun { hole = aaf; _ } -> worker aaf) aaf)
        in
        let bt = worker aaf in
        f bt af)
      bt

  let rec futu (f : 'a cvcoalgebra) (a : 'a) : 'b term =
    let rec worker ca =
      match ca with Automatic a -> futu f a | Manual g -> W (F.map worker g)
    in
    W (F.map worker (f a))
end
