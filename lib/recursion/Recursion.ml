module Make =
functor
  (F : Functor.S)
  (P : Project.S with module Base = F)
  ->
  struct
    type 'a algebra = 'a F.t -> 'a
    type 'a ralgebra = P.t -> 'a F.t -> 'a
    type 'a attr = { attribute : 'a; hole : 'a attr F.t }
    type 'a cvalgebra = 'a attr F.t -> 'a

    let rec cata (alg : 'a algebra) (x : P.t) : 'a =
      F.map (cata alg) @@ P.project x |> alg

    let rec para (alg : 'a ralgebra) (x : P.t) : 'a =
      F.map (para alg) @@ P.project x |> alg x

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
