# Attention! No public release yet!

# Recursion Schemes Library for OCaml

This library implements `cata-`, `para-`, `histo-`, `ana-`, `apo-`, and `futu-` 
morphisms, as described in the original paper.*

To build the library:

```bash

$ opam install --deps-only .
$ dune build

```

To run the examples

```bash

$ dune exec ./examples/plant/plant.exe
$ dune exec ./examples/change/change.exe

```

### Recommended Resources

| Resource | Description |
|----------|-------------|
| [*Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf) | The seminal paper introducing recursion schemes |
| [Practical Recursion Schemes](https://blog.sumtypeofway.com/archive.html) | Excellent tutorial series by Patrick Thomson |
| [recursion-schemes Haskell package](https://hackage.haskell.org/package/recursion-schemes) | The canonical implementation |
