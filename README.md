# Recursion Schemes Library for OCaml

This library implements `cata-`, `para-`, `histo-`, `ana-`, `apo-`, and `futu-` 
morphisms, as described in the original paper.*

To build the library:

```bash

$ opam install --deps-only .
$ dune build

```

```bash

$ dune exec ./examples/plant/plant.exe

```

* Meijer, Erik; Fokkinga, Maarten; Paterson, Ross. (2000).
Functional Programming with Bananas, Lenses, Envelopes and
Barbed Wire. 523. doi:10.1007/3540543961_7.
