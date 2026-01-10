# Example: Using the Recursion Schemes Library

This example draws random pseudo-graphics flower like
This example draws a random, flower-like piece of ASCII pseudo-graphics.

```text

*
|
|   *
| * |
+-+-+
  |
  | 
  ^

```

To build the library and the example, run (from the project root):

```bash

$ opam install --deps-only .
$ dune build

```

To run the example (from the current directory):

```bash

$ dune exec ./plant.exe

```

Most of the code is located in `lib/plant.ml`.
