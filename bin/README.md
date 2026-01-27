# `expand` — PPX Expansion Inspector

A small developer tool for debugging PPX / `ppxlib` transformations (derivers, extensions, etc.).

It is especially useful when you want to see:

- what the parser produced (the input AST),
- what your deriver generated,
- how the final expanded structure looks after the `ppxlib` driver ran.

## What it does

`expand`:

1. Reads an OCaml implementation from:
   - a file (`FILE` positional argument), or
   - `stdin` (if `FILE` is omitted).
2. Parses the source into an OCaml Parsetree.
3. Runs the `ppxlib` driver (`Ppxlib.Driver.map_structure`), which applies all registered
   transformations (derivers, extensions, etc.).
4. Prints either:
   - the resulting AST "tree" (similar to `ocamlc -dparsetree`), or
   - the resulting expanded OCaml source code.

> Important: for derivers to run, the executable must **link** the library/module that
> registers them (i.e. executes `Deriving.add ...` at initialization time).  
> In practice that means `expand` must depend on your PPX library (e.g. `base_functor`)
> in `bin/dune`.

## Why this tool exists (why not `ppx_tools` / `dumpast`?)

Historically, `ppx_tools` (including `dumpast`) was a convenient way to inspect ASTs.
However, in modern setups (OCaml 5.x + `ppxlib`) it is usually not the best option:

- `ppx_tools` belongs to an older PPX ecosystem and is often outdated or incompatible
  with newer compiler AST versions.
- `dumpast` is itself “just another PPX”, so you must place it at exactly the right point
  in the PPX pipeline to see the AST **after** your deriver runs.
- `ppxlib` internally uses a "selected" AST representation (from `astlib`) that is **not**
  the same as the compiler-libs `Parsetree` used by `Ocaml_common.Printast` / `ocamlc -dparsetree`.
  To get output identical to compiler dumps, you need an explicit conversion step.

This tool makes the pipeline explicit and reproducible:

`parse → Driver.map_structure → print`

## Output modes

`expand` supports two output modes:

- `--mode tree`  
  Prints an AST tree similar to `ocamlc -dparsetree`.

  Implementation note: since `ppxlib` returns its own selected AST type, we convert it
  to the compiler’s current Parsetree using `Ppxlib_ast.Convert`, then print it via
  `Ocaml_common.Printast`.

- `--mode source`  
  Prints the expanded OCaml source code (pretty-printed).

## Usage

### From a file

```sh

dune exec ./bin/expand.exe -- --mode tree path/to/file.ml
dune exec ./bin/expand.exe -- --mode source path/to/file.ml

```

### From stdin

```sh

echo 'type t = int [@@deriving base_functor]' \
  | dune exec ./bin/expand.exe -- --mode tree

```

## Notes

* If you see that nothing expands, it usually means your PPX registration code was not
linked into the executable. Ensure bin/dune depends on the library that contains
Deriving.add ....

* If you only need a quick compiler-style dump and you already have a working -ppx
invocation, an alternative is:

```sh

ocamlc -dparsetree -ppx ./your_ppx.exe -c file.ml

```

The advantage of expand is that it avoids a full compilation step and is often
easier to run iteratively while developing a PPX.
