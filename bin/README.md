# `expand` — PPX Expansion Inspector

This executable is a small debugging tool for PPX/ppxlib-based projects.

## What it does: 

1) Reads an OCaml implementation either from a file (positional FILE argument) 
or from stdin (if FILE is omitted).
2) Parses the input into a Parsetree using the compiler parser.
3) Runs ppxlib's driver on the parsed structure (Driver.map_structure),
which applies all registered transformations (extensions, derivers, etc.).
> *Important*: this works only if the executable links in the library/module 
> that registers your deriver (i.e. executes Deriving.add at startup; e.g. 
> we add recurion_schemes.ppx to the libraries stanza in the dune file). 
4) Prints the result for the selected AST stage and output view:
- `--stage raw|exp`
  * raw : prints the parsed input AST (before running any PPX)
  * exp : prints the AST after running ppxlib's driver (Driver.map_structure)
- `--view tree|src`
  * tree : prints an AST "tree" dump (like ocamlc -dparsetree). For this output
  we convert ppxlib's selected AST representation into the compiler's current
  Parsetree and print it via Ocaml_common.Printast.
  * src : pretty-prints the OCaml code (either before or after PPX, depending on --stage).

## Usage examples:

```bash

~$ dune exec ./expand.exe -- --stage exp --view tree path/to/file.ml
~$ dune exec ./expand.exe -- --stage exp --view src  path/to/file.ml
~$ dune exec ./expand.exe -- --stage raw --view tree path/to/file.ml
~$ echo "type 'a t = X of 'a | Y of 'a t [@@deriving recursion_schemes.ppx]" \
      | dune exec ./expand.exe -- --stage raw --view tree

```

## Alternatives and related tools:

- `ocamlc -dsource` : prints source after PPX expansion; equivalent to `--stage
      exp --view src`, but requires full compilation and does not support raw
      (pre-PPX) output.
- `ocamlc -dparsetree` : dumps the AST tree after PPX expansion; equivalent to
      `--stage exp --view tree`, but again only during compilation and without raw
      stage.
- `ppxlib-pp` : ships with ppxlib; applies registered transformations and
      prints expanded source. Equivalent to `--stage exp --view src`, but has no
      raw stage or tree view. This tool combines both views (tree/src) and both
      stages (raw/exp) in one utility, works without full compilation, and reads
      from stdin. The raw+tree combination (parsed AST before any PPX) has no
      direct equivalent in standard tools.

## Notes about AST types:

- ppxlib internally works with a "selected" AST (from the ppxlib/astlib
world), which is not the same type as the compiler-libs Parsetree used by
Ocaml_common.

- Therefore, for the "tree" view we explicitly convert structures using the
Ppxlib_ast.Convert helpers.

