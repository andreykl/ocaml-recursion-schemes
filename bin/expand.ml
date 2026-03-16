(**
  expand.ml — PPX expansion inspector

  This executable is a small debugging tool for PPX/ppxlib-based projects.

  What it does:
  1) Reads an OCaml implementation either from a file (positional FILE argument)
     or from stdin (if FILE is omitted).
  2) Parses the input into a Parsetree using the compiler parser.
  3) Runs ppxlib's driver on the parsed structure (Driver.map_structure),
     which applies all registered transformations (extensions, derivers, etc.).
     Important: this works only if the executable links in the library/module
     that registers your deriver (i.e. executes Deriving.add at startup; e.g. we add
     base_functor to the libraries stanza in the dune file).
  4) Prints the result for the selected AST stage and output view:
     - --stage raw|exp
         raw      : prints the parsed input AST (before running any PPX)
         exp : prints the AST after running ppxlib's driver (Driver.map_structure)
     - --view tree|src
         tree   : prints an AST "tree" dump (like ocamlc -dparsetree). For this output we
                  convert ppxlib's selected AST representation into the compiler's current
                  Parsetree and print it via Ocaml_common.Printast.
         src : pretty-prints the OCaml code (either before or after PPX, depending on --stage).

  Usage examples:
    dune exec ./expand.exe -- --stage exp --view tree   path/to/file.ml
    dune exec ./expand.exe -- --stage exp --view src path/to/file.ml
    dune exec ./expand.exe -- --stage raw      --view tree   path/to/file.ml
    echo "type 'a t = X of 'a | Y of 'a t [@@deriving base_functor]" \
      | dune exec ./expand.exe -- --stage raw --view tree

  Alternatives and related tools:
  - ocamlc -dsource        : prints source after PPX expansion; equivalent to
                             --stage exp --view src, but requires full compilation
                             and does not support raw (pre-PPX) output.
  - ocamlc -dparsetree     : dumps the AST tree after PPX expansion; equivalent to
                             --stage exp --view tree, but again only during
                             compilation and without raw stage.
  - ppxlib-pp              : ships with ppxlib; applies registered transformations and
                             prints expanded source.  Equivalent to --stage exp
                             --view src, but has no raw stage or tree view.
  This tool combines both views (tree/src) and both stages (raw/exp) in one
  utility, works without full compilation, and reads from stdin.  The raw+tree
  combination (parsed AST before any PPX) has no direct equivalent in standard tools.

  Notes about AST types:
  - ppxlib internally works with a "selected" AST (from the ppxlib/astlib world),
    which is not the same type as the compiler-libs Parsetree used by Ocaml_common.
  - Therefore, for the "tree" mode we explicitly convert structures using the
    Ppxlib_ast.Convert helpers.
*)

open Ppxlib

(* Convert ppxlib's selected AST into the current OCaml compiler Parsetree,
   so we can print it with Ocaml_common.Printast (the same style as -dparsetree).
   Private ppxlib API — no public equivalent as of ppxlib 0.35.
   Needed to convert ppxlib AST → compiler-libs Parsetree for Printast. *)
module Convert =
  Ppxlib_ast.Convert
    (Ppxlib_ast.Selected_ast)
    (Ppxlib_ast__Versions.OCaml_current)

(* Output format selection:
   - Tree   : dump AST (Parsetree) after PPX expansion
   - Source : print expanded OCaml source *)
type view = Tree | Source
type stage = Raw | Expanded

(* Main program logic:
   parse -> expand -> print *)
let run (view : view) (stage : stage) (file : string option) =
  let fname, src =
    match file with
    | Some fname ->
        let ic = open_in fname in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> (fname, In_channel.input_all ic))
    | None -> ("stdin.ml", In_channel.input_all stdin)
  in

  (* Set up locations for error messages to use [fname]. *)
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf fname;

  (* Parse and expand using the ppxlib driver. *)
  let ast =
    let parsed = Parse.implementation lexbuf in
    match stage with Raw -> parsed | Expanded -> Driver.map_structure parsed
  in

  (* Print according to the chosen mode. *)
  match view with
  | Tree ->
      (* Convert into the compiler Parsetree, then print like ocamlc -dparsetree. *)
      let ast = Convert.copy_structure ast in
      Ocaml_common.Printast.implementation Format.std_formatter ast
  | Source ->
      (* Pretty-print as OCaml source code. *)
      Format.printf "%a@." Pprintast.structure ast

open Cmdliner

(* --view tree|src *)
let view_term : view Term.t =
  let doc =
    "Output view mode: $(b,tree) prints Parsetree, $(b,src) prints OCaml source code."
  in
  let views = [ ("tree", Tree); ("src", Source) ] in
  Arg.(value & opt (enum views) Source & info [ "view" ] ~docv:"tree|src" ~doc)

(* --stage raw|exp *)
let stage_term : stage Term.t =
  let doc =
    "AST stage: $(b,raw) prints the parsed input AST, $(b,exp) prints the \
     AST after PPX expansion."
  in
  let stages = [ ("raw", Raw); ("exp", Expanded) ] in
  Arg.(value & opt (enum stages) Expanded & info [ "stage" ] ~docv:"raw|exp" ~doc)

(* Optional positional input file. If absent, read from stdin. *)
let file_term : string option Term.t =
  let doc = "Input .ml file. If omitted, reads from stdin." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

(* Commandliner command definition. *)
let cmd =
  let doc =
    "Expand OCaml code with registered ppxlib transformations and print AST or \
     source."
  in
  let info = Cmd.info "expand" ~doc in
  Cmd.v info Term.(const run $ view_term $ stage_term $ file_term)

let () = exit (Cmd.eval cmd)
