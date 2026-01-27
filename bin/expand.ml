(*
  expand.ml — PPX expansion inspector

  This executable is a small debugging tool for PPX/ppxlib-based projects.

  What it does:
  1) Reads an OCaml implementation either from a file (positional FILE argument)
     or from stdin (if FILE is omitted).
  2) Parses the input into a Parsetree using the compiler parser.
  3) Runs ppxlib's driver on the parsed structure ([Driver.map_structure]),
     which applies all registered transformations (extensions, derivers, etc.).
     Important: this works only if the executable links in the library/module
     that registers your deriver (i.e. executes [Deriving.add] at startup; e.g. we add
     [base_functor] to the [libraries] stanza in the [dune] file).
  4) Prints the result in one of two modes:
     - --mode tree   : prints an AST "tree" representation (like ocamlc -dparsetree).
                       To do that, we convert ppxlib's selected AST representation
                       into the compiler's current Parsetree and then print it via
                       Ocaml_common.Printast.
     - --mode source : pretty-prints the expanded OCaml source code.

  Usage examples:
    dune exec ./expand.exe -- --mode tree   path/to/file.ml
    dune exec ./expand.exe -- --mode source path/to/file.ml
    echo 'type t = int [@@deriving base_functor]' | dune exec ./expand.exe -- --mode tree

  Notes about AST types:
  - ppxlib internally works with a "selected" AST (from the ppxlib/astlib world),
    which is not the same type as the compiler-libs Parsetree used by Ocaml_common.
  - Therefore, for the "tree" mode we explicitly convert structures using the
    Ppxlib_ast.Convert helpers.
*)

open Ppxlib

(* Convert ppxlib's selected AST into the current OCaml compiler Parsetree,
   so we can print it with Ocaml_common.Printast (the same style as -dparsetree). *)
module Convert =
  Ppxlib_ast.Convert
    (Ppxlib_ast.Selected_ast)
    (Ppxlib_ast__Versions.OCaml_current)

(* Output format selection:
   - Tree   : dump AST (Parsetree) after PPX expansion
   - Source : print expanded OCaml source *)
type mode = Tree | Source

(* Read the full contents of an input channel into a string. *)
let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  Buffer.contents buf

(* Main program logic:
   parse -> expand -> print *)
let run (mode : mode) (file : string option) =
  let fname, src =
    match file with
    | Some fname ->
        let ic = open_in fname in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> (fname, read_all ic))
    | None -> ("stdin.ml", read_all stdin)
  in

  (* Set up locations for error messages to use [fname]. *)
  let lexbuf = Lexing.from_string src in
  Location.init lexbuf fname;

  (* Parse and expand using the ppxlib driver. *)
  let ast = Parse.implementation lexbuf in
  let ast' = Driver.map_structure ast in

  (* Print according to the chosen mode. *)
  match mode with
  | Tree ->
      (* Convert into the compiler Parsetree, then print like ocamlc -dparsetree. *)
      let ast' = Convert.copy_structure ast' in
      Ocaml_common.Printast.implementation Format.std_formatter ast'
  | Source ->
      (* Pretty-print as OCaml source code. *)
      Format.printf "%a@." Pprintast.structure ast'

open Cmdliner

(* --mode tree|source *)
let mode_term : mode Term.t =
  let doc =
    "Output mode: tree prints Parsetree, source prints expanded OCaml."
  in
  let modes = [ ("tree", Tree); ("source", Source) ] in
  Arg.(value & opt (enum modes) Tree & info [ "mode" ] ~docv:"MODE" ~doc)

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
  Cmd.v info Term.(const run $ mode_term $ file_term)

let () = exit (Cmd.eval cmd)
