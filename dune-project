(lang dune 3.17)
(name recursion)
(package
 (name recursion)
 (depends
  (ocaml (>= 5.2.1))
  (core (>= 0.17.1))
  (ppx_deriving (>= 6.0.3))
  (alcotest :with-test)))
(generate_opam_files true)
(synopsis "recursion schemes for OCaml")
(description "recursion schemes for OCaml")
(maintainer "Klaus Andrey <klaus.ai1983@gmail.com>")
(authors "Klaus Andrey <klaus.ai1983@gmail.com>")
