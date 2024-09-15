(lang dune 3.10)

(name khasmc)

(generate_opam_files true)

(authors "wren")

(maintainers "wren")

(license MPL-2.0)

(package
 (name khasmc)
 (synopsis "Compiler for khasm programming language")
 (depends ocaml angstrom dune fmt ppx_deriving batteries fileutils ppx_inline_test)
 (tags
  ("compiler" "khasm" "programming language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.1)
