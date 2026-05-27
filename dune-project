(lang dune 3.13)

(name khasmc)

(generate_opam_files true)

(authors "wren")

(maintainers "wren")

(license MPL-2.0)

(package
 (name khasmc)
 (synopsis "Compiler for khasm programming language")
 (depends ocaml sedlex angstrom parseff dune fmt ppx_deriving batteries fileutils ppx_inline_test domainslib)
 (tags
  ("compiler" "khasm" "programming language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 3.0)
