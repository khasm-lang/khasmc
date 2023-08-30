(lang dune 3.6)

(name khasmc)

(generate_opam_files true)

(authors "wren")

(maintainers "wren")

(license MPL-2.0)


(package
 (name khasmc)
 (synopsis "Compiler for khasm programming language")
 (depends ocaml dune fmt ppx_deriving batteries menhir ppx_deriving.show fileutils)
 (tags
  ("compiler" "khasm" "programming language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.1)
