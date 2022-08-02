(lang dune 3.3)

(name khasmc)

(generate_opam_files true)

;(source
; (github username/reponame))


(license LICENSE)


(package
 (name khasmc)
 (synopsis "Compiler for khasm programming language")
 (depends ocaml dune)
 (tags
  ("compiler" "khasm" "programming language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.1)