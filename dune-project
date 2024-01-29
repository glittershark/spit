(lang dune 3.12)

(using menhir 2.0)

(name spit)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name spit)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  base
  core
  core_unix
  linenoise
  menhir
  nice_parser
  ppx_inline_test)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
