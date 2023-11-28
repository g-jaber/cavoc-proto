(lang dune 3.11)

(name cavoc)
(using menhir 2.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Guilhem Jaber")

(maintainers "Guilhem Jaber")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cavoc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
