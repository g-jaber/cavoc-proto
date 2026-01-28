(lang dune 3.18)

(name cavoc)
(using menhir 3.0)

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
(depends
  ocaml
  odoc
  dune
  js_of_ocaml
  js_of_ocaml-ppx
  js_of_ocaml-lwt
  lwt
  lwt_ppx
  cohttp
  cohttp-lwt-jsoo
  cohttp-lwt-unix
  ppx_deriving_yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
