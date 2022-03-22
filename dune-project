(lang dune 3.4)

(name swagger)
(generate_opam_files true)

(source (github andrenth/ocaml-swagger))
(authors
 "Andre Nathan <andre@hostnet.com.br>"
 "Antonin Décimo <antonin@tarides.com>")
(maintainers "Andre Nathan <andre@hostnet.com.br>")
(license "MIT")
(package
 (name swagger)
 (synopsis "Swagger 2.0 code generator for OCaml")
 (description "Swagger 2.0 code generator for OCaml")
 (depends
  (ocaml (>= 4.14))
  (atdgen (>= 2.2.1))
  (re (>= 1.10.3))
  (yojson (>= 1.7.0))))
