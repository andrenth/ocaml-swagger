(lang dune 3.4)

(name swagger)
(generate_opam_files true)

(source (github andrenth/ocaml-swagger))
(authors
 "Andre Nathan <andre@hostnet.com.br>"
 "Antonin Décimo <antonin@tarides.com>")
(maintainers "Antonin Décimo <antonin@tarides.com>")
(license "MIT")
(package
 (name swagger)
 (synopsis "Swagger 2.0 code generator for OCaml")
 (description "Swagger 2.0 code generator for OCaml")
 (depends
  (ocaml (>= 4.14))
  (atdgen (>= 2.10))
  (re (>= 1.10.3))
  (yojson (>= 2.0))
  (ppxlib (>= 0.25.0))
  (ppx_yojson_conv (and (>= v0.15) :with-test))
  (cohttp-lwt-unix (and (>= 5.0.0) :with-test))
  (uri (and (>= 4.2) :with-test))
  (ocamlformat :with-test)))
