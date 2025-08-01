(lang dune 3.18)
(using menhir 3.0)
(cram enable)

(name pac)

(source
 (github fn06/pac))

(authors "Ryan Gibb <ryan@freumh.org>")

(maintainers "Ryan Gibb <ryan@freumh.org>")

(license MIT)

(documentation https://github.com/fn06/pac)

(package
 (name pac)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml menhir format cmdliner repr ppx_repr)
 (tags
  ("add topics" "to describe" your project)))
