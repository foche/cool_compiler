(lang dune 2.8)
(name cool_compiler)
(generate_opam_files true)
(using menhir 2.1)
(authors "Tuan Anh Le")
(license MIT)

(package
 (name cool_compiler)
 (synopsis "A compiler for COOL.")
 (description "A compiler for the Classroom Object-Oriented Language.")
 (depends
  (ppx_inline_test (>= 0.14))
  (bisect_ppx (>= 2.6))))
