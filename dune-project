(lang dune 3.22)

(name assay)

(generate_opam_files true)

(source
 (github enetsee/assay))

(authors "Michael Thomas <mthomas180@gmail.com>")

(maintainers "Michael Thomas <mthomas180@gmail.com>")

(license MIT)

; [assay] is the ppx, used via [(instrumentation (backend assay))].
; [assay.runtime] is linked into instrumented builds automatically through
; [ppx_runtime_libraries]. They're in one package since you always need both.

(package
 (name assay)
 (synopsis "A mutation tester for OCaml")
 (description
  "Generates mutants from the parsetree. Works as a dune instrumentation backend: every mutant is compiled into a single build and selected at runtime with an environment variable. Supports the classic Mothra operators plus statement block removal and extreme mutation (replacing a function body with a default value).")
 (depends
  (ocaml
   (>= 5.0))
  ppxlib
  (odoc :with-doc))
 (tags
  ("mutation-testing" "ppx" "testing" "coverage")))
