(lang dune 3.8)

(using directory-targets 0.1)

(using melange 0.1)

(using menhir 2.1)

(name stepper)

(generate_opam_files true)

(source
 (github tonyfettes/hazel-mini-stepper))

(authors "Haoxiang Fei")

(maintainers "Haoxiang Fei")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name stepper)
 (synopsis "Reference implementation of the Hazel Mini stepper")
 (description "Reference implementation of the Hazel Mini stepper")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
