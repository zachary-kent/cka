(lang dune 3.10)

(using menhir 2.1)

(name cka)

(generate_opam_files true)

(source
 (github zachary-kent/cka))

(authors "Zak Kent")

(maintainers "Zak Kent")

(license MIT)

(package
 (name cka)
 (synopsis "CKA Decision Procedure")
 (description
  "A decision procedure for the refinement of series-rational expressions")
 (depends ocaml dune menhir core))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
