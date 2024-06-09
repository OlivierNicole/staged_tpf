(lang dune 3.15)

(name tpf_staged)

(generate_opam_files true)

(source
 (github OlivierNicole/staged_tpf))

(authors "Olivier Nicole")

(maintainers "Olivier Nicole")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tpf_staged)
 (synopsis "Metaprogramming-based derivers")
 (description "Metaprogramming-based derivers")
 (depends ocaml dune tpf ppx_stage)
 (tags
  (metaprogramming ppx deriving ppx_deriving)))
