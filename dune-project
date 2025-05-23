(lang dune 3.15)
(name fl_jikken)
(generate_opam_files)
(version v2025.5)
(source (github ut-is-fl-jikken/fl_jikken))
(authors "Ken Sakayori <sakayori@is.s.u-tokyo.ac.jp>")
(maintainers "Ken Sakayori <sakayori@is.s.u-tokyo.ac.jp>")

(package
 (name fl_jikken)
 (synopsis "Check program for FL jikken")
 (depends
  (ocaml (= 5.3.0))
   ocamlfind
   dune
   batteries
   yojson
))
