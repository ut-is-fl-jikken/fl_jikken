(lang dune 3.15)
(name fl_jikken)
(generate_opam_files)
(version v2024.4)
(source (github KenSakayori/fl_jikken))
(authors "Ken Sakayori <sakayori@is.s.u-tokyo.ac.jp>")
(maintainers "Ken Sakayori <sakayori@is.s.u-tokyo.ac.jp>")

(package
 (name fl_jikken)
 (synopsis "Check program for FL jikken")
 (depends
  (ocaml (= 5.1.0))
   ocamlfind
   dune
   batteries
))
