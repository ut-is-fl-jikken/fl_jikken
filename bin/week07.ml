open Assignment_types

let may_be_included =
  ["constraintSolver.cmi";
   "constraintSolver.cmo";
   "tySyntax.cmi";
   "tySyntax.cmo"]

let assignments =
  [Toi(Dir, 1), [Build(None, may_be_included)]]
