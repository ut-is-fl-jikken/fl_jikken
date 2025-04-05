open Assignment_types

let report = { kind = Report; items = [] }
let toi n items = { kind = Toi(Dir, n); items }

let may_be_included =
  ["constraintSolver.cmi";
   "constraintSolver.cmo";
   "tySyntax.cmi";
   "tySyntax.cmo"]

let assignments =
  [report;
   toi 1 [Build(None, may_be_included)]]
