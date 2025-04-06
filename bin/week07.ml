open Assignment_types
open Edsl
open Interpreter

let may_be_included =
  ["constraintSolver.cmi";
   "constraintSolver.cmo";
   "tySyntax.cmi";
   "tySyntax.cmo"]

let assignments =
  [report;
   toi 1 [Build(None, may_be_included)]]
