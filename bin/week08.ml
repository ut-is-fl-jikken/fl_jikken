open Assignment_types
open Edsl

let ty =  TypeDef(0, "ty")
let subst = TypeDef(0, "subst")
let ty_subst = Type("ty_subst", "subst -> ty -> ty")
let compose = Type("compose", "subst -> subst -> subst")
let unify = Type("unify", "(ty * ty) list -> subst")

let assignments =
  [report;
   Ml.toi 2 [ty; subst; ty_subst]
     ~alt:[Ml.toi_id 3; Ml.toi_id 4];
   Ml.toi 3 [ty; subst; compose]
     ~alt:[Ml.toi_id 4];
   Ml.toi 4 [ty; subst; unify];
   Interpreter.toi 5 [];
   Interpreter.hatten 2 []]
