open Assignment_types

let ty =  TypeDef(0, "ty")
let subst = TypeDef(0, "subst")
let ty_subst = Type("ty_subst", "subst -> ty -> ty")
let compose = Type("compose", "subst -> subst -> subst")
let unify = Type("unify", "(ty * ty) list -> subst")

let toi n = Toi(ML, n)

let assignments =
  [Toi(ML, 2), [ty; subst; ty_subst];
   Toi(ML, 3), [ty; subst; compose];
   Toi(ML, 4), [ty; subst; unify];
   Toi(Dir, 5), [];
   Hatten(Dir, 2), []]
