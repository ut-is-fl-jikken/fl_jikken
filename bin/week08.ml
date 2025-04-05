open Assignment_types

let ty =  TypeDef(0, "ty")
let subst = TypeDef(0, "subst")
let ty_subst = Type("ty_subst", "subst -> ty -> ty")
let compose = Type("compose", "subst -> subst -> subst")
let unify = Type("unify", "(ty * ty) list -> subst")

let report = { kind = Report; items = [] }
let toi_ml n items = { kind = Toi(ML, n); items }
let toi_dir n items = { kind = Toi(Dir, n); items }
let hatten_dir n items = { kind = Hatten(Dir, n); items }

let assignments =
  [report;
   toi_ml 2 [ty; subst; ty_subst];
   toi_ml 3 [ty; subst; compose];
   toi_ml 4 [ty; subst; unify];
   toi_dir 5 [];
   hatten_dir 2 []]
