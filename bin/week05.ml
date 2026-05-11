open Assignment_types
open Edsl
open Interpreter

let assignments =
  [report;
   toi 1 [Type("eval", "expr -> value"); ValDef("print_expr"); ValDef("expr_gen")]
     ~alt:[toi_id 2; toi_id 3];
   toi 2 [Type("eval", "expr -> value"); ValDef("print_expr"); ValDef("expr_gen")]
     ~alt:[toi_id 3];
   toi 3 [Type("eval", "expr -> value"); ValDef("print_expr"); ValDef("expr_shr"); ValDef("expr_gen")];
   toi 4 [Type("check_inv1", "'a RedBlackTree.t -> bool");
          Type("check_inv2", "'a RedBlackTree.t -> bool")];
   hatten 1 [];
   hatten 2 [CurryUncurry("h", "f")]]
