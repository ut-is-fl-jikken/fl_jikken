open Assignment_types
open Edsl
open Ml

let assignments =
  [report;
   toi 1 [Type("add", "nat -> nat -> nat");
          Type("sub", "nat -> nat -> nat");
          Type("mul", "nat -> nat -> nat");
          Type("pow", "nat -> nat -> nat");
          Type("n2i", "nat -> int");
          Type("i2n", "int -> nat")];
   toi 2 [ValDef "pre_order";
          ValDef "in_order";
          ValDef "post_order"];
   toi 3 [ValDef "level_order"];
   toi 4 [TypeDef(0, "expr")];
   toi 5 [Type("eval", "expr -> value");
          Excep("Eval_error")];
   hatten 1 [Type("fix", "(('a -> 'b) -> 'a -> 'b) -> 'a -> 'b")];
   hatten 2 [TypeOpt("prop1", "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c");
             TypeOpt("prop2", "('a, ('b, 'c) and_t) or_t -> (('a, 'b) or_t, ('a, 'c) or_t) and_t");
             TypeOpt("prop3", "(('a, 'b) or_t, ('a, 'c) or_t) and_t -> ('a, ('b, 'c) and_t) or_t");
             TypeOpt("prop4", "true_t -> ('a, 'a not_t) or_t");
             TypeOpt("prop4", "true_t -> ('a -> 'c) -> ('a not_t -> 'c) -> 'c");
             TypeOpt("prop5", "true_t -> ('a, 'a not_t) and_t");
             TypeOpt("prop5", "true_t -> ('a -> 'a not_t -> 'c) -> 'c");
             TypeOpt("prop6", "(('a -> 'b) -> 'a) -> 'a");
             TypeOpt("prop_cc1", "('a -> 'b) -> ('b -> 'c) -> 'a -> 'c");
             TypeOpt("prop_cc2", "('a, ('b, 'c) and_t) or_t -> (('a, 'b) or_t, ('a, 'c) or_t) and_t");
             TypeOpt("prop_cc3", "(('a, 'b) or_t, ('a, 'c) or_t) and_t -> ('a, ('b, 'c) and_t) or_t");
             TypeOpt("prop_cc4", "true_t -> ('a, 'a not_t) or_t");
             TypeOpt("prop_cc4", "true_t -> ('a -> 'c) -> ('a not_t -> 'c) -> 'c");
             TypeOpt("prop_cc5", "true_t -> ('a, 'a not_t) and_t");
             TypeOpt("prop_cc5", "true_t -> ('a -> 'a not_t -> 'c) -> 'c");
             TypeOpt("prop_cc6", "(('a -> 'b) -> 'a) -> 'a")];
   hatten 3 [CurryUncurry("h", "f")]]
