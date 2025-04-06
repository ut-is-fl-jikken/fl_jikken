open Assignment_types
open Edsl
open Ml

let assignments =
  [report;
   toi 1 [Type("sum_to", "int -> int");
          Type("is_prime", "int -> bool");
          Type("gcd", "int -> int -> int")];
   toi 2 [ValDef "twice";
          ValDef "repeat"];
   toi 3 [Type("sum_to_fix", "int -> int");
          Type("is_prime_fix", "int -> bool");
          Type("gcd_fix", "int -> int -> int")];
   toi 4 [ValDef "fold_left";
          ValDef "fold_right"];
   toi 5 [Type("append", "'a list -> 'a list -> 'a list");
          Type("filter", "('a -> bool) -> 'a list -> 'a list")];
   toi 6 [Type("append_left", "'a list -> 'a list -> 'a list");
          Type("filter_left", "('a -> bool) -> 'a list -> 'a list");
          Type("append_right", "'a list -> 'a list -> 'a list");
          Type("filter_right", "('a -> bool) -> 'a list -> 'a list")];
   toi 7 [Type("perm", "'a list -> 'a list list")];
   hatten 1 [ValDef "reverse";
             ValDef "reverse_right"];
   hatten 2 [ValDef "fold_left_by_right";
             ValDef "fold_right_by_left"];
   hatten 3 [ValDef "add";
             ValDef "mul";
             ValDef "sub"]]
