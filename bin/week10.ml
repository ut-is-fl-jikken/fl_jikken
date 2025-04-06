open Assignment_types
open Edsl
open Prolog

let assignments =
  [report;
   toi 1 [Predicate("bloodrelative", 2)];
   toi 2 [Predicate("mult", 3)];
   toi 3 [Predicate("reverse", 2);
          Query("reverse([1,2,3],X),write(X).", ["[3,2,1]"]);
          Predicate("concat", 2);
          Query("concat([[1],[2,3]],X),write(X).", ["[1,2,3]"])];
   toi 4 [Predicate("hamilton", 2)]]
