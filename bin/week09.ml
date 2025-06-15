open Assignment_types
open Edsl
open Interpreter

let assignments =
  [report;
   toi 1 [build]
     ~alt:[toi_id 2; toi_id 3];
   toi 2 [build]
     ~alt:[toi_id 3];
   toi 3 [build];
   toi 4 [build];
   hatten 1 [build];
   hatten 2 [build]]
