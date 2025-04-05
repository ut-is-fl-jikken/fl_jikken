open Assignment_types

let toi n = Toi(ML, n)
let hatten n = Hatten(ML, n)

let assignments =
  [toi 1, [Type("eLookup", "'a -> ('a * 'b) list -> ('b, string) result");
           Type("lookupDiv", "'a -> 'a -> ('a * int) list -> (int, string) result");
           Value({|lookupDiv "x" "y" ["x",6;"y",0;"z",2]|}, {|Error "Division by Zero"|});
           Value({|lookupDiv "x" "z" ["x",6;"y",0;"z",2]|}, {|Ok 3|});
           Value({|lookupDiv "x" "b" ["x",6;"y",0;"z",2]|}, {|Error "Not found"|});
           Value({|lookupDiv "a" "z" ["x",6;"y",0;"z",2]|}, {|Error "Not found"|})];
   toi 2, [];
   toi 3, [TypeDef(1, "m");
           Type("(>>=)", "'a m -> ('a -> 'b m) -> 'b m");
           Type("return", "'a -> 'a m");
           Type("writer", "string -> unit m");
           Value({|let f x = x+1, Printf.sprintf "call f(%d), " x in let g x = 2*x, Printf.sprintf "call g(%d), " x in f 3 >>= fun a -> g a >>= fun b -> f b >>= fun c -> return c|},
                 {|(9, "call f(3), call g(4), call f(8), ")|})];
   toi 4, [TypeDef(1, "m");
           Type("(>>=)", "'a m -> ('a -> 'b m) -> 'b m");
           Type("return", "'a -> 'a m");
           Type("memo", "(int -> int m) -> int -> int m");
           Type("runMemo", "'a m -> 'a");
           Value("let rec fib n = if n <= 1 then return n else memo fib (n-2) >>= fun r1 -> memo fib (n-1) >>= fun r2 -> return (r1 + r2) in runMemo (fib 80)", "23416728348467685")];
   hatten 1, [];
   hatten 2, [Type("(>>=)", "'a list -> ('a -> 'b list) -> 'b list");
              Type("return", "'a -> 'a list")];
   (* TODO: Add a test case *)
   hatten 3, []]
