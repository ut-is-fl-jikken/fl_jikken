open Assignment_types

let report = { kind = Report; items = [] }
let toi n items = { kind = Toi(ML, n); items }
let hatten n items = { kind = Hatten(ML, n); items }

let assignments =
  [report;
   toi 2 [Type("AbstStack.pop", "'a AbstStack.t -> 'a * 'a AbstStack.t");
          Type("AbstStack.push", "'a -> 'a AbstStack.t -> 'a AbstStack.t");
          Type("AbstStack.empty", "'a AbstStack.t");
          Type("AbstStack.size", "'a AbstStack.t -> int");
          Value("AbstStack.empty", "<abstr>")];
   toi 3 [Value("let module M = AbstMultiset2(struct type t = int let compare _ _ = EQ end) in M.count 0 (M.remove 0 (M.add 0 M.empty))", "0")];
   toi 4 [ModDef("MakeMap(struct type t = int let compare _ _ = EQ end)", Some "MakeMap")];
   toi 5 [ModDef("Matrix(struct type t = int let add (x:t) (y:t) = x let mul = add let unit = 0 let zero = 0 end)", Some "Matrix");
          ModDef("BoolMatrix", None);
          ModDef("TropMatrix", None)];
   hatten 1 [Module("Eq", "EQ");
             Type("eval", "'a expr -> 'a value")];
   hatten 2 [Module("Eq2", "EQ2")]]
