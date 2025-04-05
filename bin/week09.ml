open Assignment_types

let report = { kind = Report; items = [] }
let build = Build(None, [])

let toi n items = { kind = Toi(Dir, n); items }
let hatten n items = { kind = Hatten(Dir, n); items }

let assignments =
  [report;
   toi 1 [build];
   toi 2 [build];
   toi 3 [build];
   toi 4 [build];
   hatten 1 [build];
   hatten 2 [build]]
