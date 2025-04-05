open Assignment_types

let report = { kind = Report; items = [] }
let toi_prolog n items = { kind = Toi(Prolog, n); items }
let hatten_dir n items = { kind = Hatten(Dir, n); items }

let assignments =
  [report;
   toi_prolog 3 [];
   hatten_dir 1 []]
