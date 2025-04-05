open Assignment_types

let report = { kind = Report; items = [] }
let hatten_dir n items = { kind = Hatten(Dir, n); items }

let assignments =
  [report;
   hatten_dir 2 []]
