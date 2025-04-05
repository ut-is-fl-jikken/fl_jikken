open Assignment_types

let build = Build(None, [])

let toi n = Toi(Dir, n)
let hatten n = Hatten(Dir, n)

let assignments =
  [toi 1, [build];
   toi 2, [build];
   toi 3, [build];
   toi 4, [build];
   hatten 1, [build];
   hatten 2, [build]]
