open Assignment_types
open Edsl
open Interpreter

let exec_2_1 = Exec ["(1+2)/3-4;;", "# - = -3"]
let exec_2_2 = Exec ["4*3 < 2-1;;", "# - = false"]
let exec_3 = Exec ["let x = 1;;",                           "# val x = 1";
                   "let y = 2 + x in let x = 3 in x + y;;", "# - = 6"]
let exec_4_1 = Exec ["let tt=true in if tt||false then 1*2 else 3-4;;", "# - = 2"]
let exec_4_2 = Exec ["let tt=true in if tt&&false then 1*2 else 3-4;;", "# - = -1"]
let exec_h2 = Exec ["let x = 10\nlet y = x+1\n let z = x*y;;", "# val x = 10";
                    "",                                        "val y = 11";
                    "",                                        "val z = 110"]
let exec_h3_1 = Exec ["let x = 10;;",                "# val x = 10";
                      "let x = 50\nand y = x * 2;;", "# val x = 50";
                      "",                            "val y = 20"]
let exec_h3_2 = Exec ["let x = 10;;",                          "# val x = 10";
                      "let x = 50\nand y = x * 2\nin x + y;;", "# - = 70"]

let assignments =
  [report;
   toi 1 [];
   toi 2 [build; exec_2_1; exec_2_2]
     ~alt:[toi_id 3; toi_id 4];
   toi 3 [build; exec_3]
     ~alt:[toi_id 4];
   toi 4 [build; exec_2_1; exec_2_2; exec_3; exec_4_1; exec_4_2];
   hatten 1 [build];
   hatten 2 [build; exec_h2];
   hatten 3 [build; exec_h3_1; exec_h3_2]]
