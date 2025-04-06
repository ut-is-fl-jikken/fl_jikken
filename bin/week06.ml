open Assignment_types
open Edsl
open Interpreter

let exec_1 = Exec ["let f = fun x -> x + x;;",                                    "# val f = <fun>";
                   "f 1;;",                                                       "# - = 2";
                   "let a = 10 in let f = fun x -> x + a in let a = 20 in f 5;;", "# - = 15";
                   "(fun f -> fun x -> f (f x)) (fun x -> x + x) 1;;",            "# - = 4"]
let exec_2 = Exec ["let rec sum x = if x<1 then 0 else x + sum (x-1);;",                 "# val sum = <fun>";
                   "sum 10;;",                                                           "# - = 55";
                   "let rec fib x = if x<2 then x else fib(x-1) + fib(x-2) in fib 10;;", "# - = 55"]
let exec_3 = Exec ["let rec even x = if x=0 then true else odd (x-1) and odd x = if x=0 then false else even (x-1);;", "# val even = <fun>";
                   "",                                                                                                 "val odd = <fun>";
                   "even 3;;",                                                                                         "# - = false";
                   "odd 3;;",                                                                                          "# - = true"]
let exec_h1 = Exec ["let add x y = x + y;;",                       "# val add = <fun>";
                    "(fun f x -> f (f x)) (fun x -> add x x) 1;;", "# - = 4"]
let exec_h3 = Exec ["let a = 10;;",              "# val a = 10";
                    "let f = dfun x -> x + a;;", "# val f = <fun>";
                    "let a = 20;;",              "# val a = 20";
                    "f 5;;",                     "# - = 25"]

let assignments =
  [report;
   toi 1 [build; exec_1]
     ~alt:[toi_id 2; toi_id 3];
   toi 2 [build; exec_2]
     ~alt:[toi_id 3];
   toi 3 [build; exec_3];
   toi 4 [build; exec_2];
   hatten 1 [build; exec_h1];
   hatten 2 [build; exec_2];
   hatten 3 [build; exec_h3]]
