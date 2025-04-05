include Assignment_types
open Error

let list =
  [ 1, Week01.assignments;
    2, Week02.assignments;
    3, Week03.assignments;
    4, Week04.assignments;
    5, Week05.assignments;
    6, Week06.assignments;
    7, Week07.assignments;
    8, Week08.assignments;
    9, Week09.assignments;
   10, Week10.assignments;
   11, Week11.assignments;
   12, Week12.assignments;
   13, []]

let assoc n =
  try
    List.assoc n list
  with Not_found ->
         show_error_and_exit (Unsupported_week_no n)
