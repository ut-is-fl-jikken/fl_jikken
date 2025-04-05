type submission_kind = Dir | ML | Prolog

type kind =
  | Report
  | Toi of submission_kind * int
  | Hatten of submission_kind * int

type item =
  (* Items for OCaml files *)
  | ValDef of string (* Check the existence of a value *)
  | Value of string * string (* Check the value of an expression *)
  | Type of string * string (* Check the type of an expression *)
  | TypeOpt of string * string (* Check the type of an expression for optional problems *)
  | TypeDef of int * string (* Check the existence of a type *)
  | ModDef of string * string option (* Check the existence of a module *)
  | Module of string * string (* Check the type of a module *)
  | Excep of string (* Check the existence of an exception *)
  | CurryUncurry of string * string (* Just for Hatten 3 of the second lecture *)
  | Build of string option * string list (* Check the buildability *)
  | Exec of (string * string) list (* Check the behavior of the main. "Exec" must follow "Build" *)
  (* Items for SWI-Prolog files *)
  | Predicate of string * int (* Check the existence of a predicate *)
  | Query of string * string list (* Check a query *)

type t = {
  kind  : kind;
  items : item list;
}

let is_hatten = function
  | Hatten (_, _) -> true
  | _ -> false

let is_optional = is_hatten

let is_directory = function
  | Toi(Dir, _) | Hatten(Dir, _) -> true
  | _ -> false

let subject_of t =
  match t, !Config.jp with
  | Report, true -> "レポート"
  | Report, false -> "Report"
  | Toi(_, n), true -> "問" ^ string_of_int n
  | Toi(_, n), false -> "Toi " ^ string_of_int n
  | Hatten(_, n), true -> "発展" ^ string_of_int n
  | Hatten(_, n), false -> "Hatten " ^ string_of_int n

let subject_id_of t =
  match t with
  | Report -> "report"
  | Toi(_, n) -> "toi" ^ string_of_int n
  | Hatten(_, n) -> "hatten" ^ string_of_int n
