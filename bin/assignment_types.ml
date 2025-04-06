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
  kind: kind;
  items: item list;
  alternative: kind list option;
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

(* EDSL for creating assignments *)
module Edsl = struct
  let report = { kind = Report; items = []; alternative = None }

  module Ml = struct
    let toi_id n = Toi(ML, n)
    let toi n ?alt items = { kind = toi_id n; items; alternative = alt }
    let hatten n ?alt items = { kind = Hatten(ML, n); items; alternative = alt }
  end

  module Interpreter = struct
    let toi_id n = Toi(Dir, n)
    let toi n ?alt items = { kind = toi_id n; items; alternative = alt }
    let hatten n ?alt items = { kind = Hatten(Dir, n); items; alternative = alt }
    let build = Build(None, [])
  end

  module Prolog = struct
    let toi n ?alt items = { kind = Toi(Prolog, n); items; alternative = alt }
    let hatten n ?alt items = { kind = Hatten(Prolog, n); items; alternative = alt }
  end

  module Dir = struct
    let toi n ?alt items = { kind = Toi(Dir, n); items; alternative = alt }
    let hatten n ?alt items = { kind = Hatten(Dir, n); items; alternative = alt }
  end
end
