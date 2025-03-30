type t =
  | Toi of kind * int
  | Hatten of kind * int
and kind = Dir | ML | Prolog

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

type 'a result =
  | OK of 'a
  | Version_mismatch
  | Cannot_extract
  | File_name_invalid of string
  | Directory_not_found of string
  | File_not_found of string
  | File_not_found_after_build of string
  | Value_not_found of string
  | Type_mismatch of string
  | Type_not_found of string
  | Constructor_not_found of string
  | Module_not_found of string
  | Predicate_not_found of string
  | Incorrect_result
  | Uncaught_exception
  | Object_file_found of string
  | Build_failed
  | Unsupported_week_no of int
  | Zip_failed
  | Unknown_error of string

let is_hatten = function
  | Hatten (_, _) -> true
  | _ -> false

let is_directory = function
  | Toi(Dir, _) | Hatten(Dir, _) -> true
  | _ -> false

let filename_of = function
  | Toi(ML, n) -> Printf.sprintf "toi%d.ml" n
  | Toi(Prolog, n) -> Printf.sprintf "toi%d.pl" n
  | Toi(Dir, n) -> Printf.sprintf "toi%d" n
  | Hatten(ML, n) -> Printf.sprintf "hatten%d.ml" n
  | Hatten(Prolog, n) -> Printf.sprintf "hatten%d.pl" n
  | Hatten(Dir, n) -> Printf.sprintf "hatten%d" n

let subject_of t =
  match t, !Config.jp with
  | Toi(_, n), true -> "問" ^ string_of_int n
  | Toi(_, n), false -> "Toi " ^ string_of_int n
  | Hatten(_, n), true -> "発展" ^ string_of_int n
  | Hatten(_, n), false -> "Hatten " ^ string_of_int n

let subject_id_of t =
  match t with
  | Toi(_, n) -> "toi" ^ string_of_int n
  | Hatten(_, n) -> "hatten" ^ string_of_int n

let message_of r =
  match r, !Config.jp with
  | OK None, _ -> ""
  | OK (Some s), _ -> s
  | Version_mismatch, true -> Printf.sprintf "OCaml %s で実行してください" Config.ocaml_version
  | Version_mismatch, false -> Printf.sprintf "Execute this program with OCaml %s." Config.ocaml_version
  | Cannot_extract, true -> Printf.sprintf "入力ファイルの展開に失敗しました"
  | Cannot_extract, false -> Printf.sprintf "Cannot extract the input file"
  | File_name_invalid f, true -> Printf.sprintf "ファイル名 %sが不正です" f
  | File_name_invalid f, false -> Printf.sprintf "Filename %s invalid" f
  | Directory_not_found f, true -> Printf.sprintf "ディレクトリ %s が見つかりません" f
  | Directory_not_found f, false -> Printf.sprintf "Directory %s not found" f
  | File_not_found f, true -> Printf.sprintf "ファイル %s が見つかりません" f
  | File_not_found f, false -> Printf.sprintf "File %s not found" f
  | File_not_found_after_build f, true -> Printf.sprintf "ビルド後にファイル %s が見つかりません" f
  | File_not_found_after_build f, false -> Printf.sprintf "File %s not found after build" f
  | Type_mismatch v, true -> Printf.sprintf "%s の型が合っていません" v
  | Type_mismatch v, false -> Printf.sprintf "Type of %s is mismatched" v
  | (Value_not_found x | Type_not_found x | Constructor_not_found x | Module_not_found x | Predicate_not_found x), true -> Printf.sprintf "%s が見つかりません" x
  | (Value_not_found x | Type_not_found x | Constructor_not_found x | Module_not_found x | Predicate_not_found x), false -> Printf.sprintf "%s not found" x
  | Incorrect_result, true -> Printf.sprintf "結果が正しくありません"
  | Incorrect_result, false -> Printf.sprintf "Incorrect result"
  | Uncaught_exception, true -> Printf.sprintf "例外が発生しました"
  | Uncaught_exception, false -> Printf.sprintf "Uncaught exception occurred"
  | Object_file_found s, true -> Printf.sprintf "%s を消してください" s
  | Object_file_found s, false -> Printf.sprintf "Remove %s" s
  | Build_failed, true -> Printf.sprintf "ビルドに失敗しました"
  | Build_failed, false -> Printf.sprintf "Build failed"
  | Unsupported_week_no n, true -> Printf.sprintf "第%d週の課題はサポートしていません" n
  | Unsupported_week_no n, false -> Printf.sprintf "Not support: Week %d" n
  | Zip_failed, true -> Printf.sprintf "zip コマンドが失敗しました (zip.err を参照してください)"
  | Zip_failed, false -> Printf.sprintf "Command zip failed (See zip.err)"
  | Unknown_error s, true -> Printf.sprintf "不明なエラー (%s)" s
  | Unknown_error s, false -> Printf.sprintf "Unknown error (%s)" s
