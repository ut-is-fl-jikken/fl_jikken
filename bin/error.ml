open Util

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
  | No_input_file
  | No_subcommand
  | Bad_week_number of string
  | Unknown_subcommand of string
  | Invalid_option of string
  | Illegal_option_not_in_subcommand of { actual: string; option: string }
  | Illegal_option_in_subcommand of { actual: string; proper: string; option: string }
  | Option_too_early of { proper: string; option: string }
  | Unknown_error of string

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
  | No_input_file, true -> Printf.sprintf "入力ファイルを提供するか、または `--disable-sandboxing` オプションを指定してください"
  | No_input_file, false -> Printf.sprintf "Please provide an input file or specify the `--disable-sandboxing` option"
  | No_subcommand, true -> Printf.sprintf "サブコマンドを指定してください"
  | No_subcommand, false -> Printf.sprintf "Please specify a subcommand"
  | Bad_week_number s, true -> Printf.sprintf "不正な週番号: %s" s
  | Bad_week_number s, false -> Printf.sprintf "Bad week number: %s" s
  | Unknown_subcommand s, true -> Printf.sprintf "不明なサブコマンド: %s" s
  | Unknown_subcommand s, false -> Printf.sprintf "Unknown subcommand: %s" s
  | Invalid_option s, true -> Printf.sprintf "不正なオプション: %s" s
  | Invalid_option s, false -> Printf.sprintf "Invalid option: %s" s
  | Illegal_option_not_in_subcommand { actual; option }, true -> Printf.sprintf "サブコマンド %s ではオプション %s は無効です" actual option
  | Illegal_option_not_in_subcommand { actual; option }, false -> Printf.sprintf "Option %s is invalid for subcommand %s" option actual
  | Illegal_option_in_subcommand { actual; proper; option }, true -> Printf.sprintf "サブコマンド %s では %s 用のオプション %s は無効です" actual proper option
  | Illegal_option_in_subcommand { actual; proper; option }, false -> Printf.sprintf "Option %s is only for subcommand %s, not for %s" option proper actual
  | Option_too_early { proper; option }, true -> Printf.sprintf "オプション %s はサブコマンド %s の後に指定してください" option proper
  | Option_too_early { proper; option }, false -> Printf.sprintf "Option %s must be specified after subcommand %s" option proper
  | Unknown_error s, true -> Printf.sprintf "不明なエラー (%s)" s
  | Unknown_error s, false -> Printf.sprintf "Unknown error (%s)" s

let finalize () =
  Sys.chdir Config.orig_working;
  if Config.sandbox () && Sys.file_exists Config.dir then
    Files.remove_rec Config.dir

let graceful_exit code =
  finalize ();
  exit code

let show_error_and_exit e =
  Printf.printf "%s\n" (message_of e);
  graceful_exit 1

let show_error_and_exit_on_error = function
  | Ok _ -> ()
  | Error e -> show_error_and_exit e
