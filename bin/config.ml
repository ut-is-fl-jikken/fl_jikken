let version = "v2024.4"
let no = ref 0
let ocaml_version = "5.1.1"
let orig_working = Sys.getcwd()
let dir = "_fl_tmp_" ^ Util.Unix.string_of_time()
let file_dir = ref (Some "")
let id = ref ""
let ignore_version_mismatch = ref false
let force_creation = ref false
let jp = ref true
let file = ref ""
let clean = ref "dune clean"
let build = ref "dune build"
let executable = ref "main.exe"
let swipl = ref "swipl"

type target_info = {
  week_number : int;
  id : string;
  for_dir : bool;
}

type filename_info = {
  input_filename : string;
  target : target_info option;
}

type mode = Unset | Check_and_zip of filename_info | Check of int | Print_file_struct of int
let mode = ref Unset

let disable_sandboxing = ref false
let sandbox () = not !disable_sandboxing

type format = Human | Json of { pretty: bool }
let output_format = ref Human

type output_dest = Stdout | Path of { path: string }
let output_dest = ref Stdout

let report_name = "report"
let report_exts = ["txt"; "md"; "pdf"]
