let version = "v2024.4"
let no = ref 0
let ocaml_version = "5.1.1"
let orig_working = Sys.getcwd()
let dir = "_fl_tmp_" ^ Util.Unix.string_of_time()
let file_dir = ref (Some "")
let id = ref ""
let force = ref false
let jp = ref true
let file = ref ""
let build = ref "dune build"
let executable = ref "main.exe"
let swipl = ref "swipl"

type mode = Check_and_zip | Check of int | Print_file_struct of int
let mode = ref Check_and_zip

let disable_sandboxing = ref false
let sandbox () = not !disable_sandboxing

type format = Human | Json of { pretty: bool }
let output_format = ref Human

type output_dest = Stdout | Path of { path: string }
let output_dest = ref Stdout

let report_name = "report"
let report_exts = ["txt"; "md"; "pdf"]
