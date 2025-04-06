let print_version () =
  Printf.printf "fl_jikken %s\n" Config.version

let options =
  let handle_output path = Config.output_dest := Path { path } in
  let handle_format = function
    | "json" -> Config.output_format := Config.Json { pretty = false }
    | "json_pretty" | "json_pp" -> Config.output_format := Config.Json { pretty = true }
    | "human" -> Config.output_format := Config.Human
    | fmt -> raise (Arg.Bad (Printf.sprintf "Unrecognized output format: \"%s\"" fmt))
  in
  let handle_check n =
    if false then
      raise (Arg.Bad (Printf.sprintf "Invalid week number: %d" n))
    else
      Config.mode := Check n
  in
  ["--ignore-version", Arg.Set Config.ignore_version_mismatch, "  Force to proceed on OCaml version mismatch";
   "--force", Arg.Set Config.force_creation, "  Force to create an archive even if the assignment is not completed";
   "-f", Arg.Set Config.force_creation, "  The same as --force";
   "-e", Arg.Clear Config.jp, "  Use English messages";
   "--build", Arg.Set_string Config.build, {|<command>  Use <command> to build ocaml projects instead of "dune build"|};
   "-b", Arg.Set_string Config.build, " The same as --build";
   "--format", Arg.String handle_format, {|<format>  Use <format> to output test status|};
   "--output", Arg.String handle_output, {|<path>  Save to <path> instead of printing to stdout|};
   "-o", Arg.String handle_output, {| The same as --output|};
   "--ci", Arg.Int (fun n -> handle_format "json"; handle_check n), {| Run in CI mode|};
   "--check", Arg.Int handle_check, "<n>  Check the assignment of Week <n> without creating an archive";
   "--disable-sandboxing", Arg.Set Config.disable_sandboxing, " Disable tmp directory sandboxing";
   "-p", Arg.Int (fun n -> Config.mode := Print_file_struct n), "<n>  Print the file structure for the assignment of Week <n>";
   "-v", Arg.Unit (fun () -> print_version (); exit 0), " Output the version";
   "--swipl", Arg.Set_string Config.swipl, "<command>  Set the path to SWI prolog";
   "--verbose", Arg.Unit (fun () -> Log.mode := Verbose), "  verbose mode";
   "--debug", Arg.Unit (fun () -> Log.mode := Debug), "  debug mode"]

let set_file filename =
  if !Config.file <> "" then
    begin
      if !Config.jp then
        Printf.printf "ファイル引数は一つまでです\n"
      else
        Printf.printf "Only one file argument is allowed.\n";
      exit 1
    end;
  Config.file := filename

let usage = Printf.sprintf "Usage: fl_jikken [path-to-submission] --check [week-number]"

let parse () = Arg.parse (Arg.align options) set_file usage
