let print_version () =
  Printf.printf "fl_jikken %s\n" Config.version

let options =
  ["-f", Arg.Set Config.force, "";
   "-e", Arg.Clear Config.jp, "";
   "--build", Arg.Set_string Config.build, {|<command>  Use <command> to build ocaml projects instead of "dune build"|};
   "-b", Arg.Set_string Config.build, " The same as --build";
   "-p", Arg.Int (fun n -> Config.mode := Print_file_struct n), "<n>  Print the file structure for the assignment of Week <n>";
   "-v", Arg.Unit (fun () -> print_version (); exit 0), " Output the version";
   "--swipl", Arg.Set_string Config.swipl, "<command>  Set the path to SWI prolog";
   "--verbose", Arg.Unit (fun () -> Log.mode := Verbose), "";
   "--debug", Arg.Unit (fun () -> Log.mode := Debug), ""]

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

let usage = Printf.sprintf "Usage: fl_jikken XX-YYYYYY.zip"

let parse () = Arg.parse (Arg.align options) set_file usage
