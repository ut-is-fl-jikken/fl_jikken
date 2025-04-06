open Error

let print_version () =
  Printf.printf "fl_jikken %s\n" Config.version

let print_version_and_exit () =
  print_version ();
  exit 0

type subcommand = Check_and_zip | Check | Print_file_struct

type parse_state =
  | Init
  | Subcommand
  | Accept_file_or_end

let state = ref Init
let mode = ref (None: subcommand option)
module Gated = struct
  let to_string = function
    | Check_and_zip -> "zip"
    | Check -> "check"
    | Print_file_struct -> "print"

  let equal proper = function
    | Some actual when actual = proper -> true
    | _ -> false

  let not_equal proper = function
    | Some actual when actual <> proper -> true
    | _ -> false

  let matches = function
    | `If p -> equal p
    | `Not p -> not_equal p

  let check subcommand arg delegate =
    let actual = !mode in
    if matches subcommand actual then
      delegate ()
    else
      show_error_and_exit (
        match subcommand, actual with
        | `Not _, Some actual -> Illegal_option_not_in_subcommand { actual = to_string actual; option = arg }
        | `If proper, Some actual -> Illegal_option_in_subcommand { actual = to_string actual; proper = to_string proper; option = arg }
        | `If proper, None -> Option_too_early { proper = to_string proper; option = arg }
        | `Not _, None -> Invalid_option arg
      )

  let string ~cond arg spec msg = arg, Arg.String (fun s -> check cond arg @@ fun () -> spec s), msg
  let set_string ~cond arg ref msg = arg, Arg.String (fun s -> check cond arg @@ fun () -> ref := s), msg
  let set ~cond arg ref msg = arg, Arg.Unit (fun () -> check cond arg @@ fun () -> ref := true), msg
  let clear ~cond arg ref msg = arg, Arg.Unit (fun () -> check cond arg @@ fun () -> ref := false), msg
  let unit ~cond arg spec msg = arg, Arg.Unit (fun () -> check cond arg @@ spec), msg
end

let set_file filename =
  if !Config.file <> "" then
    begin
      if !Config.ja then
        Printf.printf "ファイル引数は一つまでです\n"
      else
        Printf.printf "Only one file argument is allowed.\n";
      exit 1
    end;
  Config.file := filename

let options =
  let handle_output path = Config.output_dest := Path { path } in
  let handle_format = function
    | "json" -> Config.output_format := Config.Json { pretty = false }
    | "json_pretty" | "json_pp" -> Config.output_format := Config.Json { pretty = true }
    | "human" -> Config.output_format := Config.Human
    | fmt -> raise (Arg.Bad (Printf.sprintf "Unrecognized output format: \"%s\"" fmt))
  in
  ["--ignore-version", Arg.Set Config.ignore_version_mismatch, "  Force to proceed on OCaml version mismatch";
   "-e", Arg.Clear Config.ja, "  Use English messages";
   "--version", Arg.Unit print_version_and_exit, " Output the version";
   "-v", Arg.Unit print_version_and_exit, "  The same as --version";
   "--verbose", Arg.Unit (fun () -> Log.mode := Verbose), "  verbose mode";
   "--debug", Arg.Unit (fun () -> Log.mode := Debug), "  debug mode";

   "--path", Arg.String set_file, "<path>  Set the path to the assignment directory";
   "-p", Arg.String set_file, "  The same as --path";

   Gated.set "--force" Config.force_creation "  Force to create an archive even if the assignment is not completed"
     ~cond:(`If Check_and_zip);
   Gated.set "-f" Config.force_creation "  The same as --force"
     ~cond:(`If Check_and_zip);

   Gated.set_string "--swipl" Config.swipl "<command>  Set the path to SWI prolog"
     ~cond:(`Not Print_file_struct);
   Gated.set_string "--build" Config.build {|<command>  Use <command> to build ocaml projects instead of "dune build"|}
     ~cond:(`Not Print_file_struct);
   Gated.set_string "-b" Config.build " The same as --build"
     ~cond:(`Not Print_file_struct);
   Gated.set_string "--clean" Config.clean {|<command>  Use <command> to clean ocaml projects before building instead of "dune clean"|}
     ~cond:(`Not Print_file_struct);
   Gated.string "--format" handle_format {|<format>  Use <format> to output test status|}
     ~cond:(`Not Print_file_struct);
   Gated.string "--output" handle_output {|<path>  Save to <path> instead of printing to stdout|}
     ~cond:(`Not Print_file_struct);
   Gated.string "-o" handle_output {| The same as --output|}
     ~cond:(`Not Print_file_struct);
   Gated.set "--disable-sandboxing" Config.disable_sandboxing " Disable tmp directory sandboxing"
     ~cond:(`Not Print_file_struct);

   Gated.unit "--ci" (fun () -> handle_format "json") {| Run in CI mode|}
     ~cond:(`If Check);]

let arg_parser input =
  (* parses non-option arguments *)
  match !state, !mode with
  | Init, _ ->
    begin
      match input with
      | "check" ->
        mode := Some Check;
        state := Subcommand;
      | "zip" ->
        mode := Some Check_and_zip;
        state := Subcommand;
      | "print" ->
        mode := Some Print_file_struct;
        state := Subcommand;
      | s ->
        show_error_and_exit (Unknown_subcommand s);
    end
  | Subcommand, None -> failwith "unreachable"
  | Subcommand, Some Check ->
    begin
      match int_of_string_opt input with
      | Some n ->
        show_error_and_exit_on_error (Assignment.validate_week_no n);
        Config.mode := Check n;
        state := Accept_file_or_end;
      | None ->
        show_error_and_exit (Bad_week_number input);
    end
  | Subcommand, Some Print_file_struct ->
    begin
      match int_of_string_opt input with
      | Some n ->
        show_error_and_exit_on_error (Assignment.validate_week_no n);
        Config.mode := Print_file_struct n;
        Config.disable_sandboxing := true;
        (* Disable sandboxing for print command *)
        state := Accept_file_or_end;
      | None ->
        show_error_and_exit (Bad_week_number input);
    end
  | Subcommand, Some Check_and_zip ->
    begin
      let s = input in
      let for_dir = Sys.is_directory s in
      let ext = if for_dir then None else Some "zip" in
      let ext_removed = match ext with
        | None -> Some s
        | Some e when Util.String.ends_with s ("."^e) -> Some String.(sub s 0 (length s - length e - 1))
        | _ -> None
      in
      let target =
        match ext_removed
        with
        | None -> None
        | Some s ->
            let s = Filename.basename s in
            if String.length s = 9 && s.[2] = '-' then
              match int_of_string (String.sub s 0 2) with
              | week_number ->
                  let id = String.sub s 3 (String.length s - 3) in
                  if Seq.fold_left (fun acc c -> acc && Util.Char.is_int_char c) true (String.to_seq id) then
                    Some ({ week_number; id; for_dir }: Config.target_info)
                  else
                    None
              | exception Invalid_argument _ -> None
            else
              None
      in

      Config.mode := Check_and_zip { input_filename = s; target };
      state := Accept_file_or_end;
    end
  | Accept_file_or_end, _ -> set_file input

let usage_en = Printf.sprintf
{|Usage: fl_jikken check <week_number> [path_to_assignment="."] [options]
            -- Check the assignment of Week <week_number> without creating an archive
       fl_jikken zip XX-YYYYYY [options]
            -- Create an archive of the assignment (obsolete)
       fl_jikken print <week_number>
            -- Print the file structure for the assignment of Week <week_number>
|}
let usage_ja = Printf.sprintf
{|Usage: fl_jikken check <週番号> [課題のパス="."] [他のオプション]
            -- 第 <週番号> 週の課題のチェックを行う (アーカイブを作成しない)
       fl_jikken zip XX-YYYYYY [オプション]
            -- 課題をチェックし、アーカイブを作成する (古い形式)
       fl_jikken print <週番号>
            -- 第 <週番号> 週の課題のファイル構成を表示する
|}
let usage () =
  if !Config.jp then
    usage_ja
  else
    usage_en

let show_usage_and_exit () =
  Printf.printf "%s\n" (usage ());
  if !Config.ja then
    Printf.printf "使い方: fl_jikken --help\n"
  else
    Printf.printf "Use --help for more information.\n"
  ; graceful_exit 1

let parse () = Arg.parse (Arg.align options) arg_parser usage_en
