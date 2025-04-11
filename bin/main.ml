open Util
open Assignment
open Error

let init () =
  Command_line.parse();
  if not !Config.ignore_version_mismatch && Sys.ocaml_version <> Config.ocaml_version then
    Error Version_mismatch
  else
    begin
      if Config.sandbox () && not @@ Sys.file_exists Config.dir then Sys.mkdir Config.dir 0o755;
      Ok ()
    end

let show_results oc (t, result) =
  Printf.fprintf oc "[%s] " (subject_of t.kind);
  if List.for_all (function OK _ -> true | _ -> false) result then
    let r = List.filter_map (function OK s -> s | _ -> None) result in
    let is_opt = t.items <> [] && List.for_all (function TypeOpt _ -> true | _ -> false) t.items in
    match r, is_opt, !Config.ja with
    | [], true, true -> Printf.fprintf oc "%s" (Util.TColor.red "NG:" ^ "答えが見つかりません")
    | [], true, false -> Printf.fprintf oc "%s" (Util.TColor.red "NG:" ^ " No solution found")
    | [], false, _ -> Printf.fprintf oc "%s" (Util.TColor.green "OK")
    | _ -> Printf.fprintf oc "%s" (String.concat ", " r)
  else
    result
    |> List.filter (function OK _ -> false | _ -> true)
    |> List.map message_of
    |> List.unique
    |> String.concat ", "
    |> Printf.fprintf oc "%s %s" (Util.TColor.red "NG:");
  Printf.fprintf oc "\n"

let pack_results (t, result) =
  let errors =
    if List.for_all (function OK _ -> true | _ -> false) result then
      let r = List.filter_map (function OK s -> s | _ -> None) result in
      let is_opt = t.items <> [] && List.for_all (function TypeOpt _ -> true | _ -> false) t.items in
      match r, is_opt, !Config.ja with
      | [], true, true -> ["答えが見つかりません"]
      | [], true, false -> ["No solution found"]
      | [], false, _ -> []
      | _ -> r
    else
      result
      |> List.filter (function OK _ -> false | _ -> true)
      |> List.map message_of
      |> List.unique
  in
  `Assoc [
    ("id", `String (subject_id_of t.kind));
    ("is_ok", `Bool (List.is_empty errors));
    ("errors", `List (errors |> List.map (fun s -> `String s)));
  ]

let output_results results =
  let oc, close =
    match !Config.output_dest with
    | Stdout -> stdout, false
    | Path { path } -> open_out path, true
  in
  begin match !Config.output_format with
  | Human ->
    List.iter begin function results ->
      show_results oc results
    end results;
  | Json { pretty } ->
    let list =
      results
      |> List.map pack_results
    in
    let json = `List list in
    if pretty then
      Yojson.Safe.pretty_to_channel oc json
    else
      Yojson.Safe.to_channel oc json
  end;
  if close then close_out oc

let print_file_structure n =
  let dir = Format.sprintf "%02d-XXXXXX" n in
  let assignments = assoc n in
  let print is_final t =
    let f = Target.show (options t) in
    if is_final then
      if is_directory t.kind then
        (Printf.printf "└── %s\n" f;
         Printf.printf "    └── ...\n")
      else
        Printf.printf "└── %s\n" f
    else
      if is_directory t.kind then
        (Printf.printf "├── %s\n" f;
         Printf.printf "│   └── ...\n")
      else
        Printf.printf "├── %s\n" f
  in
  Printf.printf "%s\n" dir;
  if not @@ List.is_empty assignments then
    let assignments, last =
      let rev = List.rev assignments in
      List.rev (List.tl rev), List.hd rev
    in
    List.iter (print false) assignments;
    print true last
  else
    Printf.printf "└── (empty)\n"

let create_file_structure n mode =
  let assignments = assoc n in
  let create t =
    let f = Target.show_first (options t) in
    let early result =
      if result <> 0 then
        show_error_and_exit (Creation_failed f)
      else ()
    in
    if is_directory t.kind then begin
      if !Config.overwrite then early @@ Command.run "rm -rf %s" f;
      early @@ Command.run "mkdir -p %s" f
    end else begin
      if !Config.overwrite then early @@ Command.run "rm -f %s" f;
      early @@ Command.run "touch %s" f;
    end
  in
  let remove t =
    let f = Target.show_first (options t) in
    if is_directory t.kind then begin
      ignore @@ Command.run "rm -rf %s" f
    end else begin
      ignore @@ Command.run "rm -f %s" f
    end
  in
  match mode with
  | `Create -> List.iter create assignments
  | `Remove -> List.iter remove assignments

let make_env_file archive =
  (* This should go to the Util Module *)
  let@ cout = Io.CPS.open_out (Printf.sprintf "%s/ENV" archive) in
  let write s1 s2 = output_string cout @@ Printf.sprintf "%s: %s\n" s1 s2 in
  write "ver" Config.version;
  write "date" @@ Unix.open_and_read_line "date";
  write "week" (Printf.sprintf "%02d" !Config.no);
  write "ID" !Config.id;
  write "ocaml" Config.ocaml_version;
  write "system" @@ Unix.open_and_read_line "uname -a"

let make_archive () =
  let archive = Printf.sprintf "%02d-%s" !Config.no !Config.id in
  let filename = Printf.sprintf "%02d-%s.zip" !Config.no !Config.id in
  Sys.chdir Config.orig_working;
  if Config.sandbox () then Sys.chdir Config.dir;
  make_env_file archive;
  let r = Command.run ~filename:"zip" "zip -r %s %s" filename archive in
  if 0 <> r then
    (Command.mv ["zip.err"] Config.orig_working;
    Some Zip_failed)
  else
    (Command.mv [filename] Config.orig_working;
     let message = if !Config.ja then
       Printf.sprintf "%sを作成しました\n" filename
     else
       Printf.sprintf "Created %s\n" filename
     in
     let message = Util.TColor.green message in
     Printf.printf "%s" message;
     None)

let main () =
  (*  *Log.mode := Log.Debug; *)
  init()
  |> show_error_and_exit_on_error;

  match !Config.mode with
  | Unset -> Command_line.show_usage_and_exit ()
  | Check_and_zip filename_info ->
      (* validate input *)
      if !Config.file = "" then Command_line.show_usage_and_exit ();
      let target_info = match filename_info.target with
        | Some target_info -> target_info
        | None -> show_error_and_exit (File_name_invalid filename_info.input_filename)
      in

      Config.no := target_info.week_number;
      Config.id := target_info.id;
      let copy_method =
        let base = if Config.sandbox () then Check.Extract.Tmpdir else Cwd in
        if target_info.for_dir then
          Check.Extract.Directory { input_dir = filename_info.input_filename; base }
        else
          Check.Extract.Unzip { input_filename = filename_info.input_filename; base }
      in
      Check.file_organization copy_method
      |> show_error_and_exit_on_error;

      let results = assoc !Config.no
                    |> List.map (Check.file copy_method)
      in
      output_results results;
      if (not !Config.force_creation) && (not @@ Check.passed_mandatory results) then
        (let message = if !Config.ja then
                         "zipファイルを生成しませんでした"
                       else
                         "Did not generate zip file"
         in
         let message = Util.TColor.red message in
         Printf.printf "%s\n" message;
         finalize ())
      else
        (match make_archive () with
         | Some e -> show_error_and_exit e
         | None -> finalize ())
  | Check week_number ->
      let apply_default_filename s =
        if s = "" then "." else s
      in
      let input_filename = !Config.file in
      let copy_method =
        let base = if Config.sandbox () then Check.Extract.Tmpdir else Cwd in
        if !Config.disable_sandboxing then
          Check.Extract.Never { input_dir = apply_default_filename input_filename; base }
        else
          if input_filename = "" then
            show_error_and_exit No_input_file
          else
            Check.Extract.Directory { input_dir = apply_default_filename input_filename; base }
      in

      Config.no := week_number;
      Check.file_organization copy_method
      |> show_error_and_exit_on_error;

      let results = assoc week_number
                    |> List.map (Check.file copy_method)
      in
      output_results results;

      finalize ()
  | File_struct { week_number; create } ->
      if create then
        let apply_default_filename s =
          if s = "" then "." else s
        in
        let input_dirname = apply_default_filename !Config.file in
        if not !Config.undo then
          if Sys.file_exists input_dirname && not @@ Sys.is_directory input_dirname then
            show_error_and_exit (File_exists input_dirname)
          else begin
            ignore @@ Command.run "mkdir -p %s" input_dirname;
            Sys.chdir input_dirname;
            create_file_structure week_number `Create
          end
        else
          if not @@ Sys.file_exists input_dirname then
            show_error_and_exit (Directory_not_found_cannot_undo input_dirname)
          else if not @@ Sys.is_directory input_dirname then
            show_error_and_exit (File_exists_cannot_undo input_dirname)
          else begin
            Sys.chdir input_dirname;
            create_file_structure week_number `Remove
          end
      else begin
        if !Config.file <> "" then Command_line.show_usage_and_exit ();
        print_file_structure week_number;
      end;
      finalize()

let () = if not !Sys.interactive then main()
