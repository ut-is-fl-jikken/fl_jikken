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
    match r, is_opt, !Config.jp with
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
      match r, is_opt, !Config.jp with
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

let rec passed_mandatory = function
  | [] -> true
  | (t, result) :: xs ->
      List.for_all (function OK _ -> true | _ -> is_optional t.kind) result
      && passed_mandatory xs

let print_file_struct n =
  let dir = Format.sprintf "%02d-XXXXXX" n in
  let assignment_ids = assoc n |> List.map (fun t -> t.kind)
  in
  let print is_final t =
    let f = Target.show (options t) in
    if is_final then
      if is_directory t then
        (Printf.printf "└── %s\n" f;
        Printf.printf "    └── ...\n")
      else
        Printf.printf "└── %s\n" f
    else
      if is_directory t then
        (Printf.printf "├── %s\n" f;
        Printf.printf "│   └── ...\n")
      else
        Printf.printf "├── %s\n" f
  in
  Printf.printf "%s\n" dir;
  let rev = List.rev assignment_ids in
  let last = List.hd rev in
  let assignment_ids = List.rev (List.tl rev) in
  List.iter (print false) assignment_ids;
  print true last

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
     let message = if !Config.jp then
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
  | Check_and_zip ->
      (* validate input *)
      if !Config.file = "" then (Printf.printf "%s\n" Command_line.usage; exit 1);
      let filename_info = Check.filename !Config.file in
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
                    |> List.map (fun t -> t, Check.file copy_method t)
      in
      output_results results;
      if (not !Config.force_creation) && (not @@ passed_mandatory results) then
        (let message = if !Config.jp then
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
                    |> List.map (fun t -> t, Check.file copy_method t)
      in
      output_results results;

      finalize ()
  | Print_file_struct n ->
      print_file_struct n;
      finalize()

let () = if not !Sys.interactive then main()
