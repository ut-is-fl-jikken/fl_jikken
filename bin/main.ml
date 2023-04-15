open Util
open Assignment

let init () =
  Command_line.parse();
  if not !Config.force && Sys.ocaml_version <> Config.ocaml_version then
    Error Version_mismatch
  else
    begin
      if not @@ Sys.file_exists Config.dir then Sys.mkdir Config.dir 0o755;
      Ok ()
    end

let finalize () =
  Sys.chdir Config.orig_working;
  if Sys.file_exists Config.dir then
    Files.remove_rec Config.dir

let show_error_and_exit = function
  | Ok _ -> ()
  | Error e ->
      Printf.printf "%s\n" (message_of e);
      finalize ();
      exit 1

let show_results (t, items, result) =
  Printf.printf "[%s] " (subject_of t);
  if List.for_all (function OK _ -> true | _ -> false) result then
    let r = List.filter_map (function OK s -> s | _ -> None) result in
    let is_opt = items <> [] && List.for_all (function TypeOpt _ -> true | _ -> false) items in
    match r, is_opt, !Config.jp with
    | [], true, true -> Printf.printf "%s" (Util.TColor.red "NG:" ^ "答えが見つかりません")
    | [], true, false -> Printf.printf "%s" (Util.TColor.red "NG:" ^ " No solution found")
    | [], false, _ -> Printf.printf "%s" (Util.TColor.green "OK")
    | _ -> Printf.printf "%s" (String.concat ", " r)
  else
    result
    |> List.filter (function OK _ -> false | _ -> true)
    |> List.map message_of
    |> List.unique
    |> String.concat ", "
    |> Printf.printf "%s %s" (Util.TColor.red "NG:");
  Printf.printf "\n"

let rec passed_mandatory = function
  | [] -> true
  | (t, _, result) :: xs ->
      List.for_all (function OK _ -> true | _ -> is_hatten t) result
      && passed_mandatory xs

let assiginments =
  [ 1, Week01.assignments;
    2, Week02.assignments;
    3, Week03.assignments;
    4, Week04.assignments;
    5, Week05.assignments;
    6, Week06.assignments;
    7, Week07.assignments;
    8, Week08.assignments;
    9, Week09.assignments;
   10, Week10.assignments;
   11, Week11.assignments;
   12, Week12.assignments;
   13, []]

let assoc_assignments n =
  try
    List.assoc n assiginments
  with Not_found ->
         show_error_and_exit (Error (Unsupported_week_no n));
         assert false

let print_file_struct n =
  let dir = Format.sprintf "%02d-XXXXXX" n in
  let files =
    assoc_assignments n
    |> List.map (fst |- filename_of)
  in
  let report =
    Printf.sprintf "%s.{%s}" Config.report_name (String.concat "|" Config.report_exts)
  in
  let pr f =
    if Filename.remove_extension f = f then
      (Printf.printf "├── %s\n" f;
       Printf.printf "│   └── ...\n")
    else
      Printf.printf "├── %s\n" f
  in
  Printf.printf "%s\n" dir;
  List.iter pr files;
  Printf.printf "└── %s\n" report

let make_env_file archive =
  (* This should go to the Util Module *)
  let (let@) f k = f k in
  let@ cout = Io.CPS.open_out (Printf.sprintf "%s/ENV" archive) in
  let write s1 s2 = output_string cout @@ Printf.sprintf "%s: %s\n" s1 s2 in
  write "ver" Config.version;
  write "date" @@ MyUnix.open_and_read_line "date";
  write "week" (Printf.sprintf "%02d" !Config.no);
  write "ID" !Config.id;
  write "ocaml" Config.ocaml_version;
  write "system" @@ MyUnix.open_and_read_line "uname -a"

let make_archive () =
  let archive = Printf.sprintf "%02d-%s" !Config.no !Config.id in
  let filename = Printf.sprintf "%02d-%s.zip" !Config.no !Config.id in
  Sys.chdir Config.orig_working;
  Sys.chdir Config.dir;
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
  |> show_error_and_exit;

  match !Config.mode with
  | Check ->
      if !Config.file = "" then (Printf.printf "%s\n" Command_line.usage; exit 1);


      Check.file_organization()
      |> show_error_and_exit;

      let results = assoc_assignments !Config.no
                    |> List.map (fun (t,items) -> t, items, Check.file t items)
      in
      List.iter show_results results;
      if not @@ passed_mandatory results then
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
         | Some e -> show_error_and_exit (Error e)
         | None -> finalize ())
  | Print_file_struct n ->
      print_file_struct n;
      finalize()

let () = if not !Sys.interactive then main()
