open Util
open Assignment

type target_info = {
  week_number : int;
  id : string;
  for_dir : bool;
}

type filename_info = {
  input_filename : string;
  target : target_info option;
}

let filename s : filename_info =
  let for_dir = Sys.is_directory s in
  let ext = if for_dir then None else Some "zip" in
  let ext_removed = match ext with
    | None -> Some s
    | Some e when String.ends_with s ("."^e) -> Some String.(sub s 0 (length s - length e - 1))
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
              if Seq.fold_left (fun acc c -> acc && Char.is_int_char c) true (String.to_seq id) then
                Some { week_number; id; for_dir }
              else
                None
          | exception Invalid_argument _ -> None
        else
          None
  in
  { input_filename = s; target }

type file_copy_method =
  | Never
  | Directory of { input_dir: string }
  | Unzip of { input_filename: string }
let copied_relative_path copy_method =
  begin match copy_method with
    | Never -> None
    | Directory { input_dir } -> Some input_dir
    | Unzip { input_filename } -> Some input_filename
  end
  |> Option.map (fun input_filename -> Filename.remove_extension @@ Filename.basename input_filename)

let file_organization copy_method =
  let cmd =
    match copy_method with
    | Never ->
      None
    | Directory { input_dir } ->
      Option.some @@ Printf.sprintf "cp -r %s %s" input_dir Config.dir
    | Unzip { input_filename } ->
      Option.some @@ Printf.sprintf "unzip -q -d %s %s" Config.dir input_filename
  in
  let copied_relative = copied_relative_path copy_method in
  match cmd with
    | None -> Ok ()
    | Some cmd ->
      debug "cmd: %s@." cmd;
      if Sys.command cmd <> 0 then
        Error Cannot_extract
      else
        let segments = Option.to_list copied_relative in
        let segments =
          if Config.sandbox () then
            Config.dir :: segments
          else
            segments
        in
        let dir = Files.concat_segments segments in
        Config.file_dir := dir;
        match dir with
          | None -> Ok ()
          | Some dir ->
            if not (Sys.file_exists dir && Sys.is_directory dir) then
              Error (Directory_not_found (Option.value copied_relative ~default:(failwith "unreachable because of `mkdir Config.dir`")))
            else if not @@ List.exists (fun ext -> Sys.file_exists (Printf.sprintf "%s/%s.%s" dir Config.report_name ext)) Config.report_exts then
              Error (File_not_found (Config.report_name ^ ".*"))
            else
              Ok ()

let count_leading_spaces s =
  let rec loop i c s =
    if String.length s <= i || s.[i] <> ' ' then
      c
    else
      loop (i+1) (c+1) s
  in
  loop 0 0 s

let rec normalize_output acc_rev ss =
  match ss with
  | [] -> List.rev acc_rev
  | ""::ss' -> normalize_output acc_rev ss'
  | s::ss' when String.starts_with s "Hint: " -> normalize_output acc_rev ss'
  | s::ss' when String.starts_with s "  " && acc_rev <> [] ->
      let len = count_leading_spaces s in
      let s' = List.hd acc_rev ^ String.sub s (len-1) (String.length s - len + 1) in
      let acc_rev' = s' :: List.tl acc_rev in
      normalize_output acc_rev' ss'
  | s::ss' when s.[0] = '=' && acc_rev <> [] ->
      let s' = List.hd acc_rev ^ " " ^ s in
      let acc_rev' = s' :: List.tl acc_rev in
      normalize_output acc_rev' ss'
  | s::ss' -> normalize_output (s::acc_rev) ss'

let parse_ocaml_output s =
  let map =
    ["Error: Unbound value ", (fun x -> Error (Value_not_found x));
     "Error: Unbound type constructor ", (fun x -> Error (Type_not_found x));
     "Error: Unbound constructor ", (fun x -> Error (Constructor_not_found x));
     "Error: Unbound module ", (fun x -> Error (Module_not_found x));
     "Error: Signature mismatch:", (fun x -> Error (Type_mismatch x));
     "- : ", Result.ok;
     "type ", Result.ok;
     "exception ", Result.ok;
     "module ", Result.ok]
  in
  match List.find_opt (fun (p,_) -> String.starts_with s p) map with
  | None -> Error (Unknown_error s)
  | Some(prefix, f) ->
      let len = String.length prefix in
      f @@ String.sub s len (String.length s - len)

let parse_prolog_error prev_is_warning error =
  match String.split_on_char ':' error with
  | ["ERROR"; _s2; _s3; " Undefined procedure"; s5] ->
      `Error (Predicate_not_found (String.remove_prefix ~prefix:" " s5))
  | "Warning"::_ -> `Warning
  | s::_ when prev_is_warning && s.[0] = '\t' -> `Warning
  | _ -> `Error (Unknown_error error)
let parse_prolog_errors es =
  let acc_rev,_ =
    ListLabels.fold_left es
      ~init:([],false)
      ~f:(fun (acc_rev,prev) e ->
        match parse_prolog_error prev e with
        | `Error e -> e::acc_rev, false
        | `Warning -> acc_rev, true)
  in
  List.rev acc_rev

let eval_prolog_file filename query =
  debug "[eval_prolog_file] cwd: %s@." @@ Sys.getcwd ();
  debug "[eval_prolog_file]: %s %s@." filename (if Sys.file_exists filename then "exists" else "does not exist");
  let cmd = Printf.sprintf "%s -s %s -g '%s' -t halt" !Config.swipl filename query in
  let cin,cout,cerr = Unix.open_process_full cmd [||] in
  let result = input_lines cin in
  let error = input_lines cerr in
  ignore @@ Unix.close_process_full (cin,cout,cerr);
(*
  let aux s =
    let prefix = "ERROR: " in
    if String.starts_with prefix s then
      Either.Right (String.remove_prefix ~prefix s)
    else
      Either.Left s
  in
  let r,errors = List.partition_map aux ss in
 *)
  debug "query: %s@." query;
  debug "cmd: %s@." cmd;
  List.iter (debug "output: %s@.") result;
  List.iter (debug "errors: %s@.") error;
  debug "@.";
  result, error

let eval_ml_file filename x =
  let cmd = Printf.sprintf "ocaml -noprompt -color never -init %s" filename in
  let cin,cout = Unix.open_process cmd in
  output_string cout (x ^ ";;\n");
  close_out cout;
  let s,ss_rev =
    match
      input_lines cin
      |@> List.iter (debug "  output: %s@.")
      |> normalize_output []
      |> List.rev
    with
    | [] -> assert false
    | s::ss -> s, ss
  in
  debug "@.";
  close_in cin;
  let result = parse_ocaml_output s in
  let errors =
    ss_rev
    |> List.filter (fun s -> String.starts_with s "Error: ")
    |> List.map parse_ocaml_output
    |> List.map Result.get_error
  in
  match result with
  | Ok s -> Ok(s, errors)
  | Error e -> Error (e::errors)

let check_item filename ?(is_dir=Sys.is_directory filename) item =
  match item with
  | ValDef v ->
      begin
        match eval_ml_file filename v with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | Value(x,v) ->
      begin
        match eval_ml_file filename x with
        | Ok(v', _) when String.ends_with v' (" = " ^ v) -> [OK None]
        | Ok _ -> [Incorrect_result]
        | Error es -> es
      end
  | Type(v, ty) ->
      begin
        match eval_ml_file filename @@ Printf.sprintf "(%s : %s)" v ty with
        | Ok _ -> [OK None]
        | Error (Type_mismatch _ :: es) -> Type_mismatch v :: es
        | Error es -> es
      end
  | TypeOpt(v, ty) ->
      begin
        match eval_ml_file filename @@ Printf.sprintf "(%s : %s)" v ty with
        | Ok _ -> [OK (Some v)]
        | _ -> [OK None]
      end
  | TypeDef(n, ty) ->
      let param =
        assert (n >= 0);
        match n with
        | 0 -> ""
        | 1 -> "'a "
        | _ ->
            List.init n (fun i -> Printf.sprintf "'%c" @@ char_of_int (int_of_char 'a' + i))
            |> String.concat ", "
            |> Printf.sprintf "(%s) "
      in
      begin
        match eval_ml_file filename @@ Printf.sprintf ("type %st = %s%s") param param ty with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | ModDef(m,e) ->
      begin
        match eval_ml_file filename ("module M = "^m) with
        | Ok _ -> [OK None]
        | Error (Type_mismatch _::es) -> Type_mismatch (Option.value ~default:m e)::es
        | Error es -> es
      end
  | Module(m, ty) ->
      begin
        match eval_ml_file filename @@ Printf.sprintf "module M = (%s : %s)" m ty with
        | Ok _ -> [OK None]
        | Error (Type_mismatch _::es) -> Type_mismatch m::es
        | Error es -> es
      end
  | Excep e ->
      begin
        match eval_ml_file filename @@ Printf.sprintf "exception E = %s" e with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | CurryUncurry(h, f) ->
      begin
        match
          let (let*) r f = Result.map f r in
          let* _ = eval_ml_file filename "curry" in
          let* _ = eval_ml_file filename "uncurry" in
          let* s1 = eval_ml_file filename @@ Printf.sprintf "%s %s" h f in
          let* s2 = eval_ml_file filename @@ Printf.sprintf "%s (curry (uncurry %s))" h f in
          if s1 <> s2 then
            Ok("",[])
          else
            Error [Incorrect_result]
        with
        | Ok _ -> [OK None]
        | Error es -> es
      end
  | Build(main, except) ->
      assert is_dir;
      let exec =
        Option.value main ~default:!Config.executable
        |> Format.sprintf "%s/%s" filename
      in
      let prefix = Config.dir ^ "/" in
      let object_files = [".exe";".cmo";".cmx";".cma";".cmxa";".cmxs";".cmt";".cmti";".cmi";".o";".a"] in
      let result =
        let files =
          object_files
          |> List.filter_map (Files.find ~filename)
          |> List.filter (fun f -> not @@ List.mem (Filename.basename f) except)
        in
        match files with
        | file::_ ->
            Object_file_found (String.remove_prefix ~prefix file)
        | [] ->
            let r = Sys.command @@ Printf.sprintf "cd %s; %s > /dev/null 2>&1" filename !Config.build in
            if r <> 0 then
              Build_failed
            else if Sys.file_exists exec then
              OK None
            else
              File_not_found_after_build (String.remove_prefix ~prefix exec)
      in
      [result]
  | Exec tests ->
      let exec = Format.sprintf "%s/%s" filename !Config.executable in
      if not @@ Sys.file_exists exec then
        [OK None] (* Bulid must be checked before this check *)
      else
        let cin,cout,cerr = Unix.open_process_full exec [||] in
        let run acc (input,expected) =
          acc &&
            (output_string cout input;
             output_string cout "\n";
             debug "  input: %S@." input;
             flush cout;
             let output =
               input_line cin
               |> String.trim
             in
             debug "  output:   %S@." output;
             debug "  expected: %S@." expected;
             output = String.trim expected)
        in
        let r =
          match List.fold_left run true tests with
          | true -> OK None
          | false | exception _ -> Incorrect_result
        in
        debug "@.";
        ignore @@ Unix.close_process_full (cin,cout,cerr);
        [r]
  | Predicate(p, arity) ->
      let query =
        List.init arity (Printf.sprintf "X%d")
        |> String.concat ","
        |> Printf.sprintf "%s(%s)." p
      in
      let _rs,es = eval_prolog_file filename query in
      if es = [] then
        [OK None]
      else
        parse_prolog_errors es
  | Query(query, expect) ->
      let rs,es = eval_prolog_file filename query in
      let r =
        if expect = rs then
          [OK None]
        else if es <> [] then
          parse_prolog_errors es
        else
          [Incorrect_result]
      in
      r


let file copy_method t items =
  let is_dir = is_directory t in
  let filename =
    Option.to_list !Config.file_dir @ [filename_of t]
    |> Files.concat_segments 
    |> Option.get
  in
  debug "Check %s@." @@ subject_of t;
  if not @@ Sys.file_exists filename then
    let path = 
      (Option.to_list (copied_relative_path copy_method)) @ [filename_of t]
      |> Files.concat_segments
      |> Option.get in
    [if is_dir then Directory_not_found path else File_not_found path]
  else
    items
    |> List.concat_map (check_item ~is_dir filename)
