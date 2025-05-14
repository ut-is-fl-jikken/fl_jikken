open Util
open Assignment
open Error

module Extract = struct
  type base =
    | Tmpdir
    | Cwd
  type kind =
    | Never of { input_dir: string; base: base }
    | Directory of { input_dir: string; base: base }
    | Unzip of { input_filename: string; base: base }
  let extracted_relative_path = function
    | Never { base = Tmpdir; _ } -> None
    | Never { base = Cwd; input_dir } -> Some input_dir
    | Directory { input_dir; _ } -> Some (Filename.basename input_dir)
    | Unzip { input_filename; _ } -> Some (Filename.remove_extension @@ Filename.basename input_filename)
end

let file_organization extract =
  let cmd =
    match extract with
    | Extract.Never _ ->
      None
    | Directory { input_dir; base = Tmpdir } ->
      Option.some @@ Printf.sprintf "cp -r %s %s" input_dir Config.dir
    | Directory { input_dir; base = Cwd } ->
      Option.some @@ Printf.sprintf "cp -r %s ." input_dir
    | Unzip { input_filename; base = Tmpdir } ->
      Option.some @@ Printf.sprintf "unzip -q -d %s %s" Config.dir input_filename
    | Unzip { input_filename; base = Cwd } ->
      Option.some @@ Printf.sprintf "unzip -q %s" input_filename
  in
  let (let*) = Result.bind in
  let (let**) o f =
    match o with
    | None -> Ok ()
    | Some x -> f x
  in
  let* () =
    let** cmd = cmd in
    if Command.run "%s" cmd <> 0 then
      Error Cannot_extract
    else
      Ok ()
  in
  let* extracted_relative =
    match Extract.extracted_relative_path extract with
    | None -> Error Cannot_extract
    | Some x -> Ok x
  in
  let segments =
    if Config.sandbox () then
      Config.dir :: [extracted_relative]
    else
      [extracted_relative]
  in
  let** dir = Files.concat_segments segments in
  Config.file_dir := dir;
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    Error (Directory_not_found extracted_relative)
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

type parse_prolog_state =
  | Skip_until of int
  | Skip_if_warning

let default_parse_prolog_state = Skip_until 0

let parse_prolog_error state error =
  match String.split_on_char ':' error, state with
  | _, Skip_until n when n <> 0 ->
      `Information, Skip_until (n-1)
  | ["ERROR"; _prolog_argument; _error_reporter; " Undefined procedure"; predicate_kind], _ ->
      `Error (Predicate_not_found (String.remove_prefix ~prefix:" " predicate_kind)), default_parse_prolog_state
  | ["ERROR"; _prolog_argument; _error_reporter; " Unknown procedure"; predicate_kind], _ ->
      `Error (Predicate_not_found (String.remove_prefix ~prefix:" " predicate_kind)), Skip_until 2
  | "Warning"::_, _ -> `Warning, Skip_if_warning
  | s::_, Skip_if_warning when s.[0] = '\t' -> `Warning, Skip_if_warning
  | _ -> `Error (Unknown_error error), default_parse_prolog_state
let parse_prolog_errors es =
  let acc_rev,_ =
    ListLabels.fold_left es
      ~init:([],default_parse_prolog_state)
      ~f:(fun (acc_rev,prev) e ->
        let result, state = parse_prolog_error prev e in
        begin match result with
          | `Error e -> e::acc_rev
          | `Warning | `Information -> acc_rev
        end, state)
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
  | Build(main, _) ->
      assert is_dir;
      let exec =
        Option.value main ~default:!Config.executable
        |> Format.sprintf "%s/%s" filename
      in
      let prefix = Config.dir ^ "/" in
      let result =
        let r = Command.run "cd %s; %s && %s" filename !Config.clean !Config.build in
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

type file_result = t * string option Error.result list

let file extract t : file_result =
  let is_dir = is_directory t.kind in
  let open Target in
  let options = options t in
  let paths =
    Files.product_segments @@
    [!Config.file_dir] :: [iter options]
  in
  debug "Check %s@." @@ subject_of t.kind;
  t,
  match paths |> List.find_opt Sys.file_exists with
    | None ->
      let path =
        match Extract.extracted_relative_path extract with
        | None -> show options
        | Some copied_relative_path ->
          Filename.concat copied_relative_path (show options)
      in
      [if is_dir then Directory_not_found path else File_not_found path]
    | Some path ->
      t.items
      |> List.concat_map (check_item ~is_dir path)

let rec passed_mandatory : file_result list -> bool = function
  | [] -> true
  | (t, result) :: xs ->
      List.for_all (function OK _ -> true | _ -> is_optional t.kind) result
      && passed_mandatory xs
