include Assignment_types
open Error

let list =
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

let validate_week_no n =
  try
    Ok (List.assoc n list)
  with Not_found ->
    Error (Unsupported_week_no n)

let assoc n =
  try
    List.assoc n list
  with Not_found ->
         show_error_and_exit (Unsupported_week_no n)

module Target = struct
  type t =
    | Exact of string
    | Suffix of { body: string; suffix: string list }
    | Alt of { main: t; alternative: t list }

  let rec iter = function
    | Exact s -> [s]
    | Suffix { body; suffix } ->
        suffix
        |> List.map (Printf.sprintf "%s.%s" body)
    | Alt { main; alternative } ->
        let main = iter main in
        let alternative = List.concat_map iter alternative in
        main @ alternative

  let rec show = function
    | Exact s -> s
    | Suffix { body; suffix } ->
        Printf.sprintf "%s.%s" body @@
        if List.length suffix > 1
        then Printf.sprintf "{%s}" (String.concat "|" suffix)
        else List.hd suffix
    | Alt { main; alternative } ->
        let main = show main in
        let alternative = List.map show alternative in
        Printf.sprintf "%s (otherwise %s)" main @@
        String.concat ", " alternative

  let rec show_first = function
    | Exact s -> s
    | Suffix { body; suffix } ->
        Printf.sprintf "%s.%s" body @@ List.hd suffix
    | Alt { main; _ } ->
        show_first main
end

let options t =
  let options_of_kind = function
    | Report -> Target.Suffix { body = Config.report_name; suffix = Config.report_exts }
    | Toi(ML, n) -> Exact (Printf.sprintf "toi%d.ml" n)
    | Toi(Prolog, n) -> Exact (Printf.sprintf "toi%d.pl" n)
    | Toi(Dir, n) -> Exact (Printf.sprintf "toi%d" n)
    | Hatten(ML, n) -> Exact (Printf.sprintf "hatten%d.ml" n)
    | Hatten(Prolog, n) -> Exact (Printf.sprintf "hatten%d.pl" n)
    | Hatten(Dir, n) -> Exact (Printf.sprintf "hatten%d" n)
  in
  match t.alternative with
  | Some alt when List.length alt <> 0 ->
      Target.Alt { main = options_of_kind t.kind;
                   alternative = List.map options_of_kind alt }
  | _ ->
      options_of_kind t.kind
