let (|@>) x f = f x; x
let (|-) f g x = g (f x)

module List = struct
  include List

  let unique xs = List.fold_right (fun x acc -> if List.mem x acc then acc else x::acc) xs []

  let last_opt xs = List.fold_left (fun _ x -> Some x) None xs

  let rec drop n = function
    | _ :: l when n > 0 -> drop (n-1) l
    | l -> l
end

module Char = struct
  let is_int_char c =
    '0' <= c && c <= '9'

  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false
end

module String = struct
  include String

  let is_int_string s =
    match int_of_string s with
    | _ -> true
    | exception Invalid_argument _ -> false

  let starts_with s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    if len1 < len2 then
      false
    else
      String.sub s1 0 len2 = s2

  let ends_with s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    if len1 < len2 then
      false
    else
      String.sub s1 (len1-len2) len2 = s2

  let shrink_spaces s =
    s
    |> String.to_seq
    |> Seq.fold_left (fun acc c ->
           match Char.is_space c, acc with
           | true, ' '::_ -> acc
           | _ -> c::acc) []
    |> List.rev
    |> List.to_seq
    |> String.of_seq
    |> String.trim

  let remove_prefix ~prefix s =
    let len = String.length prefix in
    assert (String.sub s 0 len = prefix);
    String.sub s len (String.length s - len)
end

module Files = struct
  let rec remove_rec filename =
    if Sys.is_directory filename then
      let () =
        Sys.readdir filename
        |> Array.map (fun s -> filename ^ "/" ^ s)
        |> Array.iter remove_rec
      in
      Sys.rmdir filename
    else
      Sys.remove filename

  let rec find ?(filename=".") postfix =
    if Sys.is_directory filename then
      Sys.readdir filename
      |> Array.to_list
      |> List.find_map (fun s -> find ~filename:(filename ^ "/" ^ s) postfix)
    else if String.ends_with filename postfix then
      Some filename
    else
      None
end

let rec input_lines cin =
  match input_line cin with
  | s -> s :: input_lines cin
  | exception End_of_file -> []

let time () =
  let {Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _} = Unix.localtime @@ Unix.time() in
  Printf.sprintf "%04d%02d%02d%02d%02d%02d" (tm_year+1900) (tm_mon+1) tm_mday tm_hour tm_min tm_sec

let debug_mode = Sys.getenv_opt "DEBUG" <> None
let debug f =
  if debug_mode then Format.printf f else Format.ifprintf Format.std_formatter f

module TColor = struct
(*  *Character codes in OCaml are in decimal not in octal, so we use \027 instead of \033 *)
let nc = "\027[0m"
let red str = "\027[0;31m" ^ str ^ nc
let green str = "\027[0;32m" ^ str ^ nc
end
