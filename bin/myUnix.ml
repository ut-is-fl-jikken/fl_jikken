include Unix
module CPS = struct
  let open_process ?(check=ignore) cmd k =
    let ch = open_process cmd in
    let r = k ch in
    check (close_process ch);
    r
  let open_process_in ?(check=ignore) cmd k =
    let cin = open_process_in cmd in
    let r = k cin in
    let st = close_process_in cin in
    check st;
    r
end

let open_and_read_line cmd = CPS.open_process_in cmd input_line
let open_and_read_all cmd = CPS.open_process_in cmd Io.input_all
let open_and_read_lines cmd = CPS.open_process_in cmd Io.input_lines
let string_of_time () =
  let {tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _} = localtime @@ time() in
  Printf.sprintf "%04d%02d%02d%02d%02d%02d" (tm_year+1900) (tm_mon+1) tm_mday tm_hour tm_min tm_sec
