open Util

let run ?filename cmd =
  Log.debug "[Check.command] cwd: %s@." @@ Sys.getcwd ();
  Format.ksprintf (fun cmd ->
    Log.debug "[Check.command] cmd: %s@." cmd;
    let cmd' =
      match filename with
      | None -> Format.sprintf "%s > /dev/null 2> /dev/null" cmd
      | Some file -> Format.sprintf "%s > %s.out 2> %s.err" cmd file file
    in
    Log.debug "[Check.command] cmd': %s@." cmd';
    let r = Sys.command cmd' in
    Log.debug "[Check.command] r: %d@." r;
    r) cmd


let mv src dst =
  ignore @@ run "mv %s %s" (String.concat " " src) dst
