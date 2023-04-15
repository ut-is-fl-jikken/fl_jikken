module CPS = struct
    let open_in file k =
      let cin = open_in file in
      BatPervasives.finally
        (fun () -> close_in cin)
        k cin
    let open_out file k =
      let cout = open_out file in
      BatPervasives.finally
        (fun () -> close_out cout)
        k cout
end

let input_all = BatPervasives.input_all
let input_file = BatPervasives.input_file
let output_file = BatPervasives.output_file
let copy_file ~src ~dest =
  let text = input_file src in
  output_file ~filename:dest ~text
let empty_file filename = output_file ~filename ~text:""
let rec input_lines cin =
  match input_line cin with
  | s -> s :: input_lines cin
  | exception End_of_file -> []
