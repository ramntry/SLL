let read_file fname =
  let file = open_in fname in
  let len = in_channel_length file in
  let buf = String.create len in
  really_input file buf 0 len;
  close_in file;
  buf

let c_compiler_command =
  "gcc -std=c99 -x c -O2 -I ../backend ../backend/runtime.c -o a.out -"

let compile fname =
  let gen program =
    let c_program = Emit_c.emit program in
    let c_compiler_channel = Unix.open_process_out c_compiler_command in
    output_string c_compiler_channel c_program;
    close_out c_compiler_channel
  in
  Parser.parse (read_file fname) gen

let print_usage () =
  Printf.printf "Usage: %s <source file>\n" Sys.argv.(0)

let () =
  if Array.length Sys.argv <> 2 then
    print_usage ()
  else
    compile Sys.argv.(1)
