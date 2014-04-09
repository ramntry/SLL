let read_file fname =
  let file = open_in fname in
  let len = in_channel_length file in
  let buf = String.create len in
  really_input file buf 0 len;
  close_in file;
  buf

let c_compiler_command head_form_def =
  "gcc -std=c99 -x c -O2 "
  ^ head_form_def ^
  " -I ../backend ../backend/runtime.c -o a.out -"

let compile fname call_by_name fast_head_form =
  let head_form_def = if call_by_name then
    "-DHEAD_FORM=" ^ if fast_head_form then "fast_head_form" else "head_form"
    else ""
  in
  let gen program =
    let c_program = Emit_c.emit ~strict:(not call_by_name) program in
    let c_compiler_channel =
      Unix.open_process_out (c_compiler_command head_form_def)
    in
    output_string c_compiler_channel c_program;
    close_out c_compiler_channel
  in
  Parser.parse (read_file fname) gen

let print_usage () =
  Printf.printf
    "Usage: %s <source file> [evaluation strategy]\n\
     evaluation strategy is:\n  %s\n  %s\n  %s\n"
     Sys.argv.(0)
     "    -- (default) call-by-value OR"
     "-L  --           call-by-name  OR"
     "-LL --           call-by-need"

let () =
  let argslen = Array.length Sys.argv in
  if argslen < 2 then
    print_usage ()
  else
    compile Sys.argv.(1)
      (argslen = 3 && (Sys.argv.(2) = "-LL" || Sys.argv.(2) = "-L"))
      (argslen = 3 &&  Sys.argv.(2) = "-LL")
