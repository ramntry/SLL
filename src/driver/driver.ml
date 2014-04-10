open Emit_c

let read_file fname =
  let file = open_in fname in
  let len = in_channel_length file in
  let buf = String.create len in
  really_input file buf 0 len;
  close_in file;
  buf

let c_compiler_command evalstrat =
  let oops_src = Sys.getenv "OOPS_SRC" in
  let backend_dir = Filename.concat oops_src "backend" in
  let runtime_src = Filename.concat backend_dir "runtime.c" in
  let evalstrat_sym =
    match evalstrat with
    | Strict -> "SLL_STRICT"
    | Byname -> "SLL_BYNAME"
    | Byneed -> "SLL_BYNEED"
  in
  "gcc -x c -std=c99 -pedantic -Wall -O2"
    ^ " -D" ^ evalstrat_sym
    ^ " -I" ^ backend_dir
    ^ " " ^ runtime_src
    ^ " -o a.out -"

let compile fname call_by_name fast_head_form =
  let evalstrat  =
    match (call_by_name, fast_head_form) with
    | (false, _)    -> Strict
    | (true, false) -> Byname
    | (true, true)  -> Byneed
  in
  let gen program =
    let c_program = emit ~evalstrat program in
    let c_compiler_channel =
      Unix.open_process_out (c_compiler_command evalstrat)
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
