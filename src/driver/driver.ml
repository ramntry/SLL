open Emit_c

let c_compiler = ref "gcc"
let c_compiler_args = ref "-x c -std=c99 -Wall"
let trailing_c_compiler_args = ref ""

let evalstrat = ref Byneed
let keep_c = ref false
let only_c = ref false
let no_opt = ref false
let src_filename = ref ""
let bin_filename = ref ""
let object_only = ref false
let with_dbg_info = ref false

let command_line_args = [
  ("-strict", Arg.Unit (fun () -> evalstrat := Strict),
     " Use strict evaluation strategy (call-by-value semantics)",
     `DefaultAlt (!evalstrat = Strict));

  ("-byname", Arg.Unit (fun () -> evalstrat := Byname),
     " Use call-by-name semantics (usually produce very slow binary)",
     `DefaultAlt (!evalstrat = Byname));

  ("-byneed", Arg.Unit (fun () -> evalstrat := Byneed),
     " Use call-by-need semantics (lazy evaluation strategy)",
     `DefaultAlt (!evalstrat = Byneed));

  ("-o", Arg.Set_string bin_filename,
     "<file> Output binary file name",
     `DefaultVal "<derived from source file name>");

  ("-keep-c", Arg.Set keep_c,
     " Keep generated C source file",
     `DefaultOpt !keep_c);

  ("-only-c", Arg.Set only_c,
     " Only generate C source file",
     `DefaultOpt !only_c);

  ("-c", Arg.Set object_only,
     " Only compile generated C source file, don't link",
     `DefaultOpt !object_only);

  ("-no-opt", Arg.Set no_opt,
     " Compile without optimization (faster compilation, slower binary)",
     `DefaultOpt !no_opt);

  ("-g", Arg.Set with_dbg_info,
     " Compile with debug information",
     `DefaultOpt !with_dbg_info);

  ("-cc", Arg.Set_string c_compiler,
     "<C-compiler> Use specified " ^ !c_compiler ^ " compatible C compiler",
     `DefaultVal !c_compiler);

  ("-cc-args", Arg.Set_string trailing_c_compiler_args,
     "<C-compiler-args> Append some C compiler arguments \
     (for many, use quotation marks)",
     `DefaultVal ("\"" ^ !trailing_c_compiler_args ^ "\""));

  ("-cc-over", Arg.Set_string c_compiler_args,
     "<C-compiler-args> Override not mentioned above C compiler arguments",
     `DefaultVal ("\"" ^ !c_compiler_args ^ "\""));
]

let annotate_defaults = List.map (fun (opt, act, doc, label) -> (opt, act, doc
  ^ match label with
    | `DefaultAlt b -> if b then " [default]" else ""
    | `DefaultOpt b -> " [default: " ^ (if b then "yes" else "no") ^ "]"
    | `DefaultVal s -> " [default: " ^ s ^ "]"
  ))

let arg_speclist = Arg.align (annotate_defaults command_line_args)
let arg_usage = "Usage: " ^ Sys.argv.(0) ^ " [options] [source file]\noptions are:"

let parse_args () =
  Arg.parse arg_speclist
    (fun filename -> src_filename := filename)
    arg_usage;
  if !src_filename = "" then begin
    output_string stderr (Sys.argv.(0) ^ ": no source file specified.\n");
    Arg.usage arg_speclist arg_usage;
    exit 1
  end

let read_file fname =
  let file = open_in fname in
  let len = in_channel_length file in
  let buf = String.create len in
  really_input file buf 0 len;
  close_in file;
  buf

let src_basename () =
  let maybe_extended = Filename.basename !src_filename in
  try Filename.chop_extension maybe_extended
  with Invalid_argument _ -> maybe_extended

let c_compiler_command c_src =
  let root_dir =
    Filename.dirname (Filename.dirname (Filename.dirname Sys.executable_name))
  in
  let src_dir = Filename.concat root_dir "src" in
  let backend_dir = Filename.concat src_dir "backend" in
  let runtime_src = Filename.concat backend_dir "runtime.c" in
  let evalstrat_sym =
    match !evalstrat with
    | Strict -> "SLL_STRICT"
    | Byname -> "SLL_BYNAME"
    | Byneed -> "SLL_BYNEED"
  in
  let bin_path =
    match !bin_filename with
    | ""   -> src_basename ()
    | name -> name
  in
  !c_compiler
    ^ " " ^ !c_compiler_args
    ^ (if !no_opt then "" else " -O2")
    ^ (if !with_dbg_info then " -g" else "")
    ^ " -D" ^ evalstrat_sym
    ^ " -I" ^ backend_dir
    ^ " " ^ runtime_src
    ^ " " ^ c_src
    ^ (if !object_only then " -c" else " -o " ^ bin_path)
    ^ " " ^ !trailing_c_compiler_args

let compile () =
  let (c_filename, c_channel) =
    if !keep_c
    then let fname = src_basename () ^ ".c" in (fname, open_out fname)
    else Filename.open_temp_file "sll_" ("_" ^ src_basename () ^ ".c")
  in
  let gen program =
    let c_program = emit ~evalstrat:!evalstrat program in
    output_string c_channel c_program;
    close_out c_channel;
    if not !only_c then
      if Sys.command (c_compiler_command c_filename) <> 0 then
        failwith "C compiler returns non-zero code";
  in
  Parser.parse (read_file !src_filename) gen

let () =
  parse_args ();
  compile ()
