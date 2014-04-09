let () =
  let c_program = Emit_c.emit Arithm.program in
  print_endline c_program
