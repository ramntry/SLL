let () =
  let c_program = Emit_c.emit ~strict:true Arithm.program in
  print_endline c_program
