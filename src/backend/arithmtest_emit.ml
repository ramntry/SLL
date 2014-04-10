let () =
  let c_program = Emit_c.emit ~evalstrat:Emit_c.Byneed Arithm.program in
  print_endline c_program
