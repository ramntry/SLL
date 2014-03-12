open Sll
open Interpret

let rec make_nat = function
  | 0 -> Ctr ("Z", [])
  | n -> Ctr ("S", [make_nat (n - 1)])

let rec from_nat = function
  | Ctr ("Z", [])  -> 0
  | Ctr ("S", [x]) -> 1 + from_nat x
  | x -> raise (Interpret_error ("(from_nat) bad nat: " ^ string_of_expr x))

let make_int z =
  if z < 0
  then Ctr ("N", [make_nat (-z)])
  else (make_nat z)

let from_int = function
  | Ctr ("N", [x]) -> -(from_nat x)
  | x -> from_nat x

let () =
  print_endline (string_of_program Arithm.program);
  let tester make_call func control x y =
    let term = make_call func x y in
    let program = { Arithm.program with term } in
    if run program = make_int (control x y)
    then print_string      " ok"
    else print_endline "\n [FAIL]"
  in
  let test = tester (fun func x y -> GCall (func, make_int x, [make_int y])) in
  test  "snn" ( - )   8    5 ;
  test  "snn" ( - )   5    8 ;
  test  "snn" ( - )   8    0 ;
  test  "snn" ( - )   0    8 ;
  test  "snn" ( - )   0    0 ;
  test  "add" ( + )   0    0 ;
  test  "add" ( + )   0    8 ;
  test  "add" ( + )   0  (-8);
  test  "add" ( + )   5    0 ;
  test  "add" ( + )   5    8 ;
  test  "add" ( + )   5  (-8);
  test  "add" ( + )   8  (-5);
  test  "add" ( + ) (-7)   0 ;
  test  "add" ( + ) (-7)   5 ;
  test  "add" ( + ) (-7)   7 ;
  test  "add" ( + ) (-7)   9 ;
  test  "add" ( + ) (-7) (-5);
  test  "mul" ( * )   0    0 ;
  test  "mul" ( * )   5    0 ;
  test  "mul" ( * )   0    5 ;
  test  "mul" ( * )   5    8 ;
  test  "mul" ( * )   5  (-8);
  test  "mul" ( * ) (-8)   5 ;
  test  "mul" ( * ) (-8) (-5);
  let testf = tester (fun func x y -> FCall (func, [make_int x; make_int y])) in
  testf "sub" ( - )   5  (-8);
  testf "sub" ( - ) (-8)   5 ;
  testf "sub" ( - ) (-8) (-5);
  testf "dnn" ( / )   0    5 ;
  testf "dnn" ( / )   3    5 ;
  testf "dnn" ( / )  40    8 ;
  testf "dnn" ( / )  41    8 ;
  testf "dnn" ( / )  39    8 ;
  testf "div" ( / )   0    5 ;
  testf "div" ( / )   3    5 ;
  testf "div" ( / )  40    8 ;
  testf "div" ( / )  41    8 ;
  testf "div" ( / )  39    8 ;
  testf "div" ( / )   3  (-8);
  testf "div" ( / ) (-8)   3 ;
  testf "div" ( / ) (-8) (-3);
  testf "mod" (mod)   0    5 ;
  testf "mod" (mod)   3    5 ;
  testf "mod" (mod)  40    8 ;
  testf "mod" (mod)  41    8 ;
  testf "mod" (mod)  39    8 ;
  testf "mod" (mod)   3  (-8);
  testf "mod" (mod) (-8)   3 ;
  testf "mod" (mod) (-8) (-3);
  print_endline "";

  print_string "x (some integer value) = ";
  let x = make_int (read_int ()) in
  print_string "y (some integer again) = ";
  let y = make_int (read_int ()) in
  let ratio  = run { Arithm.program with term = FCall ("div", [x; y]) } in
  let modulo = run { Arithm.program with term = FCall ("mod", [x; y]) } in
  print_endline ("x / y = " ^ string_of_int (from_int ratio));
  print_endline ("x % y = " ^ string_of_int (from_int modulo));
