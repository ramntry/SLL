open Sll

let program =
  let gdefs = [
    (* Subtraction for Non-Negative integers *)
    "snn"  $ ("Z" +> [],    ["y"]) => GCall ("neg", Var "y", []);
    "snn"  $ ("S" +> ["x"], ["y"]) => GCall ("sud", Var "y", [Var "x"]);

    (* Subtract from already Decremented second argument Unexplored first one *)
    "sud"  $ ("Z" +> [],    ["x"]) => Ctr ("S", [Var "x"]);
    "sud"  $ ("S" +> ["y"], ["x"]) => GCall ("snn", Var "x", [Var "y"]);

    (* Negate integer *)
    "neg"  $ ("Z" +> [],    []) => Ctr ("Z", []);
    "neg"  $ ("S" +> ["x"], []) => Ctr ("N", [Ctr ("S", [Var "x"])]);
    "neg"  $ ("N" +> ["x"], []) => Var "x";

    (* Addition *)
    "add"  $ ("Z" +> [],    ["y"]) => Var "y";
    "add"  $ ("S" +> ["x"], ["y"]) =>
      GCall ("aup", Var "y", [Ctr ("S", [Var "x"])]);
    "add"  $ ("N" +> ["x"], ["y"]) => GCall ("sup", Var "y", [Var "x"]);

    (* Add Unexplored integer and Positive one *)
    "aup"  $ ("Z" +> [],    ["y"]) => Var "y";
    "aup"  $ ("S" +> ["x"], ["y"]) =>
      Ctr ("S", [GCall ("aup", Var "x", [Var "y"])]);
    "aup"  $ ("N" +> ["x"], ["y"]) => GCall ("snn", Var "y", [Var "x"]);

    (* Subtract from Unexplored integer Positive one *)
    "sup"  $ ("Z" +> [],    ["y"]) => Ctr ("N", [Var "y"]);
    "sup"  $ ("S" +> ["x"], ["y"]) => GCall ("sud", Var "y", [Var "x"]);
    "sup"  $ ("N" +> ["x"], ["y"]) =>
      Ctr ("N", [GCall ("aup", Var "y", [Var "x"])]);

    (* Multiplication *)
    "mul"  $ ("Z" +> [],    ["y"]) => Ctr ("Z", []);
    "mul"  $ ("S" +> ["x"], ["y"]) =>
      GCall ("add", Var "y", [GCall ("mul", Var "x", [Var "y"])]);
    "mul"  $ ("N" +> ["x"], ["y"]) =>
      GCall ("neg", GCall ("mul", Var "x", [Var "y"]), []);

    (* Is Negative predicate *)
    "isn"  $ ("Z" +> [],    []) => Ctr ("F", []);
    "isn"  $ ("S" +> ["x"], []) => Ctr ("F", []);
    "isn"  $ ("N" +> ["x"], []) => Ctr ("T", []);

    (* Absolute value *)
    "abs"  $ ("Z" +> [],    []) => Ctr ("Z", []);
    "abs"  $ ("S" +> ["x"], []) => Ctr ("S", [Var "x"]);
    "abs"  $ ("N" +> ["x"], []) => Var "x";

    (* Signum function *)
    "sgn"  $ ("Z" +> [],    []) => Ctr ("Z", []);
    "sgn"  $ ("S" +> ["x"], []) => Ctr ("S", [Ctr ("Z", [])]);
    "sgn"  $ ("N" +> ["x"], []) => Ctr ("N", [Ctr ("S", [Ctr ("Z", [])])]);

    (* Condition / if-then-else *)
    "cnd"  $ ("T" +> [], ["t"; "f"]) => Var "t";
    "cnd"  $ ("F" +> [], ["t"; "f"]) => Var "f";
  ] in
  let fdefs = [
    (* Subtraction *)
    "sub" >$ ["x"; "y"] >= GCall ("add", Var "x", [GCall ("neg", Var "y", [])]);

    (* Less then predicate *)
    "lss" >$ ["x"; "y"] >= GCall ("isn", FCall ("sub", [Var "x"; Var "y"]), []);

    (* Division for Non-Negative integers *)
    "dnn" >$ ["x"; "y"] >= GCall ("cnd",
      FCall ("lss", [Var "x"; Var "y"]), [
      Ctr ("Z", []);
      Ctr ("S", [FCall ("dnn", [
        FCall ("sub", [Var "x"; Var "y"]);
        Var "y"])])]);

    (* Division *)
    "div" >$ ["x"; "y"] >= GCall ("mul",
      GCall ("mul", GCall ("sgn", Var "x", []), [GCall ("sgn", Var "y", [])]), [
      FCall ("dnn", [
        GCall ("abs", Var "x", []); GCall ("abs", Var "y", [])])]);

    (* signed Modulo *)
    "mod" >$ ["x"; "y"] >= FCall ("sub", [ Var "x";
      GCall ("mul", FCall ("div", [Var "x"; Var "y"]), [Var "y"])]);
  ] in
  make_program fdefs gdefs (Ctr ("Z", []))
