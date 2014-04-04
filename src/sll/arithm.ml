open Sll

let rec make_nat = function
  | 0 -> `Ctr ("Z", [])
  | n -> `Ctr ("S", [make_nat (n - 1)])

let rec from_nat = function
  | `Ctr ("Z", [])  -> 0
  | `Ctr ("S", [x]) -> 1 + from_nat x
  | x -> failwith ("(from_nat) bad nat: " ^ string_of_pure x)

let make_int z =
  if z < 0
  then `Ctr ("N", [make_nat (-z)])
  else (make_nat z)

let from_int = function
  | `Ctr ("N", [x]) -> -(from_nat x)
  | x -> from_nat x

let rec make_list = function
  | []       -> `Ctr ("Nil", [])
  | hd :: tl -> `Ctr ("Cons", [make_int hd; make_list tl])

let program =
  let gdefs = [
    (* Subtraction for Non-Negative integers *)
    "snn"  $ ("Z" +> [],    ["y"]) => `GCall ("neg", `Var "y", []);
    "snn"  $ ("S" +> ["x"], ["y"]) => `GCall ("sud", `Var "y", [`Var "x"]);

    (* Subtract from already Decremented second argument Unexplored first one *)
    "sud"  $ ("Z" +> [],    ["x"]) => `Ctr ("S", [`Var "x"]);
    "sud"  $ ("S" +> ["y"], ["x"]) => `GCall ("snn", `Var "x", [`Var "y"]);

    (* Negate integer *)
    "neg"  $ ("Z" +> [],    []) => `Ctr ("Z", []);
    "neg"  $ ("S" +> ["x"], []) => `Ctr ("N", [`Ctr ("S", [`Var "x"])]);
    "neg"  $ ("N" +> ["x"], []) => `Var "x";

    (* Addition *)
    "add"  $ ("Z" +> [],    ["y"]) => `Var "y";
    "add"  $ ("S" +> ["x"], ["y"]) =>
      `GCall ("aup", `Var "y", [`Ctr ("S", [`Var "x"])]);
    "add"  $ ("N" +> ["x"], ["y"]) => `GCall ("sup", `Var "y", [`Var "x"]);

    (* Add Unexplored integer and Positive one *)
    "aup"  $ ("Z" +> [],    ["y"]) => `Var "y";
    "aup"  $ ("S" +> ["x"], ["y"]) =>
      `Ctr ("S", [`GCall ("aup", `Var "x", [`Var "y"])]);
    "aup"  $ ("N" +> ["x"], ["y"]) => `GCall ("snn", `Var "y", [`Var "x"]);

    (* Subtract from Unexplored integer Positive one *)
    "sup"  $ ("Z" +> [],    ["y"]) => `Ctr ("N", [`Var "y"]);
    "sup"  $ ("S" +> ["x"], ["y"]) => `GCall ("sud", `Var "y", [`Var "x"]);
    "sup"  $ ("N" +> ["x"], ["y"]) =>
      `Ctr ("N", [`GCall ("aup", `Var "y", [`Var "x"])]);

    (* Multiplication *)
    "mul"  $ ("Z" +> [],    ["y"]) => `Ctr ("Z", []);
    "mul"  $ ("S" +> ["x"], ["y"]) =>
      `GCall ("add", `Var "y", [`GCall ("mul", `Var "x", [`Var "y"])]);
    "mul"  $ ("N" +> ["x"], ["y"]) =>
      `GCall ("neg", `GCall ("mul", `Var "x", [`Var "y"]), []);

    (* Is Negative predicate *)
    "isn"  $ ("Z" +> [],    []) => `Ctr ("F", []);
    "isn"  $ ("S" +> ["x"], []) => `Ctr ("F", []);
    "isn"  $ ("N" +> ["x"], []) => `Ctr ("T", []);

    (* Absolute value *)
    "abs"  $ ("Z" +> [],    []) => `Ctr ("Z", []);
    "abs"  $ ("S" +> ["x"], []) => `Ctr ("S", [`Var "x"]);
    "abs"  $ ("N" +> ["x"], []) => `Var "x";

    (* Signum function *)
    "sgn"  $ ("Z" +> [],    []) => `Ctr ("Z", []);
    "sgn"  $ ("S" +> ["x"], []) => `Ctr ("S", [`Ctr ("Z", [])]);
    "sgn"  $ ("N" +> ["x"], []) => `Ctr ("N", [`Ctr ("S", [`Ctr ("Z", [])])]);

    (* Condition / if-then-else *)
    "cnd"  $ ("T" +> [], ["t0"; "f0"]) => Var "t0";
    "cnd"  $ ("F" +> [], ["t1"; "f1"]) => Var "f1";

    (* List Operations: Merge *)
    "merge" $ ("Nil"  +> [],           ["b"]) => `Var "b";
    "merge" $ ("Cons" +> ["hd"; "tl"], ["b"]) =>
      `GCall ("merge2", `Var "b", [`Var "hd"; `Var "tl"]);
    "merge2" $ ("Nil" +> [], ["hda"; "tla"]) =>
      `Ctr ("Cons", [`Var "hda"; `Var "tla"]);
    "merge2" $ ("Cons" +> ["hdb"; "tlb"], ["hda"; "tla"]) =>
      `GCall ("cnd",
        `GCall ("lss", `Var "hda", [`Var "hdb"]), [
        `Ctr ("Cons", [`Var "hda";
          `GCall ("merge2", `Var "tla", [`Var "hdb"; `Var "tlb"])]);
        `Ctr ("Cons", [`Var "hdb";
          `GCall ("merge2", `Var "tlb", [`Var "hda"; `Var "tla"])])]);

    (* ListUp: [a, b, c] ~~> [[a], [b], [c]] *)
    "listup" $ ("Nil" +> [], []) => `Ctr ("Nil", []);
    "listup" $ ("Cons" +> ["hd"; "tl"], []) =>
      `Ctr ("Cons", [
        `Ctr ("Cons", [`Var "hd"; `Ctr ("Nil", [])]);
        `GCall ("listup", `Var "tl", [])]);

    (* MaybeHead: [a, b] ~~> a && [] ~~> [] *)
    "maybehead" $ ("Nil" +> [], []) => `Ctr ("Nil", []);
    "maybehead" $ ("Cons" +> ["hd"; "tl"], []) => `Var "hd";

    (* SortIters: [[c], [b], [d], [a]] ~~> [[a, b, c, d]] *)
    "sortiters" $ ("Nil" +> [], []) => `Ctr ("Nil", []);
    "sortiters" $ ("Cons" +> ["hd"; "tl"], []) =>
      `GCall ("sortiters2", `Var "tl", [`Var "hd"]);
    "sortiters2" $ ("Nil" +> [], ["hd"]) =>
      `Ctr ("Cons", [`Var "hd"; `Ctr ("Nil", [])]);
    "sortiters2" $ ("Cons" +> ["hd2"; "tl"], ["hd"]) =>
      `GCall ("sortiters",
        `Ctr ("Cons", [
          `GCall ("merge", `Var "hd", [`Var "hd2"]);
          `GCall ("sortiters", `Var "tl", [])]), []);
  ] in
  let fdefs = [
    (* Subtraction *)
    "sub" >$ ["x"; "y"] >= `GCall ("add", `Var "x", [`GCall ("neg", `Var "y", [])]);

    (* Less then predicate *)
    "lss" >$ ["x"; "y"] >= `GCall ("isn", `FCall ("sub", [`Var "x"; `Var "y"]), []);

    (* Division for Non-Negative integers *)
    "dnn" >$ ["x"; "y"] >= `GCall ("cnd",
      `FCall ("lss", [`Var "x"; `Var "y"]), [
      `Ctr ("Z", []);
      `Ctr ("S", [`FCall ("dnn", [
        `FCall ("sub", [`Var "x"; `Var "y"]);
        `Var "y"])])]);

    (* Division *)
    "div" >$ ["x"; "y"] >= `GCall ("mul",
      `GCall ("mul", `GCall ("sgn", `Var "x", []), [`GCall ("sgn", `Var "y", [])]), [
      `FCall ("dnn", [
        `GCall ("abs", `Var "x", []); `GCall ("abs", `Var "y", [])])]);

    (* signed Modulo *)
    "mod" >$ ["x"; "y"] >= `FCall ("sub", [ `Var "x";
      `GCall ("mul", `FCall ("div", [`Var "x"; `Var "y"]), [`Var "y"])]);

    (* List Operations: MergeSort *)
    "sort" >$ ["a"] >= `GCall ("maybehead",
      `GCall ("sortiters", `GCall ("listup", `Var "a", []), []), []);
  ] in
  let a = make_list [4; 1; -2; 5; -3; 3; 0; -1; -4; 6; -5; 2] in
  make_program fdefs gdefs (`FCall ("sort", [a]))
