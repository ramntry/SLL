type ident = string
type pattern = { pname : ident; pargs : ident list; }
type expr =
  | Var of ident
  | Ctr of ident * expr list
  | FCall of ident * expr list
  | GCall of ident * expr list

type gdef = {
  gname : ident;
  pattern : pattern;
  gargs : ident list;
  gbody : expr;
}
type fdef = { fname : ident; fargs : ident list; fbody : expr; }

type ftable = (ident, fdef) Hashtbl.t
type gtable = (ident * ident, gdef) Hashtbl.t

type program = { ftable : ftable; gtable : gtable; term : expr; }
type env = (ident * expr) list

exception Interpret_error of string

let rec ( // ) expr env =
  let ( //* ) exprs env = List.map (fun e -> e // env) exprs in
  match expr with
  | Var vname -> List.assoc vname env
  | Ctr (name, args) -> Ctr (name, args //* env)
  | FCall (name, args) -> FCall (name, args //* env)
  | GCall (name, args) -> GCall (name, args //* env)

let string_of_app name args = name ^ "(" ^ String.concat ", " args ^ ")"

let rec string_of_expr = function
  | Var vname -> vname
  | Ctr (name, args) | FCall (name, args) | GCall (name, args) ->
      string_of_app name (List.map string_of_expr args)

let make_program fdefs gdefs term =
  let (ftable : ftable) = Hashtbl.create (Array.length fdefs) in
  let (gtable : gtable) = Hashtbl.create (Array.length gdefs) in
  Array.iter (fun ({ fname; _ } as fdef) ->
    Hashtbl.replace ftable fname fdef) fdefs;
  Array.iter (fun ({ gname; pattern = { pname; _ }; _ } as gdef) ->
    Hashtbl.replace gtable (gname, pname) gdef) gdefs;
  { ftable; gtable; term; }

let string_of_fdef { fname; fargs; fbody; } =
  string_of_app fname fargs ^ " = " ^ string_of_expr fbody

let string_of_pattern { pname; pargs; } = string_of_app pname pargs

let string_of_gdef { gname; pattern; gargs; gbody; } =
  string_of_app gname (string_of_pattern pattern :: gargs) ^
    " = " ^ string_of_expr gbody

let string_of_program { ftable; gtable; term; } =
  let buffer = Buffer.create 16 in
  let add_defs printer table =
    Hashtbl.iter (fun _ def ->
      Buffer.add_string buffer (printer def ^ "\n")) table
  in
  add_defs string_of_fdef ftable;
  add_defs string_of_gdef gtable;
  Buffer.add_string buffer (string_of_expr term);
  Buffer.contents buffer

let rec make_nat = function
  | 0 -> Ctr ("Z", [])
  | n -> Ctr ("S", [make_nat (n - 1)])

let run { ftable; gtable; term; } =
  let rec intr = function
    | Var name -> raise (Interpret_error ("Undefined variable " ^ name))
    | Ctr (cname, cargs) -> Ctr (cname, List.map intr cargs)
    | FCall (fname, act_args) ->
        let { fname; fargs; fbody; } = Hashtbl.find ftable fname in
        intr (fbody // List.combine fargs act_args)
    | GCall (gname, pat :: act_args) ->
        begin match intr pat with
        | Ctr (cname, cargs) ->
            let { gname; pattern = { pname; pargs }; gargs; gbody; } =
              Hashtbl.find gtable (gname, cname)
            in
            let env = List.combine pargs cargs @ List.combine gargs act_args in
            intr (gbody // env)
        | _ -> raise (Interpret_error ("FATAL: this code must be unreachable!"))
        end
    | _ -> raise (Interpret_error ("GCall with zero-length argument list"))
  in
  intr term

let () =
  let gname = "gAdd" in
  let gAddZ = {
    gname;
    pattern = { pname = "Z"; pargs = []; };
    gargs = ["y"];
    gbody = Var "y";
  } in
  let gAddS = {
    gname;
    pattern = { pname = "S"; pargs = ["x"]; };
    gargs = ["y"];
    gbody = Ctr ("S", [
              GCall (gname, [
                Var "x"; Var "y"])]);
  } in
  let two = make_nat 2 in
  let three = make_nat 3 in
  let term = GCall (gname, [three; two]) in
  let program = make_program [||] [|gAddZ; gAddS|] term in
  let program_text = string_of_program program in
  let result_text = string_of_expr (run program) in
  print_endline (program_text ^ " = " ^ result_text)
