open Sll

type env = (ident * pure) list

exception Interpret_error of string

module Ident_map = Map.Make(String)

let rec ( // ) expr env =
  let ( //* ) exprs env = List.map (fun e -> e // env) exprs in
  match expr with
  | `Var vname -> begin try
        List.assoc vname env
      with Not_found -> raise (Interpret_error ("Unbound variable " ^ vname))
    end
  | `Ctr (name, args) -> `Ctr (name, args //* env)
  | `FCall (name, args) -> `FCall (name, args //* env)
  | `GCall (name, parg, args) -> `GCall (name, parg // env, args //* env)

let run { fdefs; gdefs; term; } =
  let rec intr = function
    | `Var name -> raise (Interpret_error ("Undefined variable " ^ name))
    | `Ctr (cname, cargs) -> `Ctr (cname, List.map intr cargs)
    | `FCall (fname, act_args) ->
        let { fargs; fbody; } = begin try
            Ident_map.find fname fdefs
          with Not_found ->
            raise (Interpret_error ("f-definition not found: " ^ fname))
          end
        in
        intr (fbody // List.combine fargs act_args)
    | `GCall (gname, parg, act_args) ->
        begin match intr parg with
        | `Ctr (cname, cargs) ->
            let { pargs; gargs; gbody; } = begin try
                let gpdefs = Ident_map.find gname gdefs in
                Ident_map.find cname gpdefs
              with Not_found ->
                raise (Interpret_error ("g-definition not found: " ^
                                        gname ^ "(" ^ cname ^ "(...)...)"))
              end
            in
            let env = List.combine pargs cargs @ List.combine gargs act_args in
            intr (gbody // env)
        | _ -> raise (Interpret_error ("FATAL: this code must be unreachable!"))
        end
  in
  intr term
