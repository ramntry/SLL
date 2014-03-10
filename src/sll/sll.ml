type ident = string

type expr =
  | Var of ident
  | Ctr of ident * expr list
  | FCall of ident * expr list
  | GCall of ident * expr * expr list

module Ident_map = Map.Make(String)

type gpdef = {
  pargs : ident list;
  gargs : ident list;
  gbody : expr;
}

type gdef = gpdef Ident_map.t

type fdef = {
  fargs : ident list;
  fbody : expr;
}

type fdefs = fdef Ident_map.t
type gdefs = gdef Ident_map.t

type program = {
  fdefs : fdefs;
  gdefs : gdefs;
  term : expr;
}

let string_of_app name args = name ^ "(" ^ String.concat ", " args ^ ")"

let rec string_of_expr = function
  | Var vname -> vname
  | Ctr (name, args) | FCall (name, args) ->
      string_of_app name (List.map string_of_expr args)
  | GCall (name, parg, args) ->
      string_of_app name (List.map string_of_expr (parg :: args))

let make_program fdefs_ls gpdefs_ls term =
  let open Ident_map in
  let fdefs = List.fold_left (fun acc (fname, fdef) ->
    add fname fdef acc) empty fdefs_ls
  in
  let gdefs = List.fold_left (fun acc (gname, pname, gpdef) ->
    let gdef = try find gname acc with Not_found -> empty in
    add gname (add pname gpdef gdef) acc) empty gpdefs_ls
  in
  { fdefs; gdefs; term; }

let string_of_fdef (fname, { fargs; fbody; }) =
  string_of_app fname fargs ^ " = " ^ string_of_expr fbody

let string_of_gpdef (gname, pname, { pargs; gargs; gbody; }) =
  string_of_app gname (string_of_app pname pargs :: gargs) ^
    " = " ^ string_of_expr gbody

let string_of_program { fdefs; gdefs; term; } =
  let buffer = Buffer.create 16 in
  Ident_map.iter (fun fname fdef ->
    Buffer.add_string buffer (string_of_fdef (fname, fdef) ^ "\n")) fdefs;
  Ident_map.iter (fun gname gdef ->
    Ident_map.iter (fun pname gpdef ->
      Buffer.add_string buffer (string_of_gpdef (gname, pname, gpdef) ^ "\n"))
    gdef) gdefs;
  Buffer.add_string buffer (".\n" ^ string_of_expr term);
  Buffer.contents buffer

let ( +> ) pname pargs = (pname, pargs)
let ( $  ) gname ((pname, pargs), gargs) = (gname, pname, pargs, gargs)
let ( => ) (gname, pname, pargs, gargs) gbody =
  (gname, pname, { pargs; gargs; gbody; })

let ( >$ ) fname fargs = (fname, fargs)
let ( >= ) (fname, fargs) fbody = (fname, { fargs; fbody; })

