type ident = String.t

module Ident_map = Map.Make(String)

type 'e expr = [
  | `Var   of ident
  | `Ctr   of ident * 'e list
  | `FCall of ident * 'e list
  | `GCall of ident * 'e * 'e list
]

type 'e gpdef = {
  pargs : ident list;
  gargs : ident list;
  gbody : 'e;
}

type 'e gdef = 'e gpdef Ident_map.t

type 'e fdef = {
  fargs : ident list;
  fbody : 'e;
}

type 'e fdefs = 'e fdef Ident_map.t
type 'e gdefs = 'e gdef Ident_map.t

type 'e program = {
  fdefs : 'e fdefs;
  gdefs : 'e gdefs;
  term  : 'e;
}

type pure = pure expr

let string_of_app name args = name ^ "(" ^ String.concat ", " args ^ ")"
let string_of_app0 name = function
  | [] -> name
  | args -> string_of_app name args

let string_of_expr string_of_e = function
  | `Var vname -> vname
  | `Ctr (name, args) ->
      string_of_app0 name (List.map string_of_e args)
  | `FCall (name, args) ->
      string_of_app name (List.map string_of_e args)
  | `GCall (name, parg, args) ->
      string_of_app name (List.map string_of_e (parg :: args))

let rec string_of_pure pure = string_of_expr string_of_pure pure

let string_of_fdef string_of_e (fname, { fargs; fbody; }) =
  string_of_app fname fargs ^ " = " ^ string_of_e fbody

let string_of_gpdef string_of_e (gname, pname, { pargs; gargs; gbody; }) =
  string_of_app gname (string_of_app0 pname pargs :: gargs) ^
    " = " ^ string_of_e gbody

let string_of_program string_of_e { fdefs; gdefs; term; } =
  let buffer = Buffer.create 16 in
  Ident_map.iter (fun fname fdef ->
    Buffer.add_string buffer
      (string_of_fdef string_of_e (fname, fdef) ^ "\n")) fdefs;
  Ident_map.iter (fun gname gdef ->
    Ident_map.iter (fun pname gpdef ->
      Buffer.add_string buffer
        (string_of_gpdef string_of_e (gname, pname, gpdef) ^ "\n"))
    gdef) gdefs;
  Buffer.add_string buffer (".\n" ^ string_of_e term);
  Buffer.contents buffer

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

let ( +> ) pname pargs = (pname, pargs)
let ( $  ) gname ((pname, pargs), gargs) = (gname, pname, pargs, gargs)
let ( => ) (gname, pname, pargs, gargs) gbody =
  (gname, pname, { pargs; gargs; gbody; })

let ( >$ ) fname fargs = (fname, fargs)
let ( >= ) (fname, fargs) fbody = (fname, { fargs; fbody; })
