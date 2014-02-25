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
  body : expr;
}
type fdef = { fname : ident; fargs : ident list; body : expr; }

type ftable = (ident, fdef) Hashtbl.t
type gtable = (ident * ident, gdef) Hashtbl.t

type program = { ftable : ftable; gtable : gtable; term : expr; }

exception Interpret_error of string

val string_of_expr : expr -> string
val string_of_program : program -> string
val run : program -> expr
