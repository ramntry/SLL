type ident = string

type expr =
  | Var of ident
  | Ctr of ident * expr list
  | FCall of ident * expr list
  | GCall of ident * expr * expr list

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

val string_of_expr : expr -> string
val string_of_fdef : ident * fdef -> string
val string_of_gpdef : ident * ident * gpdef -> string
val string_of_program : program -> string
val make_program :
  (ident * fdef) list -> (ident * ident * gpdef) list -> expr -> program

val ( +> ) : ident -> ident list -> ident * ident list
val ( $  ) : ident -> (ident * ident list) * ident list ->
  ident * ident * ident list * ident list
val ( => ) :
  ident * ident * ident list * ident list -> expr -> ident * ident * gpdef
val ( >$ ) : ident -> ident list -> ident * ident list
val ( >= ) : ident * ident list -> expr -> ident * fdef
