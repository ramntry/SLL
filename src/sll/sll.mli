type ident = String.t

module Ident_map : module type of Map.Make(String) with type key := ident

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

val string_of_expr : ('e -> string) -> 'e expr -> string
val string_of_pure : pure -> string
val string_of_fdef : ('e -> string) -> ident * 'e fdef -> string
val string_of_gpdef : ('e -> string) -> ident * ident * 'e gpdef -> string
val string_of_program : ('e -> string) -> 'e program -> string

val make_program : (ident * 'e fdef) list -> (ident * ident * 'e gpdef) list
  -> 'e -> 'e program

val ( +> ) : ident -> ident list -> ident * ident list
val ( $  ) : ident -> (ident * ident list) * ident list ->
  ident * ident * ident list * ident list
val ( => ) :
  ident * ident * ident list * ident list -> 'e -> ident * ident * 'e gpdef
val ( >$ ) : ident -> ident list -> ident * ident list
val ( >= ) : ident * ident list -> 'e -> ident * 'e fdef
