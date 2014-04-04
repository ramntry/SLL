open Printf
open String
open Str
open Ostap
open Util
open Matcher

let ident = ostap (n:IDENT {Token.repr n})
let cnt = ostap (c:CNT {Token.repr c})

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


let list_of_opt = function 
                  | Some xs -> xs
                  | None    -> []


let defs_splitter xs = 
  let rec helper defs fdefs gdefs = 
    match defs with
    | (`FRule f :: smth)  -> helper smth (f :: defs) gdefs
    | (`GRule g :: smth)  -> helper smth fdefs (g :: gdefs)
    | []                  -> (fdefs, gdefs)
  in helper xs [] []


 	
ostap (
    program[e_parser]: funDef[e_parser] | expression[e_parser]
    ;
    funDef[e_parser]:
      fDef[e_parser] | gDef[e_parser]
    ;
    fDef[e_parser]: 
      fRule[e_parser]
    ;
    gDef[e_parser]:
      gRule[e_parser]
    ;
    fRule[e_parser]:
      name:ident (*args:args_list[ident]*) -"(" fargs:list0[ident] -")" -"=" body:e_parser
      { `FRule(name, { fargs = fargs; fbody = body })}
    ;
    gRule[e_parser]:
      name:ident -"(" pname:cnt pargs:ident_ctr_args gargs:(-"," list[ident])? -")" -"=" gbody:e_parser 
      { `GRule (name, pname, { pargs = pargs; gargs = (list_of_opt gargs); gbody = gbody }) } 
    ;
    args_list[e_parser]: -"(" list0[e_parser] -")" 
    ;
    expression[e_parser]:  
       constructor[e_parser] 
      |funCall[e_parser] (*either Ffunction or Gfunction call*)
      |v:ident  {`Var v}
    ;
    ident_ctr_args:
      pargs:(-"(" (list0[ident])? -")")?
    { match pargs with
      | Some (Some xs) -> xs
      | _    -> []
    }
    ;
    constructor[e_parser]:
      cname:cnt  args:args_list[e_parser]? 
      { `Ctr cname (list_of_opt args) }
    ;
    funCall[e_parser]:
      name:ident args:args_list[e_parser] { `FCall (name, args) }
)

let rec toString = function
            | `Var v            -> "Var  " ^ v
            | `Ctr (x,ps)       -> "Ctr " ^ x ^ "\n    " ^ (concat " \n    " (List.map toString ps))
            | `FCall (fn, ps)   ->  "FCall\n    FName " ^ fn ^ "\n    " ^ concat " \n    " (List.map toString ps)
            (*| `FRule (fn, { p;s; e;}) ->   "FRule\n    FName " ^ fn ^ "\n    " 
                 ^ concat " \n    " (List.map toString p) ^ " \n   Body" ^ toString e*)
            | _                 -> "smth /n"

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let ident = Str.regexp "[a-z][a-zA-Z0-9]*" in
  let cnt = Str.regexp "[A-Z][a-zA-Z0-9]*" in
  object (self)

    inherit t s

    method skip p c = skip s p c
    method getIDENT = self#get "identifier" ident
    method getCNT   = self#get "constructor" cnt

  end



let rec pure_sll_parser xs = expression pure_sll_parser xs

let _ =
  Combinators.unwrap (pure_sll_parser (new lexer "rimpleFunc(xa)"))
    (fun tree -> printf "Parsed expression:\n%s\n" (toString tree))
    (fun reason -> printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Acc reason))
