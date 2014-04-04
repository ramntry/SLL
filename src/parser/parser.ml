open Printf
open Ostap
open Util
open Matcher
open Sll

let ident = ostap (n:IDENT {Token.repr n})
let cnt = ostap (c:CNT {Token.repr c})

let list_of_opt = function 
  | Some xs -> xs
  | None    -> []

let defs_splitter xs = 
  let rec helper defs fdefs gdefs = 
    match defs with
    | (`FRule (fn, b) :: smth)     -> helper smth ((fn, b) :: fdefs) gdefs
    | (`GRule (gn, pn, b) :: smth) -> helper smth fdefs ((gn, pn, b) :: gdefs)
    | []                           -> (fdefs, gdefs)
  in helper xs [] []

ostap (
    program_parser[e_parser]: defs:(funDef[e_parser])* -"." term:expression[e_parser] {
      let (fdefs, gdefs) = defs_splitter defs in
      make_program fdefs gdefs term
    }
    ;
    funDef[e_parser]:
      fRule[e_parser] | gRule[e_parser]
    ;
    fRule[e_parser]:
      name:ident -"(" fargs:list0[ident] -")" -"=" body:e_parser
      { `FRule (name >$ fargs >= body) }
    ;
    gRule[e_parser]:
      name:ident -"(" pname:cnt pargs:ident_ctr_args gargs:(-"," ident)* -")"
          -"=" gbody:e_parser 
      { `GRule (name $ (pname +> pargs, gargs) => gbody) }
    ;
    args_list[e_parser]: -"(" list0[e_parser] -")" 
    ;
    expression[e_parser]:  
        constructor[e_parser] 
      | funCall[e_parser] 
      | v:ident  { `Var v }
    ;
    ident_ctr_args:
      pargs:(-"(" list0[ident] -")")? { list_of_opt pargs }
    ;
    constructor[e_parser]:
      cname:cnt  args:args_list[e_parser]? 
      { `Ctr cname (list_of_opt args) }
    ;
    funCall[e_parser]:
      name:ident args:args_list[e_parser] { `FCall (name, args) }
)

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

let rec pure_parser xs = expression pure_parser xs
let program_parser = program_parser pure_parser

let example =
    "add(Z, x) = x\n"
  ^ "add(S(x), y) = S(add(x, y))\n"
  ^ ".\n"
  ^ "add(S(Z), S(S(Z)))"

let big_example = string_of_program string_of_pure Arithm.program

let resolve_gcalls { fdefs = fdefs; gdefs = gdefs; term = term; } =
  let rec resolve_expr = function
    | `FCall (name, parg :: args) when not (Ident_map.mem name fdefs) ->
        `GCall (name, resolve_expr parg, List.map resolve_expr args)
    | `FCall (fname, fargs) -> `FCall (fname, List.map resolve_expr fargs)
    | `GCall (gname, pname, gargs) -> `GCall (gname, pname, gargs)
    | `Ctr (cname, cargs) -> `Ctr (cname, List.map resolve_expr cargs)
    | `Var name -> `Var name
  in
  let resolved_fdefs = Ident_map.map (fun { fargs = fargs; fbody = fbody; } ->
    { fargs = fargs; fbody = resolve_expr fbody } ) fdefs
  in
  let resolved_gdefs = Ident_map.map (fun gpdefs ->
    Ident_map.map (fun { pargs = pargs; gargs = gargs; gbody = gbody; } ->
      { pargs = pargs; gargs = gargs; gbody = resolve_expr gbody; })
      gpdefs) gdefs
  in
  { fdefs = resolved_fdefs; gdefs = resolved_gdefs; term = resolve_expr term; }
          
let verbose_test () =
  Combinators.unwrap (program_parser (new lexer big_example))
    (fun program -> printf "Parsed program:\n%s\n" (string_of_program string_of_pure program))
    (fun reason -> printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Acc reason))

let () =
  Combinators.unwrap (program_parser (new lexer big_example))
    (fun program ->
       let result = Interpret.run (resolve_gcalls program) in
       printf "%s\n" (string_of_pure result))
    (fun _ -> ())
