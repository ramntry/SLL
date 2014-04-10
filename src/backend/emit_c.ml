open Sll

module Ident_map = Map.Make(String)

let emit_prolog _ =
  "#include \"module_prolog.c\"\n\n"

let emit_epilog _ =
  "#include \"module_epilog.c\""

let mangle = function
  | "create_main_term_without_io" as n -> n
  | "sll_read_value" as n -> n
  | n ->  n ^ "_"

let emit_app name args =
  name ^ "(" ^ (String.concat ", " args) ^ ")"

let emit_decl name args =
  let typed_args = List.map (fun n -> "Object const " ^ n) args in
  "Object " ^ emit_app name typed_args

let emit_fdecl fname fargs =
  emit_decl fname (List.map mangle fargs)

let canonical_gargs gpdefs =
  let (_, { gargs; _ }) = Ident_map.choose gpdefs in
  List.map mangle gargs

let emit_gdecl gname gpdefs =
  emit_decl gname ("ctr" :: canonical_gargs gpdefs)

let emit_declarations { fdefs; gdefs; term; } =
  let enum = Buffer.create 16 in
  let arr = Buffer.create 16 in
  let decls = Buffer.create 16 in
  Buffer.add_string enum "enum {\n";
  Buffer.add_string arr "char const *const constructor_names[] = {\n";
  let module S = Set.Make(String) in
  let rec cnames_of_expr expr =
    let cnames_of_exprs init exprs =
      List.fold_left (fun a e ->
        S.union a (cnames_of_expr e)) init exprs
    in
    match expr with
    | `Var _ -> S.empty
    | `Ctr (cname, exprs) -> cnames_of_exprs (S.singleton cname) exprs
    | `FCall (_, exprs) -> cnames_of_exprs S.empty exprs
    | `GCall (_, ctr, exprs) -> cnames_of_exprs (cnames_of_expr ctr) exprs
  in
  let cnames_of_fdefs =
    Ident_map.fold (fun fname { fbody; fargs } a ->
      Buffer.add_string decls (emit_fdecl (mangle fname) fargs ^ ";\n");
      S.union a (cnames_of_expr fbody)) fdefs S.empty
  in
  let cnames_of_defs =
    Ident_map.fold (fun gname gpdefs a ->
      Buffer.add_string decls (emit_gdecl (mangle gname) gpdefs ^ ";\n");
      S.union a (Ident_map.fold (fun pname { gbody; _ } b ->
        S.union (S.add pname b) (cnames_of_expr gbody)) gpdefs S.empty))
      gdefs cnames_of_fdefs
  in
  let cnames = S.union cnames_of_defs (cnames_of_expr term) in
  S.iter (fun cname ->
    Buffer.add_string enum ("  " ^ mangle cname ^ ",\n");
    Buffer.add_string arr ("  \"" ^ cname ^ "\",\n")) cnames;
  let numof_cnames = S.cardinal cnames in
  Buffer.add_string enum
    ("  SllNumofCtrs = " ^ string_of_int numof_cnames ^ "\n};\n\n");
  Buffer.add_string arr "};\n\n";
  Buffer.add_buffer enum arr;
  Buffer.add_buffer enum decls;
  Buffer.add_string enum "\n";
  Buffer.contents enum

module Names = struct
  let (names : (ident, int) Hashtbl.t) = Hashtbl.create 16

  let qualify name = "m." ^ name
  let concat_name suggest = function
    | 0 -> suggest
    | counter -> suggest ^ "_" ^ (string_of_int counter)

  let make_name suggest = (
    if Hashtbl.mem names suggest
    then begin
      let counter = Hashtbl.find names suggest in
      Hashtbl.replace names suggest (counter + 1);
      concat_name suggest counter
    end else begin
      Hashtbl.add names suggest 1;
      concat_name suggest 0
    end)
    |> qualify

  let ctr name = make_name ("ctr_" ^ name)
  let fcall name = make_name ("fcall_" ^ name)
  let gcall name = make_name ("gcall_" ^ name)

  let emit buf =
    Buffer.add_string buf ("  struct {\n"
      ^ "    struct RootsBlock header;\n");
    let size = Hashtbl.fold (fun name counter acc ->
      for i = 0 to counter - 1 do
        Buffer.add_string buf ("    Object " ^ concat_name name i ^ ";\n")
      done;
      acc + counter) names 0
    in
    Buffer.add_string buf ("  } m = { { sll_roots, "
      ^ string_of_int size ^ " } };\n"
      ^ "  sll_roots = &m.header;\n")

  let reset () =
    Hashtbl.clear names
end

let emit_val_def indent vname aname args =
  String.make indent ' ' ^ vname ^ " = "
    ^ emit_app aname args ^ ";\n"

let rec emit_expr ~strict indent buf env = function
  | `Var name -> begin try List.assoc name env with Not_found -> name end
  | `Ctr (cname, exprs) ->
      let vname = Names.ctr cname in
      let numof_args = string_of_int (List.length exprs) in
      let aname = "create_object_" ^ numof_args in
      let args = List.map (fun e -> emit_expr ~strict indent buf env e) exprs in
      let val_def = emit_val_def indent vname aname (mangle cname :: args) in
      Buffer.add_string buf val_def;
      vname
  | `FCall (fname, exprs) ->
      let vname = Names.fcall fname in
      let numof_args = string_of_int (List.length exprs) in
      let args = List.map (fun e -> emit_expr ~strict indent buf env e) exprs in
      let (aname, args) = if strict
        then (mangle fname, args)
        else ("create_thunk_" ^ numof_args, ("&" ^ mangle fname) :: args)
      in
      let val_def = emit_val_def indent vname aname args in
      Buffer.add_string buf val_def;
      vname
  | `GCall (gname, ctr, exprs) ->
      let vname = Names.gcall gname in
      let numof_args = string_of_int (List.length exprs + 1) in
      let ctrval = emit_expr ~strict indent buf env ctr in
      let args = ctrval :: List.map (fun e ->
          emit_expr ~strict indent buf env e) exprs
      in
      let (aname, args) = if strict
        then (mangle gname, args)
        else ("create_thunk_" ^ numof_args, ("&" ^ mangle gname) :: args)
      in
      let val_def = emit_val_def indent vname aname args in
      Buffer.add_string buf val_def;
      vname

let emit_fdef ~strict fname { fargs; fbody; } =
  Names.reset ();
  let buf = Buffer.create 16 in
  let header = Buffer.create 16 in
  Buffer.add_string header (emit_fdecl fname fargs ^ " {\n");
  let env = List.combine fargs (List.map mangle fargs) in
  let result = emit_expr ~strict 2 buf env fbody in
  Buffer.add_string buf ("  sll_roots = m.header.next;\n"
    ^ "  return " ^ result ^ ";\n}\n\n");
  Names.emit header;
  Buffer.add_buffer header buf;
  Buffer.contents header

let emit_gdef ~strict gname gpdefs =
  Names.reset ();
  let buf = Buffer.create 16 in
  let header = Buffer.create 16 in
  let canonical_args = canonical_gargs gpdefs in
  let ctr = if strict then "ctr" else Names.ctr "" in
  let emit_case pname pargs gargs gbody =
    let penv = List.mapi (fun i arg ->
      (arg, "(Object)" ^ ctr ^ "[" ^ string_of_int (i + 1) ^ "]")) pargs
    in
    let genv = List.combine gargs canonical_args in
    Buffer.add_string buf ("case " ^ mangle pname ^ ": {\n");
    let result = emit_expr ~strict 6 buf (penv @ genv) gbody in
    Buffer.add_string buf ("      result = " ^ result ^ ";\n"
      ^ "      break;\n"
      ^ "  } ")
  in
  Buffer.add_string header (emit_gdecl gname gpdefs ^ " {\n");
  if not strict then
    Buffer.add_string buf ("  " ^ ctr ^ " = SLL_HEAD_FORM(ctr);\n");
  Buffer.add_string buf ("  Object result = NULL;\n"
    ^ "  switch (SLL_get_ctr_id(" ^ ctr ^ "[0])) {\n"
    ^ "    ");
  Ident_map.iter (fun pname { pargs; gargs; gbody; } ->
    emit_case pname pargs gargs gbody) gpdefs;
  Buffer.add_string buf ("default:\n"
    ^ "      sll_fatal_error(\"Inexhaustive pattern matching\");\n"
    ^ "  }\n"
    ^ "  sll_roots = m.header.next;\n"
    ^ "  return result;\n}\n\n");
  Names.emit header;
  Buffer.add_buffer header buf;
  Buffer.contents header

let free_vars term =
  let rec helper acc = function
    | `Var vname -> vname :: acc
    | `Ctr (_, args) | `FCall (_, args) ->
        List.fold_left helper acc args
    | `GCall (_, parg, args) ->
        List.fold_left helper (helper acc parg) args
  in
  List.rev (helper [] term)

let emit_main ~strict { term; _ } =
  let fvars = free_vars term in
  let main_term_args = List.map (fun var ->
    `FCall ("sll_read_value", [
      `Var ("\"" ^ var ^ "\""); `Var "constructor_names"; `Var "SllNumofCtrs"]))
    fvars
  in
  emit_fdef ~strict "create_main_term_without_io"
    { fargs = fvars; fbody = term; }
  ^ emit_fdef ~strict:true "create_main_term"
      { fargs = [];
        fbody = `FCall ("create_main_term_without_io", main_term_args); }

let emit_defs ~strict defs emitter =
  let buf = Buffer.create 16 in
  Ident_map.iter (fun name def ->
    Buffer.add_string buf (emitter ~strict (mangle name) def)) defs;
  Buffer.contents buf

let emit_fdefs ~strict { fdefs; _ } = emit_defs ~strict fdefs emit_fdef
let emit_gdefs ~strict { gdefs; _ } = emit_defs ~strict gdefs emit_gdef

let emit ?(strict = true) ({ fdefs; gdefs; term; } as p) =
  let buffer = Buffer.create 16 in
  List.iter (fun emitter ->
    Buffer.add_string buffer (emitter p))
    [ emit_prolog;
      emit_declarations;
      emit_gdefs ~strict;
      emit_fdefs ~strict;
      emit_main ~strict;
      emit_epilog
    ];
  Buffer.contents buffer
