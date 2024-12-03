open Cfg
open Cast

(*Le squelette de code donné ne sert ici que de guide, vous êtes libre de choisir une implémentation différente*)
type next_instr = Label of label | Block of block

type data = {
  globals : string list; 
  arg_map : int VarMap.t;
  local_map : int VarMap.t;
  next : next_instr;
  graph : graph;
}

let rec transform_global_decl { globals; functions } = function
  | CDECL (_, name, _) -> { globals = name :: globals; functions }
  | CFUN (_, name, args, _, (_, code)) ->
      let function_cfg = transform_fun_decl globals name args code in
      { globals; functions = function_cfg :: functions }

and transform_fun_decl globals name args code =
  let _, arg_map =
    List.fold_left
      (fun (i, m) decl ->
        match decl with
        | CDECL (_, x, _) -> (i + 1, VarMap.add x i m)
        | _ -> assert false)
      (0, VarMap.empty) args
  in
  let ret_reg = reg_of_string "dummy" in
  let block = ([], Return ret_reg) in
  let label, graph = add_block block empty_graph in
  let next = Label label in
  let local_map = VarMap.empty in
  let data = transform_code { globals; arg_map; local_map; next; graph } code in
  match data.next with
  | Label l -> { name; graph = data.graph; entry = l }
  | Block b ->
      let entry, graph = add_block b data.graph in
      { name; graph; entry }

and convert_local_decl data = function
  | CDECL (_, name, _) ->
      (*à modifier pour que les variables locales soient associées à leur position dans la bible*)
      { data with local_map = VarMap.add name 0 data.local_map }
  | _ -> assert false

and transform_code data c =
  match c with
  | CBLOCK (var_decls, loc_code) ->
      let outer_locals = data.local_map in
      let code = List.map snd loc_code in
      let local_map = data.local_map in
      let data =
        List.fold_left convert_local_decl { data with local_map } var_decls
      in
      let data =
        List.fold_right (fun c data -> transform_code data c) code data
      in
      (*il faut que les variables locales définies dans le block ne soient plus retenues,
        il faudra également dépiler celles ci*)
      { data with local_map = outer_locals }
  | _ -> failwith "TODO"

and transform_expr r data e =
  let (l,ex) = e in
  match ex with
  | VAR _ -> Load (r,l),r
  | CST v -> Cst (r,v),r
  | STRING s -> Cst_string (r,s),r
  | SET_VAR (s,loc) -> Store (l,r),r
  | SET_VAL (s,loc) ->Store (l,r),r
  | CALL (s,loc) ->Store (l,r),r
  | OP1 (op,loc) -> begin
                      match op with
                      | M_MINUS -> let _,r1 =  (transform_expr r data e) in Monop (r, MINUS, r1),r
                      | _ -> failwith "TODO"
                    end
  | OP2 (op, loc1, loc2) ->failwith "TODO"
  | CMP (cmp, loc1, loc2) -> failwith "TODO"
  | EIF (loc1, loc2, loc3) ->failwith "TODO"
  | ESEQ ll ->failwith "TODO"

let transform_program =
  List.fold_left transform_global_decl { globals = []; functions = [] }
