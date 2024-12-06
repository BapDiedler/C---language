open Cfg
open Cast

(*Le squelette de code donné ne sert ici que de guide, vous êtes libre de choisir une implémentation différente*)
type next_instr = Label of label | Block of block

(* structure contenant les données de notre fonction *)
type data = {
  globals : string list;  
  arg_map : int VarMap.t;
  local_map : int VarMap.t;
  next : next_instr;
  graph : graph;
}

(* fonction de création d'un nouveau registre *)
let new_reg =
  let cpt = ref 0 in
  fun () -> let c = !cpt in
            incr cpt;
            reg_of_string ("R_"^(string_of_int c))

(* fonction nous donnant la localisation en mémoire de s dans data *)
let check_location s data =
    match VarMap.find_opt s data.local_map with (*check si local*)
   | None -> begin 
               match VarMap.find_opt s data.arg_map with (*check si arg*)
               | None -> Global s (*check si global*)
               | Some i -> Param i
             end
   | Some i -> Local i
  

(* cette fonction passe de l'ast vers cfg pour les globales declarations *)
let rec transform_global_decl { globals; functions } = function
  | CDECL (_, name, _) -> { globals = name :: globals; functions }
  | CFUN (_, name, args, _, (_, code)) ->
      let function_cfg = transform_fun_decl globals name args code in
      { globals; functions = function_cfg :: functions }

(* la fonction transforme les déclarations de fonction en fonction pour cfg *)
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

(* la fonction transforme les déclarations locales *)
and convert_local_decl (data,i) = function
  | CDECL (_, name, _) ->
      (*à modifier pour que les variables locales soient associées à leur position dans la bible*)
      ({ data with local_map = VarMap.add name i data.local_map },i+1)
  | _ -> assert false

(* transformation du code *)
and transform_code data c =
  match c with
  | CBLOCK (var_decls, loc_code) ->
          let outer_locals = data.local_map in
          let code = List.map snd loc_code in
          let local_map = data.local_map in
          let data,_ =
            List.fold_left convert_local_decl ({ data with local_map },0) var_decls
          in
          let data =
            List.fold_right (fun c data -> transform_code data c) code data
          in
          (*il faut que les variables locales définies dans le block ne soient plus retenues,
            il faudra également dépiler celles ci*)
          { data with local_map = outer_locals }

  | CEXPR loc ->
          let outer_locals = data.local_map in
          let data = transform_expr (new_reg ()) data loc in
          { data with local_map = outer_locals }

  | CRETURN None -> begin (* appel récursif sur next ? *)
                      match data.next with
                      | Label l-> {data with graph = replace_block l (fun (nf,_) -> (nf,Return (new_reg ()))) data.graph}
                      | Block (nf,_)-> {data with next = Block (nf,Return (new_reg ()))}
                    end
            
  | CRETURN Some loc -> 
                  let new_r = new_reg () in
                  let data = transform_expr new_r data loc in
                  begin (* appel récursif sur next ? *)
                    match data.next with
                    | Label l-> {data with graph = replace_block l (fun (nf,_) -> (nf,Return new_r)) data.graph}
                    | Block (nf,_)-> {data with next = Block (nf,Return new_r)}
                  end

  | _ -> failwith "TODO"

(* transforme les expressions *)
and transform_expr r data e =
  let (l,ex) = e in
  match ex with
  | VAR v ->  begin (* appel récursif sur next ? *)
                match data.next with
                | Label l-> {data with graph = replace_block l (fun (nf,f) -> (Load (r, check_location v data)::nf,f)) data.graph}
                | Block (nf,f)-> {data with next = Block (Load (r, check_location v data)::nf,f)}
              end

  | CST v ->  begin (* appel récursif sur next ? *)
                match data.next with
                | Label l-> {data with graph = replace_block l (fun (nf,f) -> (Cst (r,v)::nf,f)) data.graph}
                | Block (nf,f)-> {data with next = Block (Cst (r,v)::nf,f)}
              end

  | STRING s -> begin (* appel récursif sur next ? *)
                  match data.next with
                  | Label l-> {data with graph = replace_block l (fun (nf,f) -> (Cst_string (r,s) ::nf,f)) data.graph}
                  | Block (nf,f)-> {data with next = Block (Cst_string (r,s) ::nf,f)}
                end

  | SET_VAR (s,loc) ->  
                        let new_nf = Store (check_location s data, r) in
                        let data =  begin (* appel récursif sur next ? *)
                                      match data.next with
                                      | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf ::nf,f)) data.graph}
                                      | Block (nf,f)-> {data with next = Block (new_nf ::nf,f)}
                                    end 
                        in
                        transform_expr (new_reg ()) data loc (* changer le graphe de data avec res *)

  | SET_VAL (s,loc) ->   (* changer le graphe de data avec res *)
                        let new_nf = StoreI (check_location s data, r) in
                        let data = 
                                  begin (* appel récursif sur next ? *)
                                    match data.next with
                                    | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf ::nf,f)) data.graph}
                                    | Block (nf,f)-> {data with next = Block (new_nf ::nf,f)}
                                  end
                      in
                      transform_expr (new_reg ()) data loc

  | CALL (s,loc) ->
                    let new_nf = match VarMap.find_opt s data.local_map with (*check si local*)
                            | None -> begin match VarMap.find_opt s data.arg_map with (*check si arg*)
                                      | None -> Call (s) (*check si global*)(*création de registre pour au dessus?*)
                                      | Some i -> CallR (r)
                                      end
                            | Some i -> CallR (r)
                    in
                    let data =
                              begin (* appel récursif sur next ? *)
                                match data.next with
                                | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf ::nf,f)) data.graph}
                                | Block (nf,f)-> {data with next = Block (new_nf ::nf,f)}
                              end
                    in
                    List.hd (List.map (transform_expr (new_reg ()) data) loc)

  | OP1 (op,loc) -> let new_r = new_reg () in
                    let new_nf =
                      match op with (* il faut mettre le registre que l'on vient de créer *)
                      | M_MINUS -> Monop (r,MINUS,new_r)
                      | M_NOT -> Monop (r,NOT,new_r)
                      | M_POST_INC | M_PRE_INC -> Monop (r,ADDI 1,new_r)
                      | M_POST_DEC | M_PRE_DEC -> Monop (r,ADDI (-1),new_r)
                      | M_DEREF -> Monop (r,DEREF,new_r)
                      | M_ADDR -> Monop (r,MOV,new_r)
                    in
                    let data = 
                              begin (* appel récursif sur next ? *)
                                match data.next with
                                | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf ::nf,f)) data.graph}
                                | Block (nf,f)-> {data with next = Block (new_nf ::nf,f)}
                              end
                  in
                  transform_expr (new_r) data loc
                  
  | OP2 (op, loc1, loc2) -> let new_r1 = new_reg () in let new_r2 = new_reg () in
                            let new_nf = 
                                    match op with
                                    | S_MUL -> Binop (r, MUL, new_r1, new_r2)
                                    | S_DIV -> Binop (r, DIV, new_r1, new_r2)
                                    | S_MOD -> Binop (r, MOD, new_r1, new_r2)
                                    | S_ADD -> Binop (r, ADD, new_r1, new_r2)
                                    | S_SUB -> Binop (r, SUB, new_r1, new_r2)
                            in
                            let data = begin (* appel récursif sur next ? *)
                              match data.next with
                              | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf ::nf,f)) data.graph}
                              | Block (nf,f)-> {data with next = Block (new_nf ::nf,f)}
                            end in
                            let data = transform_expr (new_r2) data loc2 in
                            transform_expr (new_r1) data loc1

  | CMP (cmp, loc1, loc2) ->  let new_r1 = new_reg () in
                              let new_r2 = new_reg () in
                              let new_nf =
                                      match cmp with
                                      | C_LT -> Binop (r, CMP_LT, new_r1, new_r2)
                                      | C_LE -> Binop (r, CMP_LE, new_r1, new_r2)
                                      | C_EQ -> Binop (r, CMP_EQ, new_r1, new_r2)
                              in
                              let data = 
                                        begin (* appel récursif sur next ? *)
                                          match data.next with
                                          | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf ::nf,f)) data.graph}
                                          | Block (nf,f)-> {data with next = Block (new_nf ::nf,f)}
                                        end
                              in
                              let data = transform_expr (new_r1) data loc1 in
                              transform_expr (new_r2) data loc2
  | EIF (loc1, loc2, loc3) -> failwith "TODO"
  | ESEQ ll -> failwith "TODO"

(* transforme le programme *)
let transform_program =
  List.fold_left transform_global_decl { globals = []; functions = [] }
