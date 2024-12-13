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
  let cpt = ref (-1) in
  fun v ->
    match v with
    | 1 ->  incr cpt;
            let c = !cpt in
            reg_of_string ("R_"^(string_of_int c))
    | _ ->  let c = !cpt in
            decr cpt;
            reg_of_string ("R_"^(string_of_int c))

let info_var =
  let hash = Hashtbl.create 10 in
  fun s v ->
    match v with
    | None -> Hashtbl.find_opt hash s
    | Some i -> Hashtbl.add hash s i; Some i
    


(* let new_reg =
  let arr = Array.make 8 true in
  fun s ->
    match s with
    | None -> let found = ref None in
              Array.iteri (fun index b ->
                if b && !found = None then (
                  arr.(index) <- false;
                  found := Some index
                )
              ) arr;
              let c = match !found with None -> failwith "no register" | Some c -> c in
              reg_of_string ("R_"^(string_of_int c))
    | Some ({reg_name = "-1"}) -> Array.fill arr 0 (Array.length arr) true; reg_of_string ("R_0")
    | Some ({reg_name = r}) -> let i = int_of_char(String.get r (String.length r - 1)) in
                arr.(i) <- true;
                reg_of_string ("R_"^(string_of_int i))
     *)

(* fonction nous donnant la localisation en mémoire de s dans data *)
let check_location s data =
    match VarMap.find_opt s data.local_map with (*check si local*)
   | None -> begin 
               match VarMap.find_opt s data.arg_map with (*check si arg*)
               | None -> Global s (*check si global*)
               | Some i -> Param i
             end
   | Some i -> Local i

let creat_if data =
  let l,g = match data.next with
            | Label l -> l,data.graph
            | Block b -> add_block b data.graph
  in 
  {data with next = Block ([],(Jmp(l))); graph = g}
  
let check_name_main dec_list =
  let names = List.map (fun a -> match a with CDECL (_, name, _) -> name | CFUN (_, name, _, _, _) -> name) dec_list in
  match List.find_opt (fun a -> a = "main") names with
  | None -> failwith "no main"
  | _ -> ()

let rec check_const (data: data) : data =
  match data.next with
  | Label l -> let b = get_block l data.graph in check_block b data
  | Block b -> check_block b data

and check_block b data =
  let (nf,f) = b in
  match nf,f with
  | [], f -> check_final_instr f data
  | nf::nf_ll, f -> let data = check_non_final_instr nf data in
                    check_block (nf_ll,f) data

and check_non_final_instr nf data =

  let change_data data new_nf =
    match data.next with
    | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf::nf,f)) data.graph}
    | Block (nf,f)-> {data with next = Block (new_nf::nf,f)}
  in

  match nf with
  | Nop -> change_data data Nop
  | Cst (r,i) -> let _ = info_var r (Some i) in change_data data (Cst (r,i))
  | Cst_string (r,s) -> change_data data (Cst_string (r,s))
  | Store (l,r) -> change_data data (Store (l,r))
  | StoreI (l,r) -> change_data data (StoreI (l,r))
  | Load (r,l) -> change_data data (Load (r,l))
  | Addr (r,l) -> change_data data (Addr (r,l))
  | Push r -> change_data data (Push r)
  | Pop r -> change_data data (Pop r)
  | ShiftStack i -> change_data data (ShiftStack i)
  | Monop (r1,op,r2) -> change_data data (Monop (r1,op,r2))
  | Binop (r1,op,r2,r3) -> change_data data (Binop (r1,op,r2,r3))
  | Call s -> change_data data (Call s)
  | CallR r -> change_data data (CallR r)

and check_final_instr f data =
  match f with
  | Jmp l -> {data with next = Block ([],Jmp l)}
  | Return r -> {data with next = Block ([],Return r)}
  | JmpC (r,l1,l2) -> (match info_var r None with
                      | None -> data
                      | Some 0 -> {data with next = Block ([],Jmp l1)}
                      | Some i -> {data with next = Block ([],Jmp l2)}
                      )

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
  let ret_reg = (reg_of_string "dummy") in
  let block = ([], Return ret_reg) in
  let label, graph = add_block block empty_graph in
  let next = Label label in
  let local_map = VarMap.empty in
  let data = transform_code { globals; arg_map; local_map; next; graph } code in
  (* let data = check_const data in change le graphe de flot de contrôle en remplassant les éléments constants par des constantes *)
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
                      List.fold_left convert_local_decl ({ data with local_map },0)  var_decls
                    in
                    let data =
                      List.fold_right (fun c data -> transform_code data c) code data
                    in
                    (*il faut que les variables locales définies dans le block ne soient plus retenues,
                      il faudra également dépiler celles ci*)
                    { data with local_map = outer_locals }

  | CEXPR loc -> let d = transform_expr (new_reg 1) data loc in
                 d (* que faire de b *)
                  
  | CRETURN None -> let r = new_reg 1 in
                  let l,g = 
                  begin
                    match data.next with
                    | Label l -> add_block ([Cst (r,0)], Return r) data.graph
                    | Block _ -> add_block ([Cst (r,0)],Return r) data.graph
                  end
                in
                {data with next = Label l; graph = g}
            
  | CRETURN Some loc -> 
                  let new_r = new_reg 1 in
                  let l,g = 
                  begin
                    match data.next with
                    | Label l -> add_block ([], Return new_r) data.graph
                    | Block (_,_) -> add_block ([],Return new_r) data.graph
                  end
                in
                let data = {data with next = Label l; graph = g} in
                let d = transform_expr new_r data loc in
                d

                (* (match v with
                | Cst (r,c) -> let l,g = 
                              begin
                                match data.next with
                                | Label l -> add_block ([Cst (new_r,c)], Return new_r) data.graph
                                | Block (_,_) -> add_block ([Cst (new_r,c)],Return new_r) data.graph
                              end
                            in
                            {data with next = Label l; graph = g}
                | _ -> d
                ) *)
                  
  | CIF (loc1,(_,loc2),(_,loc3)) -> 
                  let new_r1 = new_reg 1 in
                  let data = creat_if data in
                  let data1 = transform_code data loc2 in
                  let l1, g1 =  (
                                match data1.next with
                                | Label l-> l, data1.graph
                                | Block b-> add_block b data1.graph
                  )in
                  let data2 = transform_code {data with graph = g1} loc3 in
                  let l2, g2 =  (
                                match data2.next with
                                | Label l-> l, data2.graph
                                | Block b-> add_block b data2.graph          
                  )in
                  let d = transform_expr new_r1 {data with next = Block ([],JmpC(new_r1, l1, l2)); graph = g2} loc1 in
                  d

  | CWHILE (loc_expr,(_,code1)) ->
                let new_r = new_reg 1 in
                let data_bis = data in
                let l1, g = match data.next with
                          | Label l -> l,data.graph
                          | Block b -> add_block b data.graph
                in 
                let data = {data with next = Block ([],(Jmp(l1))); graph = g} in 
                let label,g = loop (fun l ->  let data = transform_expr new_r {data with next = Block ([],JmpC (new_r,l,l1))} loc_expr in
                                              let data = transform_code data code1 in 
                                              (match data.next with
                                                        | Label l -> l, data.graph
                                                        | Block b -> add_block b data.graph)) in
                                              

                let d = transform_expr new_r {data_bis with next = Block ([],JmpC (new_r,label,l1)); graph = g} loc_expr in
                d

(* transforme les expressions *)
and transform_expr r data e =

  let change_data data new_nf =
      match data.next with
      | Label l-> {data with graph = replace_block l (fun (nf,f) -> (new_nf::nf,f)) data.graph}
      | Block (nf,f)-> {data with next = Block (new_nf::nf,f)}
  in

  let (l,ex) = e in
  match ex with
  | VAR v ->  change_data data (Load (r, check_location v data))
            
  | CST v ->  change_data data (Cst (r,v))

  | STRING s -> change_data data (Cst_string (r,s))

  | SET_VAR (s,loc) ->  let data =  change_data data (Store (check_location s data, r)) in
                        transform_expr r data loc

  | SET_VAL (s,loc) ->  let data = change_data data (StoreI (check_location s data, r))in
                        transform_expr (new_reg 1) data loc

  | CALL (s,loc) -> let new_nf = match VarMap.find_opt s data.local_map with (*check si local*)
                                | None -> begin match VarMap.find_opt s data.arg_map with (*check si arg*)
                                          | None -> Call (s) (*check si global*)(*création de registre pour au dessus?*)
                                          | Some i -> CallR (r)
                                          end
                                | Some i -> CallR (r)
                    in
                    let new_r = new_reg 1 in
                    let add_reg e data =
                      change_data data (Pop new_r)
                    in
                    let data = List.fold_right add_reg loc data in
                    let data = change_data data (Pop r) in
                    let data = change_data data new_nf in
                    let add_reg e data =
                      let data = change_data data (Push new_r) in
                      transform_expr new_r data e
                    in
                    List.fold_right add_reg loc data

  | OP1 (op,loc) -> let get_name l =
                      let (_,e) = l in
                      match e with
                      | VAR s -> s
                      | OP1(M_DEREF,(_,VAR s)) -> s
                      | _ -> failwith "coucou"
                    in
                    let get_store l data =
                      let (_,e) = l in
                      match e with
                      | VAR s -> Store (check_location s data,r)
                      | OP1(M_DEREF,(_,VAR s)) -> StoreI (check_location s data,r)
                      | _ -> failwith "coucou"
                    in
                    let new_r = new_reg 1 in
                    let aux new_nf =
                      let data = change_data data new_nf in
                      let data = change_data data (Pop r) in
                      let data = transform_expr (new_r) data loc in
                      change_data data (Push r)
                    in
                    (match op with (* il faut mettre le registre que l'on vient de créer *)
                    | M_MINUS -> aux (Monop (r,MINUS,new_r))(* il faut récupérer un élément de la pile (loc) puis le modifier en fonction de *)
                    | M_NOT -> aux (Monop (r,NOT,new_r))
                    | M_POST_INC -> let new_nf = Monop (r,ADDI (-1),r) in
                                    let data = change_data data new_nf in
                                    let new_nf = get_store loc data in
                                    let data = change_data data new_nf in
                                    let new_nf = Monop (r,ADDI 1, r) in
                                    let data = change_data data new_nf in
                                    let new_nf = Load (r,check_location (get_name loc) data) in
                                    let data = change_data data new_nf in
                                    data
                    | M_PRE_INC ->  let new_nf = get_store loc data in
                                    let data = change_data data new_nf in
                                    let new_nf = Monop (r,ADDI 1, r) in
                                    let data = change_data data new_nf in
                                    let new_nf = Load (r,check_location (get_name loc) data) in
                                    let data = change_data data new_nf in
                                    data
                    | M_POST_DEC -> let new_nf = Monop (r,ADDI 1,r) in
                                    let data = change_data data new_nf in
                                    let new_nf = get_store loc data in
                                    let data = change_data data new_nf in
                                    let new_nf = Monop (r,ADDI (-1), r) in
                                    let data = change_data data new_nf in
                                    let new_nf = Load (r,check_location (get_name loc) data) in
                                    let data = change_data data new_nf in
                                    data
                    | M_PRE_DEC ->  let new_nf = get_store loc data in
                                    let data = change_data data new_nf in
                                    let new_nf = Monop (r,ADDI (-1), r) in
                                    let data = change_data data new_nf in
                                    let new_nf = Load (r,check_location (get_name loc) data) in
                                    let data = change_data data new_nf in
                                    data
                    | M_DEREF -> aux (Monop (r,DEREF,new_r))
                    | M_ADDR -> aux (Monop (r,MOV,new_r))
                    )

  | OP2 (op, loc1, loc2) -> let new_r1 = new_reg 1 in 
                            let new_r2 = new_reg 1 in
                            let new_nf,op = match op with
                                        | S_MUL -> Binop (r, MUL, new_r1, new_r2),( * )
                                        | S_DIV -> Binop (r, DIV, new_r1, new_r2),(/)
                                        | S_MOD -> Binop (r, MOD, new_r1, new_r2),(mod)
                                        | S_ADD -> Binop (r, ADD, new_r1, new_r2),(+)
                                        | S_SUB -> Binop (r, SUB, new_r1, new_r2),(-)
                            in
                            let data = change_data data new_nf in
                            let data = change_data data (Pop r) in
                            let data = transform_expr (new_r2) data loc2 in
                            let data = change_data data (Pop new_r2) in
                            let data = transform_expr (new_r1) data loc1 in
                            let data = change_data data (Push new_r2) in
                            change_data data (Push r)

                            

  | CMP (cmp, loc1, loc2) ->  let new_r1 = new_reg 1 in
                              let new_r2 = new_reg 1 in
                              let new_nf,op = match cmp with
                                          | C_LT -> Binop (r, CMP_LT, new_r1, new_r2),(<)
                                          | C_LE -> Binop (r, CMP_LE, new_r1, new_r2),(>)
                                          | C_EQ -> Binop (r, CMP_EQ, new_r1, new_r2),(=)
                              in
                              let data = change_data data new_nf in
                              let data = change_data data (Pop r) in
                              let data = transform_expr (new_r2) data loc2 in
                              let data = change_data data (Pop new_r2) in
                              let data = transform_expr (new_r1) data loc1 in
                              let data = change_data data (Push new_r2) in
                              change_data data (Push r)

  | EIF (loc1, loc2, loc3) -> let data = creat_if data in

                            let data1 = transform_expr r data loc2 in
                            let l1, g1 = match data1.next with
                                        | Label l-> l, data1.graph
                                        | Block b-> add_block b data1.graph
                            in

                            let data2 = transform_expr r {data with graph = g1} loc3 in
                            let l2, g2 = match data2.next with
                                        | Label l-> l, data2.graph
                                        | Block b-> add_block b data2.graph
                            in
                            (* que faire de b1 et b2 ? *)
                            (transform_expr (r) {data with next = Block ([],JmpC(r, l1, l2)); graph = g2} loc1)

  | ESEQ ll -> List.fold_right (fun c (data) -> let new_r = new_reg 1 in transform_expr new_r data c) ll data

(* transforme le programme *)
let transform_program dec_list =
  check_name_main dec_list;
  List.fold_left transform_global_decl { globals = []; functions = [] } dec_list
