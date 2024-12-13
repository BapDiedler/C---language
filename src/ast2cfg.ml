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
    incr cpt;
    let c = !cpt in
   reg_of_string ("R_"^(string_of_int c))

(* fonction qui crée un nouveau block pour les if *)
let fusion_block data =
  match data.next with
  | Label l-> l, data.graph
  | Block b-> add_block b data.graph

(* fonction nous donnant la localisation en mémoire de s dans data *)
let check_location s data =
    match VarMap.find_opt s data.local_map with (*check si local*)
   | None -> begin 
               match VarMap.find_opt s data.arg_map with (*check si arg*)
               | None -> Global s (*check si global*)
               | Some i -> Param i
             end
   | Some i -> Local i

(* fonction qui permet d'initailiser pour des if *)
let creat_if data =
  let l,g = match data.next with
            | Label l -> l,data.graph
            | Block b -> add_block b data.graph
  in 
  {data with next = Block ([],(Jmp(l))); graph = g}
  
(* fonction qui vérifie l'existance d'une fonction main *)
let check_name_main dec_list =
  let names = List.map (fun a -> match a with CDECL (_, name, _) -> name | CFUN (_, name, _, _, _) -> name) dec_list in
  match List.find_opt (fun a -> a = "main") names with
  | None -> failwith "no main"
  | _ -> ()

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
  let data = { globals; arg_map; local_map; next; graph} in
  let data = transform_code data code in
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
      ({ data with local_map = VarMap.add name i data.local_map},i+1)
  | _ -> assert false

(* transformation du code *)
and transform_code data c =
  match c with
  (* Transformation du code d'un block *)
  | CBLOCK (var_decls, loc_code) ->
                    let outer_locals = data.local_map in
                    let code = List.map snd loc_code in
                    let local_map = data.local_map in
                    let data,_ =
                      List.fold_left convert_local_decl ({ data with local_map},0)  var_decls
                    in
                    (* gestion de la pile pour les variables locales *)
                    let data  = match data.next with
                                | Label l-> {data with next = Block ([ShiftStack (-List.length var_decls)],Jmp l)}
                                | Block (nf,f)-> {data with next = Block (ShiftStack (-List.length var_decls)::nf,f)}
                    in
                    let data =
                      List.fold_right (fun c data -> transform_code data c) code data
                    in
                    (* gestion de la pile pour les variables locales *)
                    let data  = match data.next with
                                | Label l-> {data with next = Block ([ShiftStack (List.length var_decls)],Jmp l)}
                                | Block (nf,f)-> {data with next = Block (ShiftStack (List.length var_decls)::nf,f)}
                    in
                    (*il faut que les variables locales définies dans le block ne soient plus retenues*)
                    { data with local_map = outer_locals }

  (* Transformation du code d'une expression *)
  | CEXPR loc -> transform_expr (new_reg ()) data loc
    
  (* Transformation du code d'un return; *)
  | CRETURN None -> let r = new_reg () in
                    {data with next = Block([ShiftStack (-VarMap.cardinal data.local_map);Cst (r,0)], Return r)}
            
  (* Transformation du code d'un return v; *)
  | CRETURN Some loc -> 
                  let new_r = new_reg () in
                let data = {data with next = Block (([ShiftStack (-VarMap.cardinal data.local_map)], Return new_r))} in
                transform_expr new_r data loc
           
  (* Transformation du code d'un if *)
  | CIF (loc1,(_,loc2),(_,loc3)) -> 
                  let new_r1 = new_reg () in
                  let data = creat_if data in
                  let data1 = transform_code data loc2 in
                  (* récupère le premier block *)
                  let l1, g1 =  fusion_block data1 in
                  let data2 = transform_code {data with graph = g1} loc3 in
                  (* récupère le deuxième block *)
                  let l2, g2 =  fusion_block data2 in
                  transform_expr new_r1 {data with next = Block ([],JmpC(new_r1, l1, l2)); graph = g2} loc1

  (* Transformation du code d'un while *)
  | CWHILE (loc_expr,(_,code1)) ->
                let new_r = new_reg () in
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
                                                        | Block b -> add_block b data.graph))
                in
                transform_expr new_r {data_bis with next = Block ([],JmpC (new_r,label,l1)); graph = g} loc_expr

(* transforme les expressions *)
and transform_expr r data e =

  (* fonction qui transforme data en y ajoutant new_f *)
  let change_data data new_nf =
      match data.next with
      | Label l-> {data with next = Block([new_nf],Jmp l)}
      | Block (nf,f)-> {data with next = Block (new_nf::nf,f)}
  in

  let (l,ex) = e in
  match ex with
  (* récupérer la variable v *)
  | VAR v ->  change_data data (Load (r, check_location v data))
          
  (* utilisation de la constante v *)
  | CST v ->  change_data data (Cst (r,v))

  (* utilisation de la constante s *)
  | STRING s -> change_data data (Cst_string (r,s))

  (* aoute loc à s *)
  | SET_VAR (s,loc) ->  let data =  change_data data (Store (check_location s data, r)) in
                        transform_expr r data loc

  (* aoute loc à s *)                  
  | SET_VAL (s,loc) ->  let data = change_data data (StoreI (check_location s data, r))in
                        transform_expr (new_reg ()) data loc

  (* appel la fonction s avec les arguments loc *)
  | CALL (s,loc) -> let new_nf = match VarMap.find_opt s data.local_map with (*check si local*)
                                | None -> begin match VarMap.find_opt s data.arg_map with (*check si arg*)
                                          | None -> Call (s) (*check si global*)(*création de registre pour au dessus?*)
                                          | Some i -> CallR (r)
                                          end
                                | Some i -> CallR (r)
                    in
                    let new_r = new_reg () in
                    let data = change_data data (ShiftStack (-List.length loc)) in (* dépile des paramètres *)
                    let data = change_data data (Pop r) in (* récupère la valeur de retour qui est au sommet de la pile*)
                    let data = change_data data new_nf in (* ajout de l'appel de fonction *)
                    List.fold_right (fun e data ->  let data = change_data data (Push new_r) in
                                                    transform_expr new_r data e
                                    ) loc data (* empile les paramètres *)

  (* opération unaire sur loc *)
  | OP1 (op,loc) -> let get_name l = (* récupère le nom de la variable *)
                      let (_,e) = l in
                      match e with
                      | VAR s -> s
                      | OP1(M_DEREF,(_,VAR s)) -> s
                      | _ -> failwith "error in OP1"
                    in
                    let get_store l data = (* check le store pour savoir si c'est pointer ou non *)
                      let (_,e) = l in
                      match e with
                      | VAR s -> Store (check_location s data,r)
                      | OP1(M_DEREF,(_,VAR s)) -> StoreI (check_location s data,r)
                      | _ -> failwith "error in OP1"
                    in
                    let get_load l data = (* check le load pour savoir si c'est pointer ou non *)
                      let (_,e) = l in
                      match e with
                      | VAR s -> [Load (r,check_location s data)]
                      (* pour les pointeurs il faut load et deref la variable *)
                      | OP1(M_DEREF,(_,VAR s)) -> let new_r = new_reg () in [Load (new_r,check_location s data); Monop(r,DEREF,new_r)]
                      | _ -> failwith "error in OP1"
                    in
                    let aux new_nf = (* fonction auxiliaire qui modifie data avec des pop et des push *)
                      let data = change_data data new_nf in
                      let data = change_data data (Pop r) in
                      let data = transform_expr r data loc in
                      change_data data (Push r)
                    in
                    (match op with
                    (* -x *)
                    | M_MINUS -> aux (Monop (r,MINUS,r))
                    (* ~x *)
                    | M_NOT -> aux (Monop (r,NOT,r))
                    (* x++ *)
                    | M_POST_INC -> let data = change_data data (Monop (r,ADDI (-1),r)) in
                                    let data = change_data data (get_store loc data) in
                                    let data = change_data data (Monop (r,ADDI 1, r)) in
                                    change_data data (Load (r,check_location (get_name loc) data))
                    (* ++x *)               
                    | M_PRE_INC ->  let data = change_data data (get_store loc data) in
                                    let data = change_data data (Monop (r,ADDI 1, r)) in
                                    let new_nf = get_load loc data in
                                    (
                                      match data.next with
                                      | Label l-> {data with next = Block (new_nf,Jmp l)}
                                      | Block (nf,f)-> {data with next = Block (new_nf@nf,f)}
                                    )
                    (* x-- *)
                    | M_POST_DEC -> let data = change_data data (Monop (r,ADDI 1,r)) in
                                    let data = change_data data (get_store loc data) in
                                    let data = change_data data (Monop (r,ADDI (-1), r)) in
                                    let new_nf = get_load loc data in
                                    (
                                      match data.next with
                                      | Label l-> {data with next = Block (new_nf,Jmp l)}
                                      | Block (nf,f)-> {data with next = Block (new_nf@nf,f)}
                                    )
                    (* --x *)
                    | M_PRE_DEC ->  let data = change_data data (get_store loc data) in
                                    let data = change_data data (Monop (r,ADDI (-1), r)) in
                                    change_data data (Load (r,check_location (get_name loc) data))
                    (* *x *)
                    | M_DEREF -> aux (Monop (r,DEREF,r))
                    (* &x *)
                    | M_ADDR -> aux (Monop (r,MOV,r))
                    )

  (* opération binaire sur loc1 et loc2 *)
  | OP2 (op, loc1, loc2) -> let new_r1 = new_reg () in 
                            let new_r2 = new_reg () in
                            let new_nf = match op with (* récuparation de l'opération *)
                                        | S_MUL -> Binop (r, MUL, new_r1, new_r2)
                                        | S_DIV -> Binop (r, DIV, new_r1, new_r2)
                                        | S_MOD -> Binop (r, MOD, new_r1, new_r2)
                                        | S_ADD -> Binop (r, ADD, new_r1, new_r2)
                                        | S_SUB -> Binop (r, SUB, new_r1, new_r2)
                            in
                            let data = change_data data new_nf in
                            let data = change_data data (Pop r) in
                            let data = transform_expr (new_r2) data loc2 in
                            let data = change_data data (Pop new_r2) in
                            let data = transform_expr (new_r1) data loc1 in
                            let data = change_data data (Push new_r2) in
                            change_data data (Push r)

  (* opération de comparaison entre loc1 et loc2 *)                          
  | CMP (cmp, loc1, loc2) ->  let new_r1 = new_reg () in
                              let new_r2 = new_reg () in
                              let new_nf = match cmp with
                                          | C_LT -> Binop (r, CMP_LT, new_r1, new_r2)
                                          | C_LE -> Binop (r, CMP_LE, new_r1, new_r2)
                                          | C_EQ -> Binop (r, CMP_EQ, new_r1, new_r2)
                              in
                              let data = change_data data new_nf in
                              let data = change_data data (Pop r) in
                              let data = transform_expr (new_r2) data loc2 in
                              let data = change_data data (Pop new_r2) in
                              let data = transform_expr (new_r1) data loc1 in
                              let data = change_data data (Push new_r2) in
                              change_data data (Push r)

  (* opération conditionnelle sur des expressions *)
  | EIF (loc1, loc2, loc3) -> let data = creat_if data in
                            let data1 = transform_expr r data loc2 in
                            let l1, g1 = fusion_block data1 in
                            let data2 = transform_expr r {data with graph = g1} loc3 in
                            let l2, g2 = fusion_block data2 in
                            (transform_expr (r) {data with next = Block ([],JmpC(r, l1, l2)); graph = g2} loc1)

  (* exécution en séquence d'expressions *)
  | ESEQ ll -> List.fold_right (fun c (data) -> let new_r = new_reg () in transform_expr new_r data c) ll data

(* transforme le programme *)
let transform_program dec_list =
  check_name_main dec_list;
  List.fold_left transform_global_decl { globals = []; functions = [] } dec_list