open Cfg

(*
* IL FAUT MAINTENANT MERGE CORRECTEMENT DEUX ENV CAR ON PEUT MODIF DANS DEUX BRANCHES UNE VARIABLE ET DONC IL FAUT LA PASSER À BOT
*)

(* Définition des types *)
type variable = Reg of register | Loc of location

(* Création d'une Map de variable *)
module VariableMap = Map.Make(struct
  type t = variable
  let compare = compare
end)

type env_local = {
  value : (int option) VariableMap.t
}

(* Création d'une Map de label *)
module LabelMap = Map.Make(struct
  type t = (label*int)
  let compare = compare
end)

(* fonction affichant un VaraibleMap *)
let rec print_VariableMap =
  VariableMap.iter (fun v i ->  let i = match i with None -> "bot" | Some i -> string_of_int i in
                                (match v with 
                                | Reg r -> Printf.printf "%s : %s |" r.reg_name i
                                | Loc l -> (match l with
                                            | Global s -> Printf.printf "%s : %s |" s i
                                            | FuncLabel s -> Printf.printf "%s : %s |" s i
                                            | Param p -> Printf.printf "param_%d : %s |" p i
                                            | Local p -> Printf.printf "local_%d : %s |" p i
                                )
                                )
                    )

(* fonction affichant un LabelMap *)
let rec print_LabelMap env =
  LabelMap.iter (fun (l,i) {value} -> Printf.printf "(";print_label l;Printf.printf ",%d) -> " i; print_VariableMap value; Printf.printf "\n") env

(* variable permettant de savoir où l'on se trouve dans le block *)
let indiceInst = ref (-1)
let functions = ref []

(* fonction qui simplifie les constantes dans un programme *)
let rec check_const_fun f =
  let env : env_local LabelMap.t = LabelMap.empty in
  check_const_block f.entry f.graph env
  
and find_function name list = (* il ne faut pas oublier de gérer l'appel récursif *)
  match list with (* recherche de la focntion main *)
    | f::ll when f.name = name -> check_const_fun f
    | f::ll -> find_function name ll
    | _ -> failwith "error name function" (* il est impossible de ne pas avoir de fonction main *)

(* fonction qui modifie un block *)
and check_const_block l graph env =
  let old_indice = !indiceInst in
  let (nf,f) = get_block l graph in
  let env = List.fold_left (fun env nf -> let new_env = check_const_non_final l nf graph env in
                                          LabelMap.union (fun (label,i) a b -> Some a ) env new_env
                            ) env nf in (* mise à jour sur les nf *)
  let env = check_const_final l f graph env in(* mise à jour sue les f *)
  indiceInst := old_indice;
  env

(* fonction modifie une instruction non finale *)
and check_const_non_final label nf graph env =
  incr indiceInst;
  (* on récupère l'environnement local à label et indiceInst *)
  let local_env = match LabelMap.find_opt (label,!indiceInst-1) env with
                  | Some m -> m
                  | None -> {value : (int option) VariableMap.t = VariableMap.empty}
  in
  match nf with
  | Cst (r,v) ->  let value = VariableMap.add (Reg r) (Some v) local_env.value in
                  LabelMap.add (label,!indiceInst) {value} env

  | Store (l,r) -> (match VariableMap.find_opt (Reg r) local_env.value with (* check si r est constant *)
                    | Some i -> let value = VariableMap.add (Loc l) i local_env.value in (* ajout de l entant que constante *)
                                LabelMap.add (label,!indiceInst) {value} env
                    | None -> env
                   )

  | StoreI (l,r) -> (match VariableMap.find_opt (Reg r) local_env.value with (* check si r est constant *)
                    | Some i -> let value = VariableMap.add (Loc l) i local_env.value in (* ajout de l entant que constante *)
                                LabelMap.add (label,!indiceInst) {value} env
                    | None -> env
                    )

  | Load (r,l) ->   (match VariableMap.find_opt (Loc l) local_env.value with (* check si l est constant *)
                    | Some i -> let value = VariableMap.add (Reg r) i local_env.value in (* ajout de r entant que constante *)
                                LabelMap.add (label,!indiceInst) {value} env
                    | None -> env
                    )
  | Addr (r,l) ->   (match VariableMap.find_opt (Loc l) local_env.value with (* check si l est constant *)
                    | Some i -> let value = VariableMap.add (Reg r) i local_env.value in (* ajout de r entant que constante *)
                                LabelMap.add (label,!indiceInst) {value} env
                    | None -> env
                    )
                    
  | Monop (r1, op, r2) -> (match VariableMap.find_opt (Reg r2) local_env.value with (* check si r2 est constant. 
                        !!!!!!!!!!!!!!!!!!!! Mais il faut voir dans les autres tables !!!!!!!!!!!*)
                          | Some Some i -> (match op with
                                            | NOT ->  let value = VariableMap.add (Reg r1) (Some (lnot i)) local_env.value in (* ajout de r entant que constante *)
                                                      LabelMap.add (label,!indiceInst) {value} env
                                            | MINUS ->  let value = VariableMap.add (Reg r1) (Some (-i)) local_env.value in (* ajout de r entant que constante *)
                                                        LabelMap.add (label,!indiceInst) {value} env
                                            | ADDI v -> let value = VariableMap.add (Reg r1) (Some (v + i)) local_env.value in (* ajout de r entant que constante *)
                                                        LabelMap.add (label,!indiceInst) {value} env
                                            | MOV ->  let value = VariableMap.add (Reg r1) (Some i) local_env.value in (* ajout de r entant que constante *)
                                                      LabelMap.add (label,!indiceInst) {value} env
                                            | DEREF -> env
                                            )
                          | _ -> env
                          )
  | Binop (r1, op, r2, r3) -> (match VariableMap.find_opt (Reg r2) local_env.value with (* check si l est constant *)
                              | Some Some i1 -> (match VariableMap.find_opt (Reg r3) local_env.value with (* check si l est constant *)
                                          | Some Some i2 -> let op =  ( match op with
                                                                  | ADD -> (+)
                                                                  | DIV -> (/)
                                                                  | MOD -> (mod)
                                                                  | SUB -> (-)
                                                                  | MUL -> ( * )
                                                                  | _ -> failwith "coucou" (* ne sait pas quoi faire *)
                                                                )
                                                        in
                                                        let value = VariableMap.add (Reg r1) (Some(op i1 i2)) local_env.value in (* ajout de r entant que constante *)
                                                        LabelMap.add (label,!indiceInst) {value} env
                                          | _ -> env
                                          )
                              | _ -> env
                              )
  | Call s -> find_function s !functions
  | CallR r -> env (* voir quoi faire en cas de fonction *)
  | _ -> env

(* fonction qui modifie une instruction finale *)
and check_const_final label f graph env =
  let local_env = match LabelMap.find_opt (label,!indiceInst) env with
                  | Some m -> m
                  | None -> {value : int option VariableMap.t = VariableMap.empty}
  in
  match f with
  | Jmp l -> let new_env = check_const_block l graph env in LabelMap.union (fun (label,i) a b -> Some a ) env new_env
  | Return r -> env
  | JmpC (r,l1,l2) -> let new_env1 = check_const_block l1 graph env in
                      let new_env2 = check_const_block l2 graph env in
                      let new_env3 = LabelMap.union (fun (label,i) a b -> Some a) new_env1 new_env2 in
                      (match VariableMap.find_opt (Reg r) local_env.value with (* check si l est constant *)
                      | Some i -> let value = VariableMap.add (Reg r) i local_env.value in (* ajout de r entant que constante *)
                                  LabelMap.add (label,!indiceInst) {value} new_env3
                      | None -> new_env3
                      )

(* fonction principale qui commence l'exécution par main *)
let analysis_const prog =
  let rec start_main = function (* recherche de la focntion main *)
    | f::ll when f.name = "main" -> check_const_fun f
    | f::ll -> start_main ll
    | _ -> failwith "error main" (* il est impossible de ne pas avoir de fonction main *)
  in
  functions := prog.functions;
  start_main !functions