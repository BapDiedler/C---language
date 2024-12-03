open Cast
open Types

(*------EXCEPTIONS AREA------*)
exception Error of location * string
exception Anomaly of string

(*------VARIABLE AREA------*)
let env = Hashtbl.create 16 (*countains functions and variables*)
let env_fun_global = Hashtbl.create 16 (*countains global functions*)


(*------FUNCTIONS AREA------*)

(* converts typ_basic to string *)
let rec string_of_typ_basic v =
  match v with
  | Tint -> "int"
  | Tptr (Tfun f) -> "("^(string_of_typ (Tfun f))^")"
  | Tptr s -> "*"^string_of_typ s

(* converts typ to string *)
and string_of_typ v =
    match v with
    | Tscalar v -> string_of_typ_basic v
    | Tfun f when ( 0 <> List.length f.args) -> (List.fold_right (fun a b -> (string_of_typ_basic a)^" -> "^b) (f.args) "")
                                                ^(string_of_typ_basic f.result)
    | Tfun f -> "() -> "^string_of_typ_basic f.result

(* print list of typ *)
let print_typ_list = List.iter (fun a -> let (b,c) = a in (Printf.printf "%s: %s\n\n" b (string_of_typ c)))

(* converts bin_op to string *)
let string_of_bin_op op =
  match op with
  | S_MUL -> "mul"
  | S_DIV -> "div"
  | S_MOD -> "mod"
  | S_ADD -> "add"
  | S_SUB -> "sub"

(* converts mon_op to string *)
let string_of_mon_op op =
  match op with
  | M_MINUS -> "minus"
  | M_NOT -> "not"
  | M_POST_INC -> "post inc"
  | M_POST_DEC -> "post dec"
  | M_PRE_INC -> "pre inc"
  | M_PRE_DEC -> "pre inc"
  | M_DEREF -> "deref"
  | M_ADDR -> "addr"


(* testing equality between two typ *)
let rec equal_type_var l t1 t2 =
  match t1, t2 with
  | Tscalar s1, Tscalar s2 -> equal_type_basic_var l s1 s2
  (*f1 = f2 if f1.result = f2.result and f1.args = f2.args*)
  | Tfun f1, Tfun f2 when List.length f1.args = List.length f2.args -> 
              equal_type_basic_var l f1.result f2.result;
              let _ = List.map2 (equal_type_basic_var l) f1.args f2.args in ()
  | v1, v2 -> raise(Error(l,"incompatible types between "^ string_of_typ v1 ^" and "^ string_of_typ v2))

(* testing equality between two typ_basic *)
and equal_type_basic_var l t1 t2 =
  match t1, t2 with
  | Tint, Tint -> ()
  | Tptr s1, Tptr s2 -> equal_type_var l s1 s2
  | v1, v2 -> raise(Error(l, "incompatible types between "^ string_of_typ_basic v1 ^" and "^ string_of_typ_basic v2))


(* converts ctyp to typ *)
let rec typ_of_ctyp l e =
  match e with
  | TINT -> Tscalar Tint
  | TPTR p -> Tscalar (Tptr (typ_of_ctyp l p))
  | TFUN (r,arg) -> Tfun {result = (typ_basic_of_ctyp l r); args = (List.map (typ_basic_of_ctyp l) arg)}

(* converts ctyp to typ_basic *)
and typ_basic_of_ctyp l e =
  match e with
  | TINT -> Tint
  | TPTR p -> Tptr (typ_of_ctyp l p)
  | _ -> Tptr (typ_of_ctyp l e) (* pointeur de fonction*)
  

(* testing equality between declaration arguments and used arguments *)
let check_fun_args l s args =
  let args = List.map (fun arg -> match arg with
                                  | Tscalar s -> s
                                  | Tfun f -> Tptr arg
                      ) args in
    match Hashtbl.find_opt env s with
    | Some (Tfun f) when List.length args <> List.length f.args ->
                    raise(Error(l,"the number of declared arguments does not match the number of used arguments"))
    | Some (Tfun f) -> let _ = List.map2 (equal_type_basic_var l) args f.args in
                       None
    | _ -> raise (Error(l,"the function "^s^" has not been declared"))


(* get typ from env *)
let get_typ l e =
  match Hashtbl.find_opt env e with
  | Some s -> s
  | None -> raise (Error(l,e^" doesn't exist"))


(* get ctyp of declaration from arguments *)
let rec get_dec_ctyp l =
  match l with
	| [] -> []
	| CDECL(loc,s,t)::q -> t::(get_dec_ctyp q)
	| CFUN(l,s,vl,t,lc)::q -> raise (Error(l,"the function "^s^" has not been declared"))


(* check if function is rede *)
let is_redefined l s =
  match Hashtbl.find_opt env_fun_global s with
  | Some 0 -> false
  | Some 1 -> true
  | _ -> true


(*------MAINS FUNCTIONS AREA------*)

(* checks expressions *)
let rec check_expr ast =
  let (l,e) = ast in
  match e with
  (* checks variable *)
  | VAR s -> get_typ l s
  (* checks constant *)
  | CST i -> Tscalar Tint
  (* checks string *)
  | STRING s -> raise (Error(l,"couldn't have string for expression"))
  (* checks variable *)
  | SET_VAR (s, loc_expr1) ->
      if is_redefined l s then
        let t = check_expr loc_expr1 in
        equal_type_var l t (get_typ l s);
        get_typ l s
      else
        raise(Error(l,"impossible to change global function "^s))
  (* checks value *)
  | SET_VAL (s, loc_expr1) ->
                  begin
                    match get_typ l s with
                    | Tscalar (Tptr _) -> begin
                                            if is_redefined l s then
                                              let t = check_expr loc_expr1 in
                                              equal_type_var l t (get_typ l s);
                                              get_typ l s
                                            else
                                              raise(Error(l,"impossible to change global function "^s))
                                          end
                    | _ -> raise(Error(l,s^" isn't a ptr"))
                  end
    
  (* checks call to function *)
  | CALL (s, loc_expr_list) ->
          begin
            match s with (* check name's function *)
            | "print_int" ->  begin
                                match List.map check_expr loc_expr_list with
                                | Tscalar Tint :: [] -> Tscalar Tint
                                | _ -> raise(Error(l,"the function print_int takes one parameter (int)"))
                              end
            | "print_string" -> begin
                                  match loc_expr_list with
                                  | (_,STRING s) :: [] -> Tscalar Tint
                                  | _ -> raise(Error(l,"the function print_string takes one parameter (string)"))
                                end
            | _ ->  let args = List.map (fun e -> let t = check_expr e in t) loc_expr_list in
                    let _ = check_fun_args l s args in
                    begin
                      match get_typ l s with
                      | Tfun f -> Tscalar f.result
                      | _ -> raise(Error(l,s^" isn't a function"))
                    end
          end
  (* checks operation with one argument *)
  | OP1 (op, loc_expr1) ->
          let rec is_lvalue l = (* checks if expression is lvalue *)
              let (loc,ex) = l in
              match ex with
              | VAR v -> true
              | OP1(op,l) when op = M_DEREF -> is_lvalue l
              | _ -> false
          in
          begin
            match op with (* impossible to operate in some case *)
            | M_MINUS | M_POST_INC | M_POST_DEC 
            | M_PRE_INC | M_PRE_DEC | M_ADDR 
                        when not (is_lvalue loc_expr1) -> raise (Error(l,"impossible to "^string_of_mon_op op^" because the expression aren't a variable")) 
            | _ -> ()
          end;
          let t = check_expr loc_expr1 in
          begin
            match t with (* checks expression type *)
            | Tscalar Tint -> 
                        begin
                          match op with
                          | M_DEREF -> raise(Error(l,"impossible to "^string_of_mon_op op^" an \027[31mint\027[0m"))
                          | M_ADDR -> Tscalar (Tptr t)
                          | _ -> t
                        end
            | Tscalar (Tptr s) -> 
                        begin
                          match op with
                          | M_POST_INC | M_POST_DEC | M_PRE_INC | M_PRE_DEC -> t
                          | M_DEREF -> s
                          | M_ADDR -> Tscalar (Tptr t)
                          | _ -> raise(Error(l,"impossible to "^string_of_mon_op op^" an ptr"))
                        end
            | Tfun f -> raise(Error(l,"impossible to "^string_of_mon_op op^" with function"))
          end
  (* checks operation with two arguments *)
  | OP2 (op, loc_expr1, loc_expr2) ->
          let t1 = check_expr loc_expr1 in
          let t2 = check_expr loc_expr2 in
          begin
            match t1, t2 with (* check types of expressions*)
            (* int o int *)
            | Tscalar Tint, Tscalar Tint -> Tscalar Tint
            (* int o ptr *)
            | Tscalar Tint, Tscalar (Tptr v) ->
                        begin
                          match op with
                          | S_ADD -> Tscalar (Tptr v)
                          | _ -> raise(Error(l,"impossible to "^string_of_bin_op op^" int and ptr"))
                        end
            (* ptr o int *)
            | Tscalar (Tptr v), Tscalar Tint -> 
                        begin
                          match op with
                          | S_ADD | S_SUB -> Tscalar (Tptr v)
                          | _ -> raise(Error(l,"impossible to "^string_of_bin_op op^" int and ptr"))
                        end
            (* ptr o ptr *)
            | Tscalar (Tptr v1), Tscalar (Tptr v2) -> 
                        begin
                          match op with
                          | S_SUB -> equal_type_var l v2 v1;
                                     Tscalar Tint
                          | _ -> raise(Error(l,"impossible to "^string_of_bin_op op^" ptr and ptr"))
                        end
            | _ -> raise(Error(l,"impossible to "^string_of_bin_op op^" with function"))
          end
  (* checks compare *)
  | CMP (op, loc_expr1, loc_expr2) ->
          let t1 = check_expr loc_expr1 in
          let t2 = check_expr loc_expr2 in
          equal_type_var l t1 t2;
          Tscalar Tint
  (* checks condition *)
  | EIF (loc_expr1, loc_expr2, loc_expr3) ->
          let t1 = check_expr loc_expr1 in
          let t2 = check_expr loc_expr2 in
          let t3 = check_expr loc_expr3 in
          equal_type_var l t1 (Tscalar Tint);
          equal_type_var l t2 t3;
          t2
  (* checks sequence *)
  | ESEQ (loc_expr_list) -> 
          let ll = List.rev_map (fun e -> check_expr e) loc_expr_list in
          begin
            match ll with
            | t::next -> t
            | [] -> Tscalar Tint (* skipe typing with int *)
          end

(* checks declarations *)
and check_var_declaration ast =
  let check_name l s = (* check if name is valid *)
    match s with
    | "print_int" -> raise(Error(l,"impossible to redefine print_int"))
    | "print_string" -> raise(Error(l,"impossible to redefine print_string"))
    | _ -> ()
  in
  match ast with
  (* no declaration *)
  | [] -> []
  (* variable declaration *)
  | CDECL (loc, s, typ) :: ll ->
              check_name loc s;
              begin
                match Hashtbl.find_opt env s with
                | None -> Hashtbl.add env s (typ_of_ctyp loc typ);
                | Some v -> raise(Error(loc,s^" can only be declared once"))
              end;
              s::(check_var_declaration ll)
  (* function declaration *)
  | CFUN (loc, s, var_decl_list, typ, loc_cod) :: ll ->
              check_name loc s;

              (* check arguments name *)
              let names_args = check_var_declaration_block s loc var_decl_list in
              let rec has_duplicates = function
                | [] -> false
                | x :: xs -> List.mem x xs || has_duplicates xs
              in
              if has_duplicates names_args then
                raise(Error(loc,"impossible to have two variables with the same name as arguments of "^s))
              else

                (* add to hashtables *)
                let typ_list_args = get_dec_ctyp var_decl_list in
                Hashtbl.add env_fun_global s 0;
                begin
                  match Hashtbl.find_opt env s with
                  | None -> Hashtbl.add env s (typ_of_ctyp loc (TFUN(typ,typ_list_args)));
                  | Some v -> raise(Error(loc,s^" cann only be declared once"))
                end;

                (* checks code *)
                match check_code s loc_cod with
                | _, false -> raise(Error(loc,s^" needs a return for every branchs"))
                | t, _ -> begin
                            equal_type_var loc t (typ_of_ctyp loc typ);
                            List.iter (Hashtbl.remove env) names_args;
                            List.iter (Hashtbl.remove env_fun_global) names_args;
                            s::(check_var_declaration ll)
                          end

(* checks code *)
and check_code s ast =
  let (l,c) = ast in
  match c with
  | CBLOCK (var_decl_list, loc_code_list) -> 
            check_block s l var_decl_list loc_code_list
  | CEXPR (loc_expr1) ->
            let t = check_expr loc_expr1 in
            (t, false)
  | CIF (loc_expr1, loc_co1, loc_co2) ->
            let t = check_expr loc_expr1 in
            equal_type_var l t (Tscalar Tint);
            let t1, b1 = check_code s loc_co1 in
            let t2, b2 = check_code s loc_co2 in
            begin (* check return *)
              if b1 then
                (t1,b2)
              else
                (t2,b1)
            end
  | CWHILE(loc_expr1, loc_co)-> 
            let t = check_expr loc_expr1 in
            equal_type_var l t (Tscalar Tint);
            let t1,_ = check_code s loc_co in
            (t1, false)
  | CRETURN None-> (* equals to return int *)
                begin
                  match (get_typ l s) with
                  | Tfun f -> equal_type_var l (Tscalar f.result) (Tscalar Tint);
                              (Tscalar Tint), true
                  | _ -> raise(Error(l,s^" isn't a function"))
                  end
  | CRETURN Some v -> let t = check_expr v in
                      begin
                      match get_typ l s with
                      | Tfun f -> equal_type_var l (Tscalar f.result) t;
                                  t, true
                      | _ -> raise(Error(l,s^" isn't a function"))
                      end

(* checks block *)
and check_block  s loc var_list code_list =
  let rec has_duplicates = function
  | [] -> false
  | x :: xs -> List.mem x xs || has_duplicates xs
  in
  (* no redefinition of variables *)
  if has_duplicates (List.map (fun v -> match v with CDECL(_,s,_) -> s | CFUN(_,s,_,_,_) -> s) var_list) then
    raise(Error(loc,"impossible to declare same variable two times in same block"))
  else

    (* check declaration in var_list *)
    let l = List.fold_left (fun a b -> a@(check_var_declaration_block s loc [b]))  [] var_list in

    (* check code in code_list *)
    let code_list = List.map (check_code s) code_list in

    (* check if all ways have return *)
    let b = List.fold_left (fun acc (_,b) -> acc || b) false code_list in


    List.iter (Hashtbl.remove env) l;
    List.iter (fun a -> match Hashtbl.find_opt env_fun_global a with 
                        | None -> ()
                        | Some _ -> (Hashtbl.add env_fun_global a 0)
              ) l;

    (* check return value *)
    match List.find_opt (fun (t,b) -> b) code_list with
    | None when List.length code_list > 0 -> let (t,_) = List.hd(List.rev code_list) in t,b
    | Some (t,_) -> t,b
    | _ -> (Tscalar Tint),b

(* check declaration of variable in block *)
and check_var_declaration_block fun_s l ast =
  match ast with
  | [] -> []

  | CDECL (loc, s, typ) :: ll -> 
              (* add to env *)
              Hashtbl.add env s (typ_of_ctyp loc typ);
              begin (* change value in env_fun_global if it's redefinition of function*)
                match Hashtbl.find_opt env_fun_global s with
                | None -> ();
                | Some t -> Hashtbl.replace env_fun_global s 1;
              end;
              s::(check_var_declaration_block fun_s l ll)

  | CFUN (loc, s, var_decl_list, typ, loc_cod):: ll ->
              let typ_list_args = get_dec_ctyp var_decl_list in
              begin (* add to env *)
                match Hashtbl.find_opt env s with
                | None -> Hashtbl.add env s (typ_of_ctyp loc (TFUN(typ,typ_list_args)));
                | Some v -> raise(Error(loc,s^" can only be declared once"))
              end;
              begin (* change value in env_fun_global if it's redefinition of function*)
                match Hashtbl.find_opt env_fun_global s with
                | None -> ();
                | Some t -> Hashtbl.replace env_fun_global s 1;
              end;
              s :: (check_var_declaration_block fun_s l ll)

(* MAIN FUNCTION *)
let check_file f =
  let _ = check_var_declaration f in
  Hashtbl.fold (fun key value acc -> (key, value) :: acc) env []