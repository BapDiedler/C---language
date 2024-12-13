type label = { label : string }

let label_of_func_name name = { label = name }

type register = { reg_name : string }

let reg_of_string reg_name = { reg_name }

type monop = ADDI of int | MOV | DEREF | MINUS | NOT
type binop = MUL | DIV | MOD | ADD | SUB | CMP_EQ | CMP_LT | CMP_LE

type location =
  | FuncLabel of string
  | Global of string
  | Param of int
  | Local of int

type final_instruction =
  | Return of register
  | Jmp of label
  | JmpC of register * label * label

type non_final_instruction =
  | Nop
  | Cst of register * int
  | Cst_string of register * string
  | Store of location * register
  | StoreI of location * register
  | Load of register * location
  | Addr of register * location
  | Push of register
  | Pop of register
  | ShiftStack of int
  | Monop of register * monop * register
  | Binop of register * binop * register * register
  | Call of string
  | CallR of register

type block = non_final_instruction list * final_instruction

module VarMap = Map.Make (String)

module G = Map.Make (struct
  type t = label

  let compare = compare
end)

type graph = block G.t

let empty_graph : graph = G.empty

let new_label =
  let x = ref (-1) in
  fun () ->
    incr x;
    { label = Format.asprintf "L%i" !x }

let add_block b g =
  let l = new_label () in
  (l, G.add l b g)

let loop f =
  let l = new_label () in
  let l', g = f l in
  (l, G.add l (G.find l' g) (G.remove l' g))

let get_block l g =
  try G.find l g
  with _ ->
    failwith (Format.asprintf "Label %s not associated to any block" l.label)

let get_predecessors : label -> graph -> (label * block) list =
 fun l g ->
  G.filter
    (fun _ (_, final) ->
      match final with
      | JmpC (_, _, l') when l = l' -> true
      | (Jmp l' | JmpC (_, l', _)) when l = l' -> true
      | _ -> false)
    g
  |> G.bindings

let replace_block l f g =
  match G.find_opt l g with
  | Some b -> G.add l (f b) g
  | None -> failwith "Label not associated to any block"

type function_cfg = { name : string; graph : graph; entry : label }
type program = { globals : string list; functions : function_cfg list }

module PrettyPrinter = struct
  module S = Set.Make (struct
    type t = label

    let compare = compare
  end)

  let pp_register p { reg_name } = Format.fprintf p "%s" reg_name
  let pp_label p { label } = Format.fprintf p "%s" label

  let pp_monop p = function
    | ADDI i -> Format.fprintf p "%i +" i
    | MOV -> Format.fprintf p ""
    | MINUS -> Format.fprintf p "-"
    | NOT -> Format.fprintf p "!"
    | DEREF -> Format.fprintf p "*"

  let pp_binop p op =
    Format.fprintf p "%s"
      (match op with
      | MUL -> "*"
      | DIV -> "/"
      | MOD -> "%"
      | ADD -> "+"
      | SUB -> "-"
      | CMP_EQ -> "=="
      | CMP_LT -> "<"
      | CMP_LE -> "<=")

  let pp_location p =
    let open Format in
    function
    | FuncLabel s -> fprintf p "FuncLabel %s" s
    | Global s -> fprintf p "Global %s" s
    | Param i -> fprintf p "Param %i" i
    | Local i -> fprintf p "Local %i" i

  let pp_instr p =
    let open Format in
    function
    | Nop -> fprintf p "Nop"
    | Cst (r, i) -> fprintf p "%a <- %i" pp_register r i
    | Cst_string (r, s) -> fprintf p "%a <- %s" pp_register r s
    | Store (loc, r) -> fprintf p "%a <- %a" pp_location loc pp_register r
    | StoreI (loc, r) -> fprintf p "*%a <- %a" pp_location loc pp_register r
    | Load (r, loc) -> fprintf p "%a <- %a" pp_register r pp_location loc
    | Addr (r, loc) -> fprintf p "%a <- &(%a)" pp_register r pp_location loc
    | Push r -> fprintf p "Push %a" pp_register r
    | Pop r -> fprintf p "Pop %a" pp_register r
    | ShiftStack i -> fprintf p "ShiftStack %i" i
    | Monop (r1, monop, r2) ->
        fprintf p "%a <- %a %a" pp_register r1 pp_monop monop pp_register r2
    | Binop (r1, binop, r2, r3) ->
        fprintf p "%a <- %a %a %a" pp_register r1 pp_register r2 pp_binop binop
          pp_register r3
    | Call f -> fprintf p "Call %s" f
    | CallR r -> fprintf p "CallR %a" pp_register r

  let pp_instr_list =
    Format.pp_print_list ~pp_sep:(fun p () -> Format.fprintf p "\\n") pp_instr

  let rec pp_final p (label, final, graph, s) =
    match final with
    | Jmp l ->
        Format.fprintf p "Jmp\" shape=box]@;%a -> %a@;%a" pp_label label
          pp_label l pp_cfg (l, graph, s)
    | JmpC (r, l1, l2) ->
        Format.fprintf p "JmpC %a\" shape=box]@;%a -> %a@;%a -> %a@;%a@;%a"
          pp_register r pp_label label pp_label l1 pp_label label pp_label l2
          pp_cfg (l1, graph, s) pp_cfg (l2, graph, s)
    | Return r -> Format.fprintf p "Ret %a\" shape=box]" pp_register r

  and pp_cfg p (entry, graph, s) =
    if S.mem entry s then Format.fprintf p ""
    else
      let instr_list, final = get_block entry graph in
      Format.printf "tutu\n";
      Format.fprintf p "%a [label=\"%a\\n%a" pp_label entry pp_instr_list
        instr_list pp_final
        (entry, final, graph, S.add entry s)

  let pp_func_cfg p { name; graph; entry } =
    let s = S.empty in
    Format.fprintf p "%s [label=\"FUN %s\" shape=box]@;%s -> %a@;%a" name name
      name pp_label entry pp_cfg (entry, graph, s)
end

let dump_dot filename { functions; _ } =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "@[<hv 4>strict digraph G {@;%a@;@]}@]"
    (Format.pp_print_list
       ~pp_sep:(fun p () -> Format.fprintf p "@;")
       PrettyPrinter.pp_func_cfg)
    functions
