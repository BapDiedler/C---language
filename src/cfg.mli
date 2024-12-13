type label
(** Les types labels sont opaques et sont soit crées par [label_of_func_name],
     ou bien manipulés par 2 fonctions décrites plus tard*)

val label_of_func_name : string -> label
(** label ayant le nom de la function *)

type register = { reg_name : string }
(** A l'inverse, les registres sont totalement transparents et peuvent être manipulés explicitements*)

val reg_of_string : string -> register
(** [reg_of_string x = {reg_name = x}]*)

type monop =
  | ADDI of int
  | MOV
  | DEREF
  | MINUS
  | NOT
      (** Les opérations unaires sont réduites au minimum: 
   Ajout d'un entier, chargement d'un registre, déréférencement, moins, not*)

type binop =
  | MUL
  | DIV
  | MOD
  | ADD
  | SUB
  | CMP_EQ
  | CMP_LT
  | CMP_LE  (** Opérations arithmétiques et comparaisons*)

type location =
  | FuncLabel of string
  (*label de début de fonction*)
  | Global of string
  (*Une variable globale est identifiée explicitement par son nom*)
  | Param of int
  (*Un paramètre est identifié par son indice dans la pile d'arguments *)
  | Local of int
(*Une variable locale est identifiée par sa position dans la pile *)

type final_instruction =
  | Return of register
  (*instruction finale d'une fonction, avant de placer cette instruction, il faudra notamment
    - placer le résultat de la fonction dans le registre de retour
    - dépiler les variables locales de la fonction
  *)
  | Jmp of label
  (* instruction de saut vers un label *)
  | JmpC of register * label * label
(* instruction de saut conditionnel; saute vers le premier label si la valeur du registre est non nulle.*)

type non_final_instruction =
  | Nop
  | Cst of register * int
  (* Place une constante entière dans un registre *)
  | Cst_string of register * string
    (* Place une chaine de caractères dans un registre *)
  | Store of location * register
  (* x = e *)
  | StoreI of location * register
  (* *x = e *)
  | Load of register * location
  (* Lecture de la valeur d'une location *)
  | Addr of register * location
  (* Lecture de l'adresse d'une location *)
  | Push of register
  | Pop of register
  (* Pile ou dépile un registre dans la pile*)
  | ShiftStack of int
  (*Déplace le registre de pile*)
  | Monop of register * monop * register
  | Binop of register * binop * register * register
  (*Opérations de base *)
  | Call of string
  (* Comme pour Return, un appel de fonction se contente d'appeler la fonction.
     Pour s'assurer d'un comportement correct, il faudra notamment mettre les arguments de la function sur la pile,
     puis les dépiler après l'appel*)
  | CallR of register
(*appelle une fonction dont le nom a été placé dans le registre*)

type block = non_final_instruction list * final_instruction
(*Un bloc est une liste d'instruction terminée par un retour ou un saut*)

(**Sous module utilisé pour associer une variable à une valeur arbitraire,
     typiquement un registre, ou une position dans la liste d'arguments pour les arguements d'une fonction
     Ce module n'est pas indispensable en tant que tel, et vous êtes libre de choisir une autre implémentation*)
module VarMap : sig
  type 'a t

  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
  val cardinal : 'a t -> int
end

type graph
(**un graphe est un type opaque qu'on pourra manipuler avec les fonctions ci-dessous*)

val empty_graph : graph
(** graphe vide*)

(* Les deux fonctions ci-dessous sont les seules pouvant manipuler des labels*)

val add_block : block -> graph -> label * graph
(**Ajoute un block à un graphe et retourne une paire [(l,g)], avec [l] un nouveau label de début de bloc*)

val loop : (label -> label * graph) -> label * graph
(**Renvoie une paire [(l,g)] telle que [f l = loop f = l,g],
   nécessaire pour les boucles*)

val get_block : label -> graph -> block
(** [get_block l g] renvoie [b] si [l] est le début d'un block [b] dans [g]; 
      une erreur sinon*)

type function_cfg = { name : string; graph : graph; entry : label }
(**Le graphe d'une fonction est la donnée du nom de la fonction, son graphe, et un label d'entrée
     Le label d'entrée peut être crée avec [label_of_func_name]
     Le label d'entrée et le label de la fonction sont différents!
     Il faudra bien penser à relier les deux, et à rajouter des instructions si nécessaires*)

type program = { globals : string list; functions : function_cfg list }
(**Un programme est la liste des variables globales, et celle des graphes de fonctions*)

val dump_dot : string -> program -> unit
(**[dump_dot filename program] écrit tous les CFG d'un programme dans [filename] au format .dot*)
(*Les deux fonctions suivantes ne devraient pas être utilisées avant les passes d'optimisation*)

val get_predecessors : label -> graph -> (label * block) list
(**Retourne la liste des prédecesseurs d'un label *)

val replace_block : label -> (block -> block) -> graph -> graph
(**Remplace un bloc, on s'attendra à ce que le nouveau bloc pointe vers un sous ensemble des anciens labels*)
