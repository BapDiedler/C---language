open Cast
open Types

exception Error of location * string
exception Anomaly of string
val check_file : var_declaration list -> (string*typ) list
val print_typ_list : (string*typ) list -> unit