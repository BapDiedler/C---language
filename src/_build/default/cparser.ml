
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | TILDE_CHR
    | SUB_CHR
    | STRING_LITERAL of (
# 45 "cparser.mly"
       (string)
# 18 "cparser.ml"
  )
    | STAR_CHR
    | SEMI_CHR
    | RETURN
    | QUES_CHR
    | OR_OP
    | OPEN_PAREN_CHR
    | OPEN_BRACE_CHR
    | OPEN_ANGLE_CHR
    | NE_OP
    | MOD_CHR
    | LE_OP
    | INTEGER
    | INC_OP
    | IF
    | IDENTIFIER of (
# 43 "cparser.mly"
       (string)
# 37 "cparser.ml"
  )
    | GE_OP
    | FOR
    | EQ_OP
    | EQ_CHR
    | EOF
    | ELSE
    | DIV_CHR
    | DEC_OP
    | CONSTANT of (
# 44 "cparser.mly"
       (int)
# 50 "cparser.ml"
  )
    | COMMA_CHR
    | COLON_CHR
    | CLOSE_PAREN_CHR
    | CLOSE_BRACE_CHR
    | CLOSE_ANGLE_CHR
    | BANG_CHR
    | AND_OP
    | AMP_CHR
    | ADD_CHR
  
end

include MenhirBasics

# 1 "cparser.mly"
  

(*
 *	Copyright (C) 2024 by Laboratoire Méthodes Formelles (LMF),
 *      UMR 9021  Université Paris-Saclay, CNRS et ENS Paris-Saclay.
 *      Modified by Mihaela Sighireanu.
 *
 *      Copyright (C) 2005, 2006 by Laboratoire Spécification et Vérification (LSV),
 *      UMR 8643 CNRS & ENS Cachan.
 *      Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *      Permission is granted to anyone to use this software for any
 *      purpose on any computer system, and to redistribute it freely,
 *      subject to the following restrictions:
 *
 *      1. Neither the author nor its employer is responsible for the consequences of use of
 *              this software, no matter how awful, even if they arise
 *              from defects in it.
 *
 *      2. The origin of this software must not be misrepresented, either
 *              by explicit claim or by omission.
 *
 *      3. Altered versions must be plainly marked as such, and must not
 *              be misrepresented as being the original software.
 *
 *      4. This software is restricted to non-commercial use only.  Commercial
 *              use is subject to a specific license, obtainable from LMF.
 *
*)

(* Analyse syntaxique d'un sous-ensemble (tres) reduit de C.
 *)

open Cast

exception Parse_error of Cast.location * string
let sup_locator loc1 loc2 = 
  let st1, end1 = loc1 in
  let st2, end2 = loc2 in st1, end2


# 108 "cparser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_file) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: file. *)

  | MenhirState003 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 003.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState006 : ((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_state
    (** State 006.
        Stack shape : type_specifier OPEN_PAREN_CHR STAR_CHR.
        Start symbol: file. *)

  | MenhirState010 : (((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 010.
        Stack shape : type_specifier OPEN_PAREN_CHR STAR_CHR identifier CLOSE_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState013 : ((((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_type_specifier_list, _menhir_box_file) _menhir_state
    (** State 013.
        Stack shape : type_specifier OPEN_PAREN_CHR STAR_CHR identifier CLOSE_PAREN_CHR type_specifier_list.
        Start symbol: file. *)

  | MenhirState014 : (((((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_type_specifier_list, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 014.
        Stack shape : type_specifier OPEN_PAREN_CHR STAR_CHR identifier CLOSE_PAREN_CHR type_specifier_list type_specifier.
        Start symbol: file. *)

  | MenhirState016 : ((((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 016.
        Stack shape : type_specifier OPEN_PAREN_CHR STAR_CHR identifier CLOSE_PAREN_CHR type_specifier.
        Start symbol: file. *)

  | MenhirState021 : ((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_state
    (** State 021.
        Stack shape : type_specifier identifier.
        Start symbol: file. *)

  | MenhirState023 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 023.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState029 : (((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_parameter_list, _menhir_box_file) _menhir_state
    (** State 029.
        Stack shape : type_specifier identifier parameter_list.
        Start symbol: file. *)

  | MenhirState037 : (('s, _menhir_box_file) _menhir_cell1_function_declarator, _menhir_box_file) _menhir_state
    (** State 037.
        Stack shape : function_declarator.
        Start symbol: file. *)

  | MenhirState040 : (('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_state
    (** State 040.
        Stack shape : open_block.
        Start symbol: file. *)

  | MenhirState044 : (('s, _menhir_box_file) _menhir_cell1_STRING_LITERAL, _menhir_box_file) _menhir_state
    (** State 044.
        Stack shape : STRING_LITERAL.
        Start symbol: file. *)

  | MenhirState049 : (('s, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 049.
        Stack shape : OPEN_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState056 : (('s, _menhir_box_file) _menhir_cell1_unary_operator, _menhir_box_file) _menhir_state
    (** State 056.
        Stack shape : unary_operator.
        Start symbol: file. *)

  | MenhirState063 : (('s, _menhir_box_file) _menhir_cell1_postfix_expression, _menhir_box_file) _menhir_state
    (** State 063.
        Stack shape : postfix_expression.
        Start symbol: file. *)

  | MenhirState066 : (('s, _menhir_box_file) _menhir_cell1_inc_op, _menhir_box_file) _menhir_state
    (** State 066.
        Stack shape : inc_op.
        Start symbol: file. *)

  | MenhirState069 : (('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_state
    (** State 069.
        Stack shape : identifier.
        Start symbol: file. *)

  | MenhirState072 : (('s, _menhir_box_file) _menhir_cell1_unary_expression, _menhir_box_file) _menhir_state
    (** State 072.
        Stack shape : unary_expression.
        Start symbol: file. *)

  | MenhirState075 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 075.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState078 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_state
    (** State 078.
        Stack shape : multiplicative_expression STAR_CHR.
        Start symbol: file. *)

  | MenhirState079 : (('s, _menhir_box_file) _menhir_cell1_dec_op, _menhir_box_file) _menhir_state
    (** State 079.
        Stack shape : dec_op.
        Start symbol: file. *)

  | MenhirState086 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 086.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState088 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 088.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState092 : (('s, _menhir_box_file) _menhir_cell1_additive_expression _menhir_cell0_SUB_CHR, _menhir_box_file) _menhir_state
    (** State 092.
        Stack shape : additive_expression SUB_CHR.
        Start symbol: file. *)

  | MenhirState094 : (('s, _menhir_box_file) _menhir_cell1_additive_expression _menhir_cell0_ADD_CHR, _menhir_box_file) _menhir_state
    (** State 094.
        Stack shape : additive_expression ADD_CHR.
        Start symbol: file. *)

  | MenhirState096 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 096.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState098 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 098.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState100 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 100.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState103 : (('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_state
    (** State 103.
        Stack shape : logical_or_expression.
        Start symbol: file. *)

  | MenhirState105 : (('s, _menhir_box_file) _menhir_cell1_logical_and_expression, _menhir_box_file) _menhir_state
    (** State 105.
        Stack shape : logical_and_expression.
        Start symbol: file. *)

  | MenhirState109 : (('s, _menhir_box_file) _menhir_cell1_equality_expression, _menhir_box_file) _menhir_state
    (** State 109.
        Stack shape : equality_expression.
        Start symbol: file. *)

  | MenhirState111 : (('s, _menhir_box_file) _menhir_cell1_equality_expression, _menhir_box_file) _menhir_state
    (** State 111.
        Stack shape : equality_expression.
        Start symbol: file. *)

  | MenhirState115 : ((('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 115.
        Stack shape : logical_or_expression expression.
        Start symbol: file. *)

  | MenhirState116 : ((('s, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR, _menhir_box_file) _menhir_state
    (** State 116.
        Stack shape : expression COMMA_CHR.
        Start symbol: file. *)

  | MenhirState119 : (((('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR, _menhir_box_file) _menhir_state
    (** State 119.
        Stack shape : logical_or_expression expression COLON_CHR.
        Start symbol: file. *)

  | MenhirState122 : (('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_state
    (** State 122.
        Stack shape : logical_or_expression.
        Start symbol: file. *)

  | MenhirState127 : ((('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_state
    (** State 127.
        Stack shape : identifier argument_expression_list.
        Start symbol: file. *)

  | MenhirState128 : (((('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR, _menhir_box_file) _menhir_state
    (** State 128.
        Stack shape : identifier argument_expression_list COMMA_CHR.
        Start symbol: file. *)

  | MenhirState132 : ((('s, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 132.
        Stack shape : OPEN_PAREN_CHR expression.
        Start symbol: file. *)

  | MenhirState138 : (('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_state
    (** State 138.
        Stack shape : whilekw.
        Start symbol: file. *)

  | MenhirState139 : ((('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 139.
        Stack shape : whilekw expression.
        Start symbol: file. *)

  | MenhirState140 : (((('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 140.
        Stack shape : whilekw expression close_paren.
        Start symbol: file. *)

  | MenhirState144 : (('s, _menhir_box_file) _menhir_cell1_return, _menhir_box_file) _menhir_state
    (** State 144.
        Stack shape : return.
        Start symbol: file. *)

  | MenhirState146 : ((('s, _menhir_box_file) _menhir_cell1_return, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 146.
        Stack shape : return expression.
        Start symbol: file. *)

  | MenhirState151 : (('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_state
    (** State 151.
        Stack shape : ifkw.
        Start symbol: file. *)

  | MenhirState152 : ((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 152.
        Stack shape : ifkw expression.
        Start symbol: file. *)

  | MenhirState153 : (((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 153.
        Stack shape : ifkw expression CLOSE_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState156 : (('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_state
    (** State 156.
        Stack shape : forkw.
        Start symbol: file. *)

  | MenhirState157 : ((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_state
    (** State 157.
        Stack shape : forkw expression_statement.
        Start symbol: file. *)

  | MenhirState158 : (((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_state
    (** State 158.
        Stack shape : forkw expression_statement expression_statement.
        Start symbol: file. *)

  | MenhirState159 : ((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 159.
        Stack shape : forkw expression_statement expression_statement expression.
        Start symbol: file. *)

  | MenhirState160 : (((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 160.
        Stack shape : forkw expression_statement expression_statement expression close_paren.
        Start symbol: file. *)

  | MenhirState163 : (('s, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 163.
        Stack shape : expression.
        Start symbol: file. *)

  | MenhirState166 : ((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 166.
        Stack shape : forkw expression_statement expression_statement close_paren.
        Start symbol: file. *)

  | MenhirState169 : ((((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement, _menhir_box_file) _menhir_state
    (** State 169.
        Stack shape : ifkw expression CLOSE_PAREN_CHR compound_statement.
        Start symbol: file. *)

  | MenhirState172 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 172.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState173 : ((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list, _menhir_box_file) _menhir_state
    (** State 173.
        Stack shape : open_block statement_list.
        Start symbol: file. *)

  | MenhirState180 : ((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_state
    (** State 180.
        Stack shape : open_block declaration_list.
        Start symbol: file. *)

  | MenhirState181 : (((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list, _menhir_box_file) _menhir_state
    (** State 181.
        Stack shape : open_block declaration_list statement_list.
        Start symbol: file. *)

  | MenhirState189 : (('s, _menhir_box_file) _menhir_cell1_external_declaration, _menhir_box_file) _menhir_state
    (** State 189.
        Stack shape : external_declaration.
        Start symbol: file. *)


and ('s, 'r) _menhir_cell1_additive_expression = 
  | MenhirCell1_additive_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_argument_expression_list = 
  | MenhirCell1_argument_expression_list of 's * ('s, 'r) _menhir_state * (Cast.loc_expr list)

and ('s, 'r) _menhir_cell1_close_paren = 
  | MenhirCell1_close_paren of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position)

and ('s, 'r) _menhir_cell1_compound_statement = 
  | MenhirCell1_compound_statement of 's * ('s, 'r) _menhir_state * (Cast.loc_code) * Lexing.position

and ('s, 'r) _menhir_cell1_dec_op = 
  | MenhirCell1_dec_op of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position)

and ('s, 'r) _menhir_cell1_declaration_list = 
  | MenhirCell1_declaration_list of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_equality_expression = 
  | MenhirCell1_equality_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_expression_statement = 
  | MenhirCell1_expression_statement of 's * ('s, 'r) _menhir_state * (Cast.loc_expr) * Lexing.position

and ('s, 'r) _menhir_cell1_external_declaration = 
  | MenhirCell1_external_declaration of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_forkw = 
  | MenhirCell1_forkw of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position)

and ('s, 'r) _menhir_cell1_function_declarator = 
  | MenhirCell1_function_declarator of 's * ('s, 'r) _menhir_state * ((Cast.location * string) * Cast.var_declaration list * Cast.ctyp)

and ('s, 'r) _menhir_cell1_identifier = 
  | MenhirCell1_identifier of 's * ('s, 'r) _menhir_state * (Cast.location * string)

and ('s, 'r) _menhir_cell1_ifkw = 
  | MenhirCell1_ifkw of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position) * Lexing.position

and ('s, 'r) _menhir_cell1_inc_op = 
  | MenhirCell1_inc_op of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position)

and ('s, 'r) _menhir_cell1_logical_and_expression = 
  | MenhirCell1_logical_and_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_logical_or_expression = 
  | MenhirCell1_logical_or_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_multiplicative_expression = 
  | MenhirCell1_multiplicative_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_open_block = 
  | MenhirCell1_open_block of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position)

and ('s, 'r) _menhir_cell1_parameter_list = 
  | MenhirCell1_parameter_list of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_postfix_expression = 
  | MenhirCell1_postfix_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_relational_expression = 
  | MenhirCell1_relational_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_return = 
  | MenhirCell1_return of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_statement_list = 
  | MenhirCell1_statement_list of 's * ('s, 'r) _menhir_state * (Cast.loc_code list)

and ('s, 'r) _menhir_cell1_type_specifier = 
  | MenhirCell1_type_specifier of 's * ('s, 'r) _menhir_state * (Cast.ctyp)

and ('s, 'r) _menhir_cell1_type_specifier_list = 
  | MenhirCell1_type_specifier_list of 's * ('s, 'r) _menhir_state * (Cast.ctyp list)

and ('s, 'r) _menhir_cell1_unary_expression = 
  | MenhirCell1_unary_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_unary_operator = 
  | MenhirCell1_unary_operator of 's * ('s, 'r) _menhir_state * (Cast.location * token)

and ('s, 'r) _menhir_cell1_whilekw = 
  | MenhirCell1_whilekw of 's * ('s, 'r) _menhir_state * (Lexing.position * Lexing.position)

and 's _menhir_cell0_ADD_CHR = 
  | MenhirCell0_ADD_CHR of 's * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_CLOSE_PAREN_CHR = 
  | MenhirCell1_CLOSE_PAREN_CHR of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and 's _menhir_cell0_CLOSE_PAREN_CHR = 
  | MenhirCell0_CLOSE_PAREN_CHR of 's * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_COLON_CHR = 
  | MenhirCell1_COLON_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COMMA_CHR = 
  | MenhirCell1_COMMA_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OPEN_PAREN_CHR = 
  | MenhirCell1_OPEN_PAREN_CHR of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_STAR_CHR = 
  | MenhirCell0_STAR_CHR of 's * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_STRING_LITERAL = 
  | MenhirCell1_STRING_LITERAL of 's * ('s, 'r) _menhir_state * (
# 45 "cparser.mly"
       (string)
# 529 "cparser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_SUB_CHR = 
  | MenhirCell0_SUB_CHR of 's * Lexing.position * Lexing.position

and _menhir_box_file = 
  | MenhirBox_file of (Cast.var_declaration list) [@@unboxed]

let _menhir_action_001 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 144 "cparser.mly"
                        ( _sloc, ADD_CHR   )
# 546 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_002 =
  fun _1 ->
    (
# 175 "cparser.mly"
            ( _1 )
# 554 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_003 =
  fun _1 _3 ->
    (
# 177 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_ADD, _1, _3)
	)
# 564 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_004 =
  fun _1 _3 ->
    (
# 181 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_SUB, _1, _3)
	)
# 574 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_005 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 149 "cparser.mly"
                        ( _sloc, AMP_CHR   )
# 585 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_006 =
  fun _1 ->
    (
# 226 "cparser.mly"
                              ( _1 )
# 593 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_007 =
  fun _1 ->
    (
# 109 "cparser.mly"
                                ( [_1] )
# 601 "cparser.ml"
     : (Cast.loc_expr list))

let _menhir_action_008 =
  fun _1 _3 ->
    (
# 110 "cparser.mly"
                                                                   ( 
          _3 :: _1 )
# 610 "cparser.ml"
     : (Cast.loc_expr list))

let _menhir_action_009 =
  fun _1 ->
    (
# 263 "cparser.mly"
                                 ( _1 )
# 618 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_010 =
  fun _1 _3 ->
    (
# 265 "cparser.mly"
     (
	     let locvar, left = _1 in
	     let loc = sup_locator locvar (loc_of_expr _3) in
	     match left with
	       VAR x -> loc, SET_VAR (x, _3)
	     | OP1 (M_DEREF, (_, VAR x)) -> loc, SET_VAL (x, _3)
	     | _ ->
		 raise (Parse_error (loc,
		     "Can only write assignments of the form x=e or *x=e.\n"))
	   )
# 635 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_011 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 146 "cparser.mly"
                        ( _sloc, BANG_CHR  )
# 646 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_012 =
  fun _1 ->
    (
# 154 "cparser.mly"
                           ( _1 )
# 654 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_013 =
  fun _1 ->
    (
# 339 "cparser.mly"
                          ( _1 )
# 662 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_014 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 74 "cparser.mly"
                              ( _sloc )
# 673 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_015 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 151 "cparser.mly"
                              ( _sloc )
# 684 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_016 =
  fun _1 _2 ->
    (
# 343 "cparser.mly"
        ( sup_locator _1 _2, CBLOCK ([], []) )
# 692 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_017 =
  fun _1 _2 _3 ->
    (
# 345 "cparser.mly"
 ( sup_locator _1 _3, CBLOCK ([], List.rev _2) )
# 700 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_018 =
  fun _1 _2 _3 ->
    (
# 347 "cparser.mly"
 ( sup_locator _1 _3, CBLOCK (_2, []) )
# 708 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_019 =
  fun _1 _2 _3 _4 ->
    (
# 349 "cparser.mly"
 ( sup_locator _1 _4, CBLOCK (_2, List.rev _3) )
# 716 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_020 =
  fun _1 ->
    (
# 254 "cparser.mly"
                                ( _1 )
# 724 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_021 =
  fun _1 _3 _5 ->
    (
# 256 "cparser.mly"
 ( 
	  sup_locator (loc_of_expr _1) (loc_of_expr _5),
	  EIF (_1, _3, _5)
	)
# 735 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_022 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 70 "cparser.mly"
                    ( _sloc, _1 )
# 746 "cparser.ml"
     : (Cast.location * int))

let _menhir_action_023 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 86 "cparser.mly"
                ( _sloc )
# 757 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_024 =
  fun _1 _2 ->
    (
# 288 "cparser.mly"
          ( let loc,var = _2 in [CDECL(loc,var,_1)] )
# 765 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_025 =
  fun _1 ->
    (
# 290 "cparser.mly"
          ( [_1] )
# 773 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_026 =
  fun _1 ->
    (
# 355 "cparser.mly"
          ( _1 )
# 781 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_027 =
  fun _1 _2 ->
    (
# 357 "cparser.mly"
          ( _1 @ _2 )
# 789 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_028 =
  fun _1 _4 _6 ->
    (
# 295 "cparser.mly"
        ( let loc,var = _4 in CDECL(loc,var,TFUN(_1,_6)) )
# 797 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_029 =
  fun _1 ->
    (
# 299 "cparser.mly"
                     ( _1 )
# 805 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_030 =
  fun _1 ->
    (
# 211 "cparser.mly"
                                ( _1 )
# 813 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_031 =
  fun _1 _3 ->
    (
# 213 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_EQ, _1, _3)
	)
# 823 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_032 =
  fun _1 _3 ->
    (
# 217 "cparser.mly"
 ( 
          let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF ((loc, CMP (C_EQ, _1, _3)),
		    (loc, CST 0),
		    (loc, CST 1))
	)
# 836 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_033 =
  fun _1 ->
    (
# 230 "cparser.mly"
                         ( _1 )
# 844 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_034 =
  fun _1 ->
    (
# 278 "cparser.mly"
                                ( _1 )
# 852 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_035 =
  fun _1 _3 ->
    (
# 280 "cparser.mly"
 ( 
	  sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  ESEQ [_1; _3]
	)
# 863 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_036 =
  fun _1 ->
    (
# 370 "cparser.mly"
            ( _1, ESEQ [] )
# 871 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_037 =
  fun _1 ->
    (
# 372 "cparser.mly"
            ( _1 )
# 879 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_038 =
  fun _1 ->
    (
# 440 "cparser.mly"
            ( [_1] )
# 887 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_039 =
  fun _1 ->
    (
# 442 "cparser.mly"
            ( _1 )
# 895 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_040 =
  fun () ->
    (
# 433 "cparser.mly"
          ( [] )
# 903 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_041 =
  fun _1 _2 ->
    (
# 435 "cparser.mly"
          ( _1 @ _2 )
# 911 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_042 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 396 "cparser.mly"
            ( _sloc )
# 922 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_043 =
  fun _1 _2 _3 ->
    (
# 469 "cparser.mly"
 ( _2, _3, _1 )
# 930 "cparser.ml"
     : ((Cast.location * string) * Cast.var_declaration list * Cast.ctyp))

let _menhir_action_044 =
  fun _1 _2 ->
    (
# 474 "cparser.mly"
 ( 
          let (loc, var), decls, rty = _1 in
	  CFUN (loc, var, decls, rty, _2)
	)
# 941 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_045 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 72 "cparser.mly"
                              ( _sloc, _1 )
# 952 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_046 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 377 "cparser.mly"
          ( _sloc )
# 963 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_047 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 85 "cparser.mly"
                ( _sloc )
# 974 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_048 =
  fun _1 ->
    (
# 234 "cparser.mly"
                                  ( _1 )
# 982 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_049 =
  fun _1 _3 _5 ->
    (
# 399 "cparser.mly"
    (
	    let loc = sup_locator _1 (fst _5) in
	    loc, CWHILE (_3, _5)
	   )
# 993 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_050 =
  fun _1 _3 _4 _6 ->
    (
# 405 "cparser.mly"
 ( 
          let loc = sup_locator _1 (fst _6) in
	  loc, CBLOCK ([], [(loc_of_expr _3, CEXPR _3);
			    loc, CWHILE (_4, _6)])
	)
# 1005 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_051 =
  fun _1 _3 _4 _5 _7 ->
    (
# 412 "cparser.mly"
 ( 
          let loc = sup_locator _1 (fst _7) in
	  loc, CBLOCK ([], [(loc_of_expr _3, CEXPR _3);
			    loc, CWHILE (_4,
					 (sup_locator (loc_of_expr _5) (loc_of_expr _7),
					  CBLOCK ([], [_7; (loc_of_expr _5,
							    CEXPR _5)])))])
	)
# 1020 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_052 =
  fun _1 ->
    (
# 426 "cparser.mly"
            ( _1, CRETURN None )
# 1028 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_053 =
  fun _1 _2 ->
    (
# 428 "cparser.mly"
            ( sup_locator _1 (loc_of_expr _2), CRETURN (Some _2) )
# 1036 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_054 =
  fun _1 ->
    (
# 238 "cparser.mly"
                                  ( _1 )
# 1044 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_055 =
  fun _1 _3 ->
    (
# 240 "cparser.mly"
 ( let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF (_1, _3, (loc, CST 0))
	)
# 1054 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_056 =
  fun _1 ->
    (
# 246 "cparser.mly"
                                 ( _1 )
# 1062 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_057 =
  fun _1 _3 ->
    (
# 248 "cparser.mly"
 ( let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF (_1, (loc, CST 1), _3)
	)
# 1072 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_058 =
  fun _1 ->
    (
# 158 "cparser.mly"
                          ( _1 )
# 1080 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_059 =
  fun _1 _3 ->
    (
# 160 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_MUL, _1, _3)
	)
# 1090 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_060 =
  fun _1 _3 ->
    (
# 164 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_DIV, _1, _3)
	)
# 1100 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_061 =
  fun _1 _3 ->
    (
# 168 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_MOD, _1, _3)
	)
# 1110 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_062 =
  fun _1 ->
    (
# 338 "cparser.mly"
                        ( _1 )
# 1118 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_063 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 73 "cparser.mly"
                              ( _sloc )
# 1129 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_action_064 =
  fun _1 _2 ->
    (
# 446 "cparser.mly"
          ( let _, vname = _2 in CDECL(getloc(), vname, _1) )
# 1137 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_065 =
  fun _1 ->
    (
# 448 "cparser.mly"
          ( _1 )
# 1145 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_066 =
  fun () ->
    (
# 464 "cparser.mly"
                                  ( [] )
# 1153 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_067 =
  fun _2 ->
    (
# 465 "cparser.mly"
                                                      ( _2 )
# 1161 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_068 =
  fun _1 ->
    (
# 454 "cparser.mly"
          ( [_1] )
# 1169 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_069 =
  fun _1 _3 ->
    (
# 456 "cparser.mly"
          ( _3 :: _1 )
# 1177 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_070 =
  fun () ->
    (
# 312 "cparser.mly"
                                         ( [] )
# 1185 "cparser.ml"
     : (Cast.ctyp list))

let _menhir_action_071 =
  fun _2 ->
    (
# 313 "cparser.mly"
                                                                  ( _2 )
# 1193 "cparser.ml"
     : (Cast.ctyp list))

let _menhir_action_072 =
  fun _1 ->
    (
# 317 "cparser.mly"
                              ( List.rev _1 )
# 1201 "cparser.ml"
     : (Cast.ctyp list))

let _menhir_action_073 =
  fun _1 ->
    (
# 460 "cparser.mly"
                         ( List.rev _1)
# 1209 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_074 =
  fun _1 ->
    (
# 89 "cparser.mly"
                             ( _1 )
# 1217 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_075 =
  fun _1 _3 ->
    (
# 91 "cparser.mly"
 ( let loc, var = _1 in
	  let loc1 = sup_locator loc _3 in
	    loc1, CALL (var, [])
	)
# 1228 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_076 =
  fun _1 _3 _4 ->
    (
# 96 "cparser.mly"
 ( let loc, var = _1 in
	  let loc1 = sup_locator loc _4 in
	    loc1, CALL (var, List.rev _3)
	)
# 1239 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_077 =
  fun _1 _2 ->
    (
# 101 "cparser.mly"
 ( sup_locator (loc_of_expr _1) _2, OP1 (M_POST_INC, _1) )
# 1247 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_078 =
  fun _1 _2 ->
    (
# 103 "cparser.mly"
 ( sup_locator (loc_of_expr _1) _2, OP1 (M_POST_DEC, _1) )
# 1255 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_079 =
  fun _1 ->
    (
# 64 "cparser.mly"
                     ( let loc, var = _1 in loc, VAR var )
# 1263 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_080 =
  fun _1 ->
    (
# 65 "cparser.mly"
                   ( let loc, cst = _1 in loc, CST cst )
# 1271 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_081 =
  fun _1 ->
    (
# 66 "cparser.mly"
                         ( let loc, s = _1 in loc, STRING s )
# 1279 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_082 =
  fun _2 ->
    (
# 67 "cparser.mly"
                                                    ( _2 )
# 1287 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_083 =
  fun _1 ->
    (
# 191 "cparser.mly"
                           ( _1 )
# 1295 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_084 =
  fun _1 _3 ->
    (
# 193 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LT, _1, _3)
	)
# 1305 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_085 =
  fun _1 _3 ->
    (
# 197 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LT, _3, _1)
	)
# 1315 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_086 =
  fun _1 _3 ->
    (
# 201 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LE, _1, _3)
	)
# 1325 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_087 =
  fun _1 _3 ->
    (
# 205 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LE, _3, _1)
	)
# 1335 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_088 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 422 "cparser.mly"
                ( _sloc )
# 1346 "cparser.ml"
     : (Cast.location))

let _menhir_action_089 =
  fun () ->
    (
# 303 "cparser.mly"
                  ( TINT )
# 1354 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_090 =
  fun _1 ->
    (
# 304 "cparser.mly"
                           ( TPTR(_1) )
# 1362 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_091 =
  fun _1 _3 _5 _endpos__5_ _startpos__1_ ->
    let _endpos = _endpos__5_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 381 "cparser.mly"
 ( 
          sup_locator _1 (fst _5), CIF (_3, _5,
					(_sloc, CBLOCK ([], [])))
	)
# 1376 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_092 =
  fun _1 _3 _5 _7 ->
    (
# 386 "cparser.mly"
 ( 
          sup_locator _1 (fst _7), CIF (_3, _5, _7)
	)
# 1386 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_093 =
  fun _1 _3 _5 _7 ->
    (
# 390 "cparser.mly"
 ( 
          sup_locator _1 (fst _7), CIF (_3, _5, _7)
	)
# 1396 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_094 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 375 "cparser.mly"
                    ( _sloc )
# 1407 "cparser.ml"
     : (Cast.location))

let _menhir_action_095 =
  fun _1 ->
    (
# 187 "cparser.mly"
                              ( _1 )
# 1415 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_096 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 148 "cparser.mly"
                        ( _sloc, STAR_CHR  )
# 1426 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_097 =
  fun _1 ->
    (
# 327 "cparser.mly"
            ( _1 )
# 1434 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_098 =
  fun _1 ->
    (
# 329 "cparser.mly"
            ( loc_of_expr _1, CEXPR _1 )
# 1442 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_099 =
  fun _1 ->
    (
# 331 "cparser.mly"
            ( _1 )
# 1450 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_100 =
  fun _1 ->
    (
# 333 "cparser.mly"
            ( _1 )
# 1458 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_101 =
  fun _1 ->
    (
# 335 "cparser.mly"
            ( _1 )
# 1466 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_102 =
  fun _1 ->
    (
# 363 "cparser.mly"
          ( [_1] )
# 1474 "cparser.ml"
     : (Cast.loc_code list))

let _menhir_action_103 =
  fun _1 _2 ->
    (
# 365 "cparser.mly"
          ( _2 :: _1 )
# 1482 "cparser.ml"
     : (Cast.loc_code list))

let _menhir_action_104 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 77 "cparser.mly"
                         ( _sloc, _1 )
# 1493 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_105 =
  fun _1 _2 _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 79 "cparser.mly"
            ( 
              let l, s = _2 in
              let s2 = _1 in
              (_sloc, s2^s)
            )
# 1508 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_106 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 145 "cparser.mly"
                        ( _sloc, SUB_CHR   )
# 1519 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_107 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 147 "cparser.mly"
                        ( _sloc, TILDE_CHR )
# 1530 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_108 =
  fun _1 ->
    (
# 308 "cparser.mly"
            ( _1 )
# 1538 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_109 =
  fun _1 ->
    (
# 321 "cparser.mly"
                         ( [_1] )
# 1546 "cparser.ml"
     : (Cast.ctyp list))

let _menhir_action_110 =
  fun _1 _3 ->
    (
# 323 "cparser.mly"
            ( _3 :: _1 )
# 1554 "cparser.ml"
     : (Cast.ctyp list))

let _menhir_action_111 =
  fun _1 ->
    (
# 115 "cparser.mly"
                             ( _1 )
# 1562 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_112 =
  fun _1 _2 ->
    (
# 117 "cparser.mly"
 ( sup_locator _1 (loc_of_expr _2), OP1 (M_PRE_INC, _2) )
# 1570 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_113 =
  fun _1 _2 ->
    (
# 119 "cparser.mly"
 ( sup_locator _1 (loc_of_expr _2), OP1 (M_PRE_DEC, _2) )
# 1578 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_114 =
  fun _1 _2 ->
    (
# 121 "cparser.mly"
 ( 
          let loc, c = _1 in
          let loc' = sup_locator loc (loc_of_expr _2) in
	  match c with
	      ADD_CHR -> _2
	    | SUB_CHR -> loc', OP1 (M_MINUS, _2)
	    | BANG_CHR -> loc', EIF (_2, (loc', CST 0), (loc', CST 1))
            | TILDE_CHR -> loc', OP1 (M_NOT, _2)
            | STAR_CHR -> loc', OP1 (M_DEREF, _2)
            | AMP_CHR -> loc', OP1 (M_ADDR, _2)
	    | _ -> raise (Parse_error (loc, "unknown unary operator"))
	)
# 1597 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_115 =
  fun _1 ->
    (
# 136 "cparser.mly"
                    ( _1 )
# 1605 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_116 =
  fun _1 ->
    (
# 137 "cparser.mly"
                    ( _1 )
# 1613 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_117 =
  fun _1 ->
    (
# 138 "cparser.mly"
                    ( _1 )
# 1621 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_118 =
  fun _1 ->
    (
# 139 "cparser.mly"
                    ( _1 )
# 1629 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_119 =
  fun _1 ->
    (
# 140 "cparser.mly"
                    ( _1 )
# 1637 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_120 =
  fun _1 ->
    (
# 141 "cparser.mly"
                    ( _1 )
# 1645 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_121 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 395 "cparser.mly"
                ( _sloc )
# 1656 "cparser.ml"
     : (Lexing.position * Lexing.position))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD_CHR ->
        "ADD_CHR"
    | AMP_CHR ->
        "AMP_CHR"
    | AND_OP ->
        "AND_OP"
    | BANG_CHR ->
        "BANG_CHR"
    | CLOSE_ANGLE_CHR ->
        "CLOSE_ANGLE_CHR"
    | CLOSE_BRACE_CHR ->
        "CLOSE_BRACE_CHR"
    | CLOSE_PAREN_CHR ->
        "CLOSE_PAREN_CHR"
    | COLON_CHR ->
        "COLON_CHR"
    | COMMA_CHR ->
        "COMMA_CHR"
    | CONSTANT _ ->
        "CONSTANT"
    | DEC_OP ->
        "DEC_OP"
    | DIV_CHR ->
        "DIV_CHR"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ_CHR ->
        "EQ_CHR"
    | EQ_OP ->
        "EQ_OP"
    | FOR ->
        "FOR"
    | GE_OP ->
        "GE_OP"
    | IDENTIFIER _ ->
        "IDENTIFIER"
    | IF ->
        "IF"
    | INC_OP ->
        "INC_OP"
    | INTEGER ->
        "INTEGER"
    | LE_OP ->
        "LE_OP"
    | MOD_CHR ->
        "MOD_CHR"
    | NE_OP ->
        "NE_OP"
    | OPEN_ANGLE_CHR ->
        "OPEN_ANGLE_CHR"
    | OPEN_BRACE_CHR ->
        "OPEN_BRACE_CHR"
    | OPEN_PAREN_CHR ->
        "OPEN_PAREN_CHR"
    | OR_OP ->
        "OR_OP"
    | QUES_CHR ->
        "QUES_CHR"
    | RETURN ->
        "RETURN"
    | SEMI_CHR ->
        "SEMI_CHR"
    | STAR_CHR ->
        "STAR_CHR"
    | STRING_LITERAL _ ->
        "STRING_LITERAL"
    | SUB_CHR ->
        "SUB_CHR"
    | TILDE_CHR ->
        "TILDE_CHR"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_188 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      MenhirBox_file _v
  
  let rec _menhir_goto_file : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState189 ->
          _menhir_run_190 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_188 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_190 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_external_declaration -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let MenhirCell1_external_declaration (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_041 _1 _2 in
      _menhir_goto_file _menhir_stack _v _menhir_s
  
  let _menhir_run_002 : type  ttv_stack. ttv_stack -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_s ->
      let _v = _menhir_action_040 () in
      _menhir_goto_file _menhir_stack _v _menhir_s
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_089 () in
      _menhir_goto_scalar_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_scalar_specifier : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_108 _1 in
      _menhir_goto_type_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_specifier : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState189 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_172 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPEN_PAREN_CHR ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState172
      | IDENTIFIER _v_0 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState172
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _v = _menhir_action_090 _1 in
      _menhir_goto_scalar_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_005 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_STAR_CHR (_menhir_stack, _startpos, _endpos) in
          let _menhir_s = MenhirState006 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_045 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_identifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_identifier : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState172 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_068 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState069 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CLOSE_PAREN_CHR ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DEC_OP | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | INC_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_079 _1 in
          _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_107 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_118 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_operator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_unary_operator (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState056
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState056
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState056
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_106 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_116 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_044 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING_LITERAL _v_0 ->
          let _menhir_stack = MenhirCell1_STRING_LITERAL (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState044
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DEC_OP | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | INC_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_104 _1 _endpos__1_ _startpos__1_ in
          _menhir_goto_string_literal _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_string_literal : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState181 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_060 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_081 _1 in
      _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primary_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_074 _1 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_postfix_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | INC_OP ->
          let _menhir_stack = MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_047 _endpos__1_ _startpos__1_ in
      _menhir_goto_inc_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_inc_op : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_066 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_inc_op (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState066
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState066
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState066
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_096 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_119 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_049 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState049 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_023 _endpos__1_ _startpos__1_ in
      _menhir_goto_dec_op _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dec_op : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState040 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_079 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_dec_op (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState079
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState079
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState079
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_022 _1 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_080 _1 in
      _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_011 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_117 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_005 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_120 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_055 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_001 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_115 _1 in
      _menhir_goto_unary_operator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_postfix_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_078 _1 _2 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_064 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_postfix_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_postfix_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_077 _1 _2 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState181 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState122 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_080 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_dec_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_dec_op (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_113 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ_CHR ->
          let _menhir_stack = MenhirCell1_unary_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState072 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DIV_CHR | EQ_OP | GE_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_012 _1 in
          _menhir_goto_cast_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_cast_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState056 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState040 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState078 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_131 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_operator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_operator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_114 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_090 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_058 _1 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_multiplicative_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState094 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_095 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression _menhir_cell0_ADD_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let MenhirCell0_ADD_CHR (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_003 _1 _3 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_STAR_CHR (_menhir_stack, _startpos, _endpos) in
      let _menhir_s = MenhirState078 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState088 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_additive_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _v) in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_SUB_CHR (_menhir_stack, _startpos, _endpos) in
          let _menhir_s = MenhirState092 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _v) in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_ADD_CHR (_menhir_stack, _startpos, _endpos) in
          let _menhir_s = MenhirState094 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_095 _1 in
          _menhir_goto_shift_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_shift_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState100 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState096 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState180 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_101 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_085 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_relational_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_031 _1 _3 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState075 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState096 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState098 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_100 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState100 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_equality_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NE_OP ->
          let _menhir_stack = MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState109 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EQ_OP ->
          let _menhir_stack = MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState111 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_006 _1 in
          let _1 = _v in
          let _v = _menhir_action_033 _1 in
          let _1 = _v in
          let _v = _menhir_action_048 _1 in
          _menhir_goto_inclusive_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_inclusive_or_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_114 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_054 _1 in
      _menhir_goto_logical_and_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_logical_and_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_123 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND_OP ->
          let _menhir_stack = MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_057 _1 _3 in
          _menhir_goto_logical_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_105 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState105 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_logical_or_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUES_CHR ->
          let _menhir_stack = MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState103 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | OR_OP ->
          let _menhir_stack = MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState122 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_020 _1 in
          _menhir_goto_conditional_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_conditional_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState119 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState040 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_120 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COLON_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_021 _1 _3 _5 in
      _menhir_goto_conditional_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_117 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_goto_assignment_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_assignment_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState128 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState069 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState040 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_129 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_argument_expression_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_008 _1 _3 in
      _menhir_goto_argument_expression_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_argument_expression_list : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_argument_expression_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_COMMA_CHR (_menhir_stack, MenhirState127) in
          let _menhir_s = MenhirState128 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState127
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_015 _endpos__1_ _startpos__1_ in
      _menhir_goto_close_paren _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_close_paren : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState158 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState159 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState127 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState069 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_166 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState166
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState166
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState166
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_121 _endpos__1_ _startpos__1_ in
      let _menhir_stack = MenhirCell1_whilekw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_s = MenhirState138 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_094 _endpos__1_ _startpos__1_ in
      let _endpos = _endpos__1_ in
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_036 _1 in
      _menhir_goto_expression_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_expression_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_162 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_098 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState173 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState166 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState160 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState153 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState140 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_177 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_102 _1 in
      _menhir_goto_statement_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_statement_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_181 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState181
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState181
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState181
      | CLOSE_BRACE_CHR ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_088 _endpos__1_ _startpos__1_ in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | STRING_LITERAL _v_0 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState144
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | SEMI_CHR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _endpos__2_) = (_v, _endpos) in
          let _v = _menhir_action_052 _1 in
          _menhir_goto_jump_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _menhir_s _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | INC_OP ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | IDENTIFIER _v_1 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState144
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | CONSTANT _v_2 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState144
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | _ ->
          _eRR ()
  
  and _menhir_goto_jump_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_101 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_063 _endpos__1_ _startpos__1_ in
      let _1 = _v in
      let _v = _menhir_action_062 _1 in
      let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState040
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState040
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState040
      | CLOSE_BRACE_CHR ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_046 _endpos__1_ _startpos__1_ in
      let _startpos = _startpos__1_ in
      let _menhir_stack = MenhirCell1_ifkw (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_s = MenhirState151 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_042 _endpos__1_ _startpos__1_ in
      let _menhir_stack = MenhirCell1_forkw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_s = MenhirState156 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI_CHR ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_136 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_014 _endpos__1_ _startpos__1_ in
      let _endpos = _endpos__1_ in
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_013 _1 in
      _menhir_goto_close_block _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_close_block : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState040 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState180 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState181 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState173 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_186 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_016 _1 _2 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _menhir_s _tok
  
  and _menhir_goto_compound_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState037 ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState169 ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState153 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_187 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_function_declarator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_function_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_044 _1 _2 in
      let _1 = _v in
      let _v = _menhir_action_038 _1 in
      _menhir_goto_external_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_external_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_external_declaration (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState189
      | EOF ->
          _menhir_run_002 _menhir_stack MenhirState189
      | _ ->
          _eRR ()
  
  and _menhir_run_171 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_compound_statement (_menhir_stack, _, _5, _) = _menhir_stack in
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1, _) = _menhir_stack in
      let (_endpos__7_, _7) = (_endpos, _v) in
      let _v = _menhir_action_093 _1 _3 _5 _7 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__7_ _v _menhir_s _tok
  
  and _menhir_goto_selection_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState169 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState180 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_170 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_compound_statement (_menhir_stack, _, _5, _) = _menhir_stack in
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1, _) = _menhir_stack in
      let (_endpos__7_, _7) = (_endpos, _v) in
      let _v = _menhir_action_092 _1 _3 _5 _7 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__7_ _v _menhir_s _tok
  
  and _menhir_run_143 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_099 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_168 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_compound_statement (_menhir_stack, _menhir_s, _v, _endpos) in
          let _menhir_s = MenhirState169 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OPEN_BRACE_CHR ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_CHR | AMP_CHR | BANG_CHR | CLOSE_BRACE_CHR | CONSTANT _ | DEC_OP | FOR | IDENTIFIER _ | IF | INC_OP | OPEN_BRACE_CHR | OPEN_PAREN_CHR | RETURN | SEMI_CHR | STAR_CHR | STRING_LITERAL _ | SUB_CHR | TILDE_CHR | WHILE ->
          let (_endpos__1_, _1) = (_endpos, _v) in
          let _v = _menhir_action_097 _1 in
          _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_165 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_097 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_184 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_declaration_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__3_, _3) = (_endpos, _v) in
      let _v = _menhir_action_018 _1 _2 _3 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  and _menhir_run_182 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_declaration_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__4_, _4) = (_endpos, _v) in
      let _v = _menhir_action_019 _1 _2 _3 _4 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _v _menhir_s _tok
  
  and _menhir_run_176 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__3_, _3) = (_endpos, _v) in
      let _v = _menhir_action_017 _1 _2 _3 in
      _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  and _menhir_run_173 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState173
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState173
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState173
      | CLOSE_BRACE_CHR ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState173
      | _ ->
          _eRR ()
  
  and _menhir_run_174 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_103 _1 _2 in
      _menhir_goto_statement_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_167 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _4, _) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _3, _) = _menhir_stack in
      let MenhirCell1_forkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__6_, _6) = (_endpos, _v) in
      let _v = _menhir_action_050 _1 _3 _4 _6 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__6_ _v _menhir_s _tok
  
  and _menhir_goto_iteration_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos__1_, _1) = (_endpos, _v) in
      let _v = _menhir_action_100 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_run_161 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _4, _) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _3, _) = _menhir_stack in
      let MenhirCell1_forkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__7_, _7) = (_endpos, _v) in
      let _v = _menhir_action_051 _1 _3 _4 _5 _7 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__7_ _v _menhir_s _tok
  
  and _menhir_run_154 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
      let (_endpos__5_, _5) = (_endpos, _v) in
      let _v = _menhir_action_091 _1 _3 _5 _endpos__5_ _startpos__1_ in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _v _menhir_s _tok
  
  and _menhir_run_141 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_whilekw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let (_endpos__5_, _5) = (_endpos, _v) in
      let _v = _menhir_action_049 _1 _3 _5 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _v _menhir_s _tok
  
  and _menhir_run_158 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression_statement (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState158
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState158
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState158
      | CLOSE_PAREN_CHR ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression_statement (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState157
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState157
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState157
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState160
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState160
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState160
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | _ ->
          _eRR ()
  
  and _menhir_run_140 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState140
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState140
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState140
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | _ ->
          _eRR ()
  
  and _menhir_run_130 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_argument_expression_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_identifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_076 _1 _3 _4 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_125 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_identifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_075 _1 _3 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_126 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_007 _1 in
      _menhir_goto_argument_expression_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_124 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_010 _1 _3 in
      _menhir_goto_assignment_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_121 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_034 _1 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_163 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _endpos__2_) = (_v, _endpos) in
          let _v = _menhir_action_037 _1 in
          _menhir_goto_expression_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _menhir_s _tok
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COMMA_CHR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState116 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_LITERAL _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENTIFIER _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONSTANT _v ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | CLOSE_PAREN_CHR ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState159
      | _ ->
          _eRR ()
  
  and _menhir_run_152 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState152
      | CLOSE_PAREN_CHR ->
          let _menhir_s = MenhirState152 in
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _menhir_s, _startpos, _endpos) in
          let _menhir_s = MenhirState153 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI_CHR ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_BRACE_CHR ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_146 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_return as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_return (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let (_2, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_053 _1 _2 in
          _menhir_goto_jump_statement _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | CLOSE_PAREN_CHR ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState139
      | _ ->
          _eRR ()
  
  and _menhir_run_132 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState132
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_082 _2 in
          _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_115 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState115
      | COLON_CHR ->
          let _menhir_stack = MenhirCell1_COLON_CHR (_menhir_stack, MenhirState115) in
          let _menhir_s = MenhirState119 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SUB_CHR ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_LITERAL _v ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR_CHR ->
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OPEN_PAREN_CHR ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INC_OP ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENTIFIER _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC_OP ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT _v ->
              _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG_CHR ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AMP_CHR ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ADD_CHR ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_118 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_035 _1 _3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_104 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND_OP ->
          let _menhir_stack = MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_056 _1 in
          _menhir_goto_logical_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_106 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_055 _1 _3 in
      _menhir_goto_logical_and_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_110 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_032 _1 _3 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_030 _1 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_087 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_097 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_086 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_076 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_084 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_083 _1 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression _menhir_cell0_SUB_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let MenhirCell0_SUB_CHR (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_004 _1 _3 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_002 _1 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_060 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_087 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_061 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression _menhir_cell0_STAR_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_STAR_CHR (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_059 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_067 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_inc_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_inc_op (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_112 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_057 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_goto_cast_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_045 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_STRING_LITERAL -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_STRING_LITERAL (_menhir_stack, _menhir_s, _1, _startpos__1_, _) = _menhir_stack in
      let (_endpos__2_, _2) = (_endpos, _v) in
      let _v = _menhir_action_105 _1 _2 _endpos__2_ _startpos__1_ in
      _menhir_goto_string_literal _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _menhir_s _tok
  
  and _menhir_run_024 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_029 _1 in
      _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declarator : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState172 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState003 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState023 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_024 _1 _2 in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState189 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_191 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_039 _1 in
      _menhir_goto_external_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_185 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_026 _1 in
      _menhir_goto_declaration_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declaration_list : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | TILDE_CHR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | SUB_CHR ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | STRING_LITERAL _v_0 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState180
      | STAR_CHR ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | SEMI_CHR ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | RETURN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | OPEN_PAREN_CHR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | INC_OP ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | IF ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | IDENTIFIER _v_1 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState180
      | FOR ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | DEC_OP ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | CONSTANT _v_2 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState180
      | CLOSE_BRACE_CHR ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | BANG_CHR ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | AMP_CHR ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | ADD_CHR ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | _ ->
          _eRR ()
  
  and _menhir_run_183 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_027 _1 _2 in
      _menhir_goto_declaration_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_064 _1 _2 in
      _menhir_goto_parameter_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState021 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_032 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_goto_parameter_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_list : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState029 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INTEGER ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          let _1 = _v in
          let _v = _menhir_action_073 _1 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_067 _2 in
          _menhir_goto_parameter_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_parameter_declarator : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_identifier (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_043 _1 _2 _3 in
      let _menhir_stack = MenhirCell1_function_declarator (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_BRACE_CHR ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_parameter_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_069 _1 _3 in
      _menhir_goto_parameter_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_020 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INTEGER ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState021
          | CLOSE_PAREN_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_066 () in
              _menhir_goto_parameter_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_029 _1 in
          _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | CLOSE_PAREN_CHR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_CLOSE_PAREN_CHR (_menhir_stack, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OPEN_PAREN_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INTEGER ->
                  _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
              | CLOSE_PAREN_CHR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_070 () in
                  _menhir_goto_parameter_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_parameter_specifier : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_CLOSE_PAREN_CHR (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_identifier (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_STAR_CHR (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_028 _1 _4 _6 in
      _menhir_goto_declaration_ptr_fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_declaration_ptr_fun : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState189 ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_178 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_025 _1 in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_065 _1 in
      _menhir_goto_parameter_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPEN_PAREN_CHR ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | IDENTIFIER _v_0 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState023
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COMMA_CHR ->
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_goto_type_specifier_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_specifier_list : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_type_specifier_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState013 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INTEGER ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          let _1 = _v in
          let _v = _menhir_action_072 _1 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_071 _2 in
          _menhir_goto_parameter_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_014 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR _menhir_cell0_STAR_CHR, _menhir_box_file) _menhir_cell1_identifier _menhir_cell0_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_type_specifier_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COMMA_CHR ->
          let MenhirCell1_type_specifier_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_110 _1 _3 in
          _menhir_goto_type_specifier_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | OPEN_PAREN_CHR ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState003
      | IDENTIFIER _v_0 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState000 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INTEGER ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EOF ->
          _menhir_run_002 _menhir_stack _menhir_s
      | _ ->
          _eRR ()
  
end

let file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_file v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 481 "cparser.mly"
  
# 5413 "cparser.ml"
