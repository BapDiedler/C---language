{

(*
 *	Copyright (C) 2023 by Laboratoire Méthodes Formelles (LMF),
 *	UMR 9021 Université Paris-Saclay, CNRS et ENS Paris-Saclay.
 *	Modified by Mihaela Sighireanu.
 *
 *	Copyright (C) 2005, 2006 by Laboratoire Spécification et Vérification (LSV),
 *	UMR 8643 CNRS & ENS Cachan.
 *	Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. Neither the author nor its employer is responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 *	4. This software is restricted to non-commercial use only.  Commercial
 *		use is subject to a specific license, obtainable from LMF.
 * 
*)

(* Analyse lexicale d'un sous-ensemble (tres) reduit de C.
 *)

open Lexing
open Cast
open Cparser

exception Lexing_error of string

let string_buffer = Buffer.create 256

let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
let newlines s lexbuf =
    String.iter (fun c -> if c = '\n' then newline lexbuf) s

let parse_hex yytext tend =
	let n = ref 0
	in let len = String.length yytext-tend
	in ((for i=2 to len-1 do
	     let c = yytext.[i] in
	     match c with
	         '0'..'9' -> n := 16 * !n + (int_of_char c - int_of_char '0')
               | 'a'..'f' -> n := 16 * !n + (int_of_char c + 10 - int_of_char 'a')
               | 'A'..'F' -> n := 16 * !n + (int_of_char c + 10 - int_of_char 'A')
	       | _ -> raise (Lexing_error ("invalid hexadecimal number " ^ yytext))
	     done);
	    !n)

let parse_oct yytext start tend =
	let n = ref 0
	in let len = String.length yytext-tend
	in ((for i=start to len-1 do
	     let c = yytext.[i] in
	     match c with
	         '0'..'7' -> n := 8 * !n + (int_of_char c - int_of_char '0')
	       | _ -> raise (Lexing_error ("invalid octal number " ^ yytext))
	     done);
	    !n)

let parse_dec yytext tend =
	let n = ref 0
	in let len = String.length yytext-tend
	in ((for i=0 to len-1 do
	     let c = yytext.[i] in
	     match c with
	         '0'..'9' -> n := 10 * !n + (int_of_char c - int_of_char '0')
	       | _ -> raise (Lexing_error ("invalid number " ^ yytext))
	    done);
	    !n)

}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let hex    = ['a'-'f' 'A'-'F' '0'-'9']
let expo   = ['E' 'e'] ['+' '-']? digit+
let fs     = ['f' 'F' 'l' 'L']
let is     = ['u' 'U' 'l' 'L']*
let space  = ' ' | '\t'

rule ctoken = parse
  | (space* '\n')+ as s { newlines s lexbuf; ctoken lexbuf }
  | space+ { ctoken lexbuf }
  | "/*" { comment lexbuf; ctoken lexbuf }
  | "//" [^'\n']* '\n'  { newline lexbuf; ctoken lexbuf }
  | "else" { ELSE }
  | "for" { FOR }
  | "if" { IF }
  | "int" { INTEGER }
  | "return" { RETURN }
  | "while" { WHILE }
  | letter (letter | digit)* as s { IDENTIFIER s } 
  | '0' ['x' 'X'] hex+ { CONSTANT (parse_hex (Lexing.lexeme lexbuf) 0) }

  | '0' ['0'-'7']+ { CONSTANT (parse_oct (Lexing.lexeme lexbuf) 1 0) }

  | digit+ { CONSTANT (parse_dec (Lexing.lexeme lexbuf) 0) }

  | '\'' [^ '\'' '\\'] '\'' { CONSTANT (int_of_char (Lexing.lexeme_char lexbuf 1)) }
  | '\'' '\\' ['0'-'7'] ['0'-'7']? ['0'-'7']? '\'' { 
                        CONSTANT (parse_oct (Lexing.lexeme lexbuf) 2 1) }
  | '\'' '\\' 'a' '\'' { CONSTANT 7 (* bell, ^G *) }
  | '\'' '\\' 'b' '\'' { CONSTANT (int_of_char '\b') }
  | '\'' '\\' 'f' '\'' { CONSTANT 12 (* form feed, ^L *) }
  | '\'' '\\' 'n' '\'' { CONSTANT (int_of_char '\n') }
  | '\'' '\\' 'r' '\'' { CONSTANT (int_of_char '\r') }
  | '\'' '\\' 't' '\'' { CONSTANT (int_of_char '\t')
				 (* bell, ^G *) }
  | '\'' '\\' 'v' '\'' { CONSTANT 11 (* vertical tab, ^K *) }
  | '\'' '\\' _ '\'' { CONSTANT (int_of_char (Lexing.lexeme_char lexbuf 2)) }
  | "++"  { INC_OP }
  | "--"  { DEC_OP }
  | "&&"  { AND_OP }
  | "||"  { OR_OP }
  | "<="  { LE_OP }
  | ">="  { GE_OP }
  | "=="  { EQ_OP }
  | "!="  { NE_OP }
  | ";"   { SEMI_CHR }
  | ("{" | "<%") { OPEN_BRACE_CHR }
  | ("}" | "%>") { CLOSE_BRACE_CHR }
  | "," { COMMA_CHR }
  | ":" { COLON_CHR }
  | "=" { EQ_CHR }
  | "(" { OPEN_PAREN_CHR }
  | ")" { CLOSE_PAREN_CHR }
  | "!" { BANG_CHR }
  | "~" { TILDE_CHR }
  | "+" { ADD_CHR }
  | "-" { SUB_CHR }
  | "*" { STAR_CHR }
  | "&" { AMP_CHR }
  | "/" { DIV_CHR }
  | "%" { MOD_CHR }
  | "<" { OPEN_ANGLE_CHR }
  | ">" { CLOSE_ANGLE_CHR }
  | "?" { QUES_CHR }
  | '#' { line lexbuf }
  | '"' { STRING_LITERAL (string lexbuf) }
  | eof { EOF }
  | _ as c
      { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
    "*/" { () }
  | "\n" { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexing_error "unterminated comment") }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
        Buffer.reset string_buffer;
        s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
        string lexbuf }
  | '\\' ['0'-'7'] ['0'-'7']? ['0'-'7']? 
      { Buffer.add_char string_buffer (Char.chr (parse_oct (Lexing.lexeme lexbuf) 1 0)); 
        string lexbuf }
  | '\\' 'a' { Buffer.add_char string_buffer '\007'; string lexbuf }
  | '\\' 'b' { Buffer.add_char string_buffer '\b'; string lexbuf }
  | '\\' 'f' { Buffer.add_char string_buffer '\014'; string lexbuf }
  | '\\' 'n' { Buffer.add_char string_buffer '\n'; string lexbuf }
  | '\\' 'r' { Buffer.add_char string_buffer '\r'; string lexbuf }
  | '\\' 't' { Buffer.add_char string_buffer '\t'; string lexbuf }
  | '\\' 'v' { Buffer.add_char string_buffer '\013'; string lexbuf }
  | '\\' _   { Buffer.add_char string_buffer (Lexing.lexeme_char lexbuf 1); string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
        string lexbuf }
  | eof { raise (Lexing_error ("end of file reached inside string literal")) }

and line = parse
    ['0'-'9']+ { cline := parse_dec (Lexing.lexeme lexbuf) 0 - 1; line2 lexbuf }
  | space+ { line lexbuf }
  | '\n' { newline lexbuf; ctoken lexbuf }
  | "\"" { cfile := (string lexbuf); ctoken lexbuf }
  | eof { raise (Lexing_error ("end of file reached inside # directive")) }
and line2 = parse
    space+ { line2 lexbuf }
  | '\n' { newline lexbuf; ctoken lexbuf }
  | "\"" { cfile := (string lexbuf); line3 lexbuf }
  | eof { raise (Lexing_error ("end of file reached inside # directive")) }
and line3 = parse
    '\n' { newline lexbuf; ctoken lexbuf }
  |  _ { line3 lexbuf }
  | eof  { raise (Lexing_error ("end of file reached inside # directive")) }
