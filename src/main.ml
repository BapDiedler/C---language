(* Main program
 *
 * Calls all steps of the compiler.
*)

(* Main program
 *
 * Calls all steps of the compiler.
 *)

 open Format
 open Lexing
 open Clexer
 open Cparser
 open Usage
 open Cfg   

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Cparser.file Clexer.ctoken lb in
    close_in c;

    if Usage.debug then begin
      let ast_dot_file = open_out (Filename.chop_suffix file ".c" ^ "_ast.dot") in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_ast f (not !no_pretty));
      close_out ast_dot_file
    end;

    eprintf "Lexing and parsing: done!\n@.";
    if !Usage.parse_only then exit 0;

    (* TO WRITE *)
    eprintf "Analysis: done!\n@.";
    let _ = Ctyping.check_file f in
    (* Ctyping.print_typ_list (Ctyping.check_file f); *)

    (* TO WRITE *)
    eprintf "Synthesis: not yet implemented!\n@.";

    (*WHAT YOU DID ON PREVIOUS PART*)
    let program = f in
    match !Usage.cfg_file with
    | None -> ()
    | Some filename ->
        Ast2cfg.transform_program program |> Cfg.dump_dot filename;

        exit 2

  with
    | Clexer.Lexing_error s ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "Lexical error: %s\n@." s;
      exit 1
    | Cparser.Error ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "Syntax error\n@.";
      exit 2

    | Ctyping.Error (l, msg) ->
      report_loc l;
      eprintf "Typing error: %s\n@." msg;
      exit 3

    | Ctyping.Anomaly msg ->
      eprintf "Typing Anomaly: %s\n@." msg;
      exit 3
(*    | Compile.Anomaly msg ->
      eprintf "Compile anomaly: %s\n@." msg;
      exit 2
*)
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
