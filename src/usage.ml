open Format
open Lexing
open Clexer

let usage = "usage: ./ccomp [options] file.c"
let debug = ref false
let no_pretty = ref false
let parse_only = ref false
let type_only = ref false
let analyse_only = ref false
let cfg_file = ref None

let spec =
  [
    ("--debug", Arg.Set debug, "\truns in debug mode");
    ("--no-pretty", Arg.Set no_pretty, "\tdisplays trees in the command line");
    ("--parse-only", Arg.Set parse_only, "\tstops after parsing");
    ("--type-only", Arg.Set type_only, "\tstops after typing");
    ("--analyse-only", Arg.Set analyse_only, "\tstops after analysis");
    ("--convert-cfg",
      Arg.String (fun s -> cfg_file := Some s),
      "\tconverts to cfg and dump on filename" );
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None ->
      Arg.usage spec usage;
      exit 1

let debug = !debug

let warning (b, e) msg =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol in
  let lc = e.pos_cnum - b.pos_bol in
  printf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc;
  printf "warning: %s\n@." msg

let report_loc (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol in
  let lc = e.pos_cnum - b.pos_bol in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc