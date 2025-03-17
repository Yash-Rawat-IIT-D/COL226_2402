(* main_parser.ml - Test driver for the parser *)

open My_ast
open My_lexer
open My_parser

(* Function to parse input from a lexing buffer *)
let parse_from_lexbuf lexbuf =
  try
    (* Adjust this to match your parser's entry point rule name *)
    let ast = My_parser.program My_lexer.token lexbuf in
    (* Process the resulting AST *)
    print_endline "Parsing successful!";
    (* Here you can add code to print or process the AST *)
    ast
  with
  | My_lexer.Lexical_error(msg, line, col) ->
      Printf.fprintf stderr "Lexical error at line %d, column %d: %s\n" line col msg;
      exit 1
  | My_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.fprintf stderr "Syntax error at line %d, column %d\n" 
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  | e ->
      Printf.fprintf stderr "Unexpected error: %s\n" (Printexc.to_string e);
      exit 2

(* Function to parse from a file *)
let parse_file filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  (* Set the filename for accurate error reporting *)
  Lexing.set_filename lexbuf filename;
  (* Initialize position tracking *)
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  let result = parse_from_lexbuf lexbuf in
  close_in channel;
  result

(* Main entry point *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.fprintf stderr "Usage: %s <input file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    ignore (parse_file filename)
