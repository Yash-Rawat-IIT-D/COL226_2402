(* main_parser.ml - Test driver for the parser *)

open My_ast
open My_lexer
open My_parser
open Prog

(* Function to parse from a file *)
let parse_file filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  (* Set filename for accurate error reporting *)
  Lexing.set_filename lexbuf filename;
  try
    (* Parse the program - this is where the lexer and parser work together *)
    let ast = My_parser.program My_lexer.token lexbuf in
    close_in channel;
    
    (* Print the parsed program using your string_of_ functions *)
    print_endline "Parsing successful!";
    print_endline "Parsed program:";
    print_endline (string_of_program ast);
    
    ast
  with
  | My_lexer.Illogical_Lex msg ->
      Printf.fprintf stderr "Lexical error: %s\n" msg;
      close_in channel;
      exit 1
  | e ->
      Printf.fprintf stderr "Unexpected error: %s\n" (Printexc.to_string e);
      close_in channel;
      exit 2

(* Main entry point *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.fprintf stderr "Usage: %s <input file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    ignore (parse_file filename)
