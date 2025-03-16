open My_lexer
open My_parser
open My_ast
open Token 
(* Read entire file into a string *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Function to parse from string *)
let parse_from_string s =
  let lexbuf = Lexing.from_string s in
  try
    let ast = program token lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (string_of_program ast)
  with
  | My_lexer.Illogical_Lex msg -> Printf.eprintf "Lexer Error: %s\n" msg
  | My_ast.SyntaxError msg -> Printf.eprintf "Syntax Error: %s\n" msg
  | My_ast.TypeMismatch msg -> Printf.eprintf "Type Error: %s\n" msg
  | My_ast.DimensionMismatch msg -> Printf.eprintf "Dimension Error: %s\n" msg
  | End_of_file -> Printf.eprintf "Unexpected end of file\n"
  | Failure msg -> Printf.eprintf "Failure: %s\n" msg
  | _ -> Printf.eprintf "Unknown Error\n"

(* Main function *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s <input file>\n" Sys.argv.(0)
  else
    let input = read_file Sys.argv.(1) in
    parse_from_string input
