(* main.ml - For Test Cases *)

open Lexing
open Token
open Lexer

(* Helper function to print a float list as a comma-separated vector *)
let print_vector n vec =
  let elements = String.concat ", " (List.map string_of_float vec) in
  Printf.printf "VECTOR(%d [%s])\n" n elements

(* Helper function to print a matrix with comma-separated rows *)
let print_matrix m n mat =
  let rows = List.map (fun row ->
    let elements = String.concat ", " (List.map string_of_float row) in
    Printf.sprintf "[%s]" elements
  ) mat in
  Printf.printf "MATRIX(%d, %d, [%s])\n" m n (String.concat "; " rows)

(* Function to print tokens *)
let print_token = function
  | PRINT -> print_endline "PRINT"
  | INPUT -> print_endline "INPUT"
  | FNAME s -> Printf.printf "FNAME(%s)\n" s
  | LPAREN -> print_endline "LPAREN"
  | RPAREN -> print_endline "RPAREN"
  | LBRACE -> print_endline "LBRACE"
  | RBRACE -> print_endline "RBRACE"
  | LSQUARE -> print_endline "LSQUARE"
  | RSQUARE -> print_endline "RSQUARE"
  | INT_T -> print_endline "INT_T"
  | FLOAT_T -> print_endline "FLOAT_T"
	| BOOL_T -> print_endline "BOOL_T"
	| VECTOR_T -> print_endline "VECTOR_T"
	| MATRIX_T -> print_endline "MATRIX_T"
  | INT x -> Printf.printf "INT(%d)\n" x
  | FLOAT x -> Printf.printf "FLOAT(%f)\n" x
  | BOOL x -> Printf.printf "BOOL(%b)\n" x
  | IDENT s -> Printf.printf "IDENT(%s)\n" s
  | _ -> print_endline "Unrecognised Token !" 

(* Recursive function to tokenize input *)
let rec lex_all lexbuf =
  match Lexer.token lexbuf with
  | EOF -> ()
  | my_token ->
      print_token my_token;
      lex_all lexbuf

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lex_all lexbuf;
    close_in ic