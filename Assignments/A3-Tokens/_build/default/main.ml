(* main.ml - For Test Cases *)

(* open Lexing *)
open Token
(* open My_lexer *)

let print_vector_fl dim vec =
  let elements = String.concat ", " (List.map string_of_float vec) in
  Printf.printf "CONS_VF(%d [%s])\n" dim elements

let print_matrix_fl dim_m dim_n mat =
      let rows = List.map (fun row -> 
        let elements = String.concat ", " (List.map string_of_float row) in
        Printf.sprintf "[%s]" elements
      ) mat in
      Printf.printf "CONS_MF(%d %d [%s])\n" (dim_m) (dim_n) (String.concat ", " rows) 

let print_vector_int dim vec =
  let elements = String.concat ", " (List.map string_of_int vec) in
  Printf.printf "CONS_VN(%d [%s])\n" dim elements 

let print_matrix_int dim_m dim_n mat =
    let rows = List.map (fun row -> 
      let elements = String.concat ", " (List.map string_of_int row) in
      Printf.sprintf "[%s]" elements
    ) mat in
    Printf.printf "CONS_MN(%d %d [%s])\n" (dim_m) (dim_n) (String.concat ", " rows)


(* Function to print tokens *)
let print_token (tok : token) = match tok with
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
  | VECTOR_N_T -> print_endline "VECTOR_N_T"
  | VECTOR_F_T -> print_endline "VECTOR_F_T"
  | MATRIX_N_T -> print_endline "MATRIX_N_T"
  | MATRIX_F_T -> print_endline "MATRIX_F_T"
  | CONS_N n -> Printf.printf "CONS_N(%d)\n" n
  | CONS_F f -> Printf.printf "CONS_F(%f)\n" f
  | CONS_B b -> Printf.printf "CONS_B(%b)\n" b
  | IDENT s -> Printf.printf "IDENT(%s)\n" s
  | CONS_VN (n,v) -> print_vector_int n v
  | CONS_VF(n,v) -> print_vector_fl n v 
  | CONS_MN (m,n,mat) -> print_matrix_int m n mat
  | CONS_MF (m,n,mat) -> print_matrix_fl m n mat
  | NOT -> print_endline "NOT"
  | AND -> print_endline "AND"
  | OR -> print_endline "OR"
  | ADD -> print_endline "ADD"
  | MUL -> print_endline "MUL"
  | SUB -> print_endline "SUB"
  | DIV -> print_endline "DIV"
  | ABS -> print_endline "ABS"
  | MODULO -> print_endline "MODULO"
  | EQ -> print_endline "EQ"
  | NEQ -> print_endline "NEQ"
  | LT -> print_endline "LT"
  | GT -> print_endline "GT"
  | LE -> print_endline "LE"
  | GE -> print_endline "GE"
  | ADD_VEC -> print_endline "ADD_VEC"
  | SCAL_VEC -> print_endline "SCAL_VEC"
  | DOT_PROD -> print_endline "DOT_PROD"
  | ANGLE_VEC -> print_endline "ANGLE_VEC"
  | MAG_VEC -> print_endline "MAG_VEC"
  | DIM_VEC -> print_endline "DIM_VEC"
  | ADD_MAT -> print_endline "ADD_MAT"
  | SCAL_MAT -> print_endline "SCAL_MAT"
  | MAT_MUL_MAT -> print_endline "MAT_MUL_MAT"
  | TRP_MAT -> print_endline "TRP_MAT"
  | DET_MAT -> print_endline "DET_MAT"
  | ASSIGN -> print_endline "ASSIGN"
  | IF -> print_endline "IF"
  | ELSE -> print_endline "ELSE"
  | ELSE_IF -> print_endline "ELSE_IF"
  | WHILE -> print_endline "WHILE"
  | FOR -> print_endline "FOR"
  | BREAK -> print_endline "BREAK"
  | CONTINUE -> print_endline "CONTINUE"
  | SEMICOLON -> print_endline "SEMICOLON"
  | COMMA -> print_endline "COMMA"
  | RETURN -> print_endline "RETURN"
  | _ -> print_endline "Unrecognised Token"
  
(* Recursive function to tokenize input *)
let rec lex_all lexbuf =
  match My_lexer.token lexbuf with
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