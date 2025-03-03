(* lexer.mll - Lexical Analyzer *)
{
  open Token

  (* Helper function for keyword or identifiers *)
  let keyword_or_ident s =
    match s with
    | "Input" -> INPUT
    | "Print" -> PRINT
    | "true" -> BOOL true
    | "false" -> BOOL false
		| "int" -> INT_T
		| "float" -> FLOAT_T
		| "bool" -> BOOL_T
		| "vector" -> VECTOR_T
		| "matrix" -> MATRIX_T
    | _ -> IDENT s

}

(* Regular expressions for lexical tokens *)
let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum_score = ['a'-'z' 'A'-'Z' '0'-'9' '_']

let fname = '"' [^'"']* '"'
let ident_or_kw = alpha alnum_score*
let white_space = [' ' '\t' '\n']+

(* Regular Expressions for Constants *)
let int_reg = num+
let float_reg = num+ '.' num*
let float_sci_reg = num+ '.' num* ['e' 'E'] ['+' '-']? num+
let vec_reg = '[' (int_reg | float_reg | float_sci_reg) (',' (int_reg | float_reg | float_sci_reg))* ']'
let mat_reg = '[' vec_reg (';' vec_reg)* ']'

(* Tokenizer rules *)
rule token = parse
  | white_space    { token lexbuf }
  | ident_or_kw as s { keyword_or_ident s }
  | fname as s { FNAME (String.sub s 1 (String.length s - 2)) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LSQUARE }
  | ']' { RSQUARE }
  | int_reg as s { INT (int_of_string s) }
  | float_reg as s { FLOAT (float_of_string s) }
  | float_sci_reg as s { FLOAT (float_of_string s) }
  | eof { EOF }