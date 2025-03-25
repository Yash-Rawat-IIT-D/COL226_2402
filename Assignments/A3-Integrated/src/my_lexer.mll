(* lexer.mll - Lexical Analyzer *)
{
  	open My_ast
	open My_parser

	exception Illogical_Lex of string
  (* Helper function for keyword or identifiers *)
  let keyword_or_ident s =
    match s with
    | "Input" -> INPUT
    | "Print" -> PRINT
    | "true" -> CONS_B true
    | "false" -> CONS_B false
	| "int" -> INT_T
	| "float" -> FLOAT_T
	| "bool" -> BOOL_T
	| "vector_n" -> VECTOR_N_T
	| "vector_f" -> VECTOR_F_T
	| "matrix_n" -> MATRIX_N_T
	| "matrix_f" -> MATRIX_F_T
	| "neg" -> NEG
	| "not" -> NOT
	| "and" -> AND
	| "or" -> OR
	| "abs" -> ABS
	| "mod" -> MODULO
	| "inv" -> INV
	| "add_v" -> ADD_VEC
	| "scal_v" -> SCAL_VEC
	| "dot_v" -> DOT_PROD
	| "ang_v" -> ANGLE_VEC
	| "mag_v" -> MAG_VEC
	| "dim_v" -> DIM_VEC
	| "add_m" -> ADD_MAT
	| "scal_m" -> SCAL_MAT
	| "mul_mv" -> MAT_MUL_MAT
	| "transp_m" -> TRP_MAT
	| "det_m" -> DET_MAT
	| "if" -> IF
	| "then" -> THEN
	| "else" -> ELSE
	| "while" -> WHILE
	| "for" -> FOR
	| "elif" -> ELSE_IF 
	| "break" -> BREAK
	| "return" -> RETURN
	| "continue" -> CONTINUE
	| "def_vn" -> DEF_VN
	| "def_vf" -> DEF_VF
	| "def_mn" -> DEF_MN
	| "def_mf" -> DEF_MF
	| _ -> IDENT s

	(* Helper function to convert string to vector *)
	(* Easy to split on , after removing the outer [ and ]*)
	let str_to_vec_f s =
		let s = String.sub s 1 (String.length s - 2) |> String.trim in
		let elements = String.split_on_char ',' s in
		List.map (fun x -> float_of_string (String.trim x)) elements

	(* Helper function to convert string to matrix *)
	(* Uses depth to keep track of the matrix we are trying to extract *)
	(* More verbose due to helper function extract_rows *)

	let str_to_mat_f s =
  	let s = String.sub s 1 (String.length s - 2) in  (* Remove outer [ ] *)

		let rec extract_rows acc depth current chars =
			match chars with
			| [] -> List.rev (current :: acc)  (* Add the last row and return *)
			| '[' :: rest -> extract_rows acc (depth + 1) (current ^ "[") rest
			| ']' :: rest -> extract_rows acc (depth - 1) (current ^ "]") rest
			| ',' :: rest when depth = 0 ->  (* Only split at top-level commas *)
					extract_rows (current :: acc) depth "" rest
			| c :: rest -> extract_rows acc depth (current ^ String.make 1 c) rest
		in

		let row_strings = extract_rows [] 0 "" (List.of_seq (String.to_seq s)) in
		let clean_rows = List.map String.trim row_strings in  (* Trim spaces *)
		List.map str_to_vec_f clean_rows  (* Convert each row to a vector *)


	let str_to_vec_n s = 
		let s = String.sub s 1 (String.length s - 2) |> String.trim in
		let elements = String.split_on_char ',' s in
		List.map (fun x -> int_of_string (String.trim x)) elements

	let str_to_mat_n s =
		let s = String.sub s 1 (String.length s - 2) in
		
		let rec extract_rows acc depth current chars =
			match chars with
			| [] -> List.rev (current :: acc)  
			| '[' :: rest -> extract_rows acc (depth + 1) (current ^ "[") rest
			| ']' :: rest -> extract_rows acc (depth - 1) (current ^ "]") rest
			| ',' :: rest when depth = 0 ->  
					extract_rows (current :: acc) depth "" rest
			| c :: rest -> extract_rows acc depth (current ^ String.make 1 c) rest
			in 

		let row_strings = extract_rows [] 0 "" (List.of_seq (String.to_seq s)) in
		let clean_rows = List.map String.trim row_strings in
		List.map str_to_vec_n clean_rows

}

(* Regular expressions for lexical tokens *)
let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum_score = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let sign = ['-' '+']?

let fname = '"' [^'"']* '"'
let ident_or_kw = (alpha | '_'| '\'') alnum_score*
let w_space = [' ' '\t' '\n']
(* Regular Expressions for Integers, Floats *)

let int_reg = sign num+
let fl1 = sign num + "." num*
let fl2 = sign "." num+
let fl3 = num+ ['e' 'E'] sign num+
let float_sci_reg = sign (num+ '.' num* | '.' num+ | num+) ('e' | 'E') sign num+
let float_reg = fl1 | fl2 | fl3 | float_sci_reg

(* Regular Expressions for Vectors and Matrices *)

let vec_n_reg = '[' w_space* int_reg (w_space* ',' w_space*  int_reg)* w_space*  ']'
let mat_n_reg = '[' w_space*  vec_n_reg (w_space* ',' w_space*  vec_n_reg)* w_space*  ']'

let vec_f_reg = '[' w_space*  float_reg (w_space* ',' w_space*  float_reg)* w_space*  ']'
let mat_f_reg = '[' w_space*  vec_f_reg (w_space* ',' w_space*  vec_f_reg)* w_space*  ']'

(* Regexp for handling operations and  parenthesis*)

let brack_reg = ('(' | ')' | '{' | '}' | '[' | ']')
let num_op_reg = ('+'| '-' |'/' | '*')
let comp_reg = ( '=' | '<' | '>' | "!=" | "<=" | ">=")
let bsln_reg = "\\n"

(* Regexp for handling comments and multi line comments*)

let single_comm_reg = "//" [^'\n']* '\n'?
let multi_comm_reg = "/*" ([^'*']* ('*' [^'/'])? )* "*/"
let comm_reg = (single_comm_reg | multi_comm_reg)

(* Tokenizer rules *)
rule token = parse
	(* Skipping over whitespace	*)
  	| w_space+    { token lexbuf }
	(* keyword or identifier tokens *)
 	| ident_or_kw as s { keyword_or_ident s }
  	| fname as s { FNAME (String.sub s 1 (String.length s - 2)) }
  	(* Constants of Integer, Float, Vector, Matrix type*)
  	| int_reg as s { CONS_N (int_of_string s) }
  	|	float_reg as s { CONS_F (float_of_string s) }
	| (int_reg as vdim) w_space* (w_space+|bsln_reg) w_space* (vec_n_reg as vecn) {let vec = str_to_vec_n vecn in CONS_VN (int_of_string vdim, vec)}
	| (int_reg as mdim) w_space* ','? w_space* (int_reg as ndim) w_space* (w_space+|bsln_reg) w_space* (mat_n_reg as matn) {let mat = str_to_mat_n matn in CONS_MN (int_of_string mdim, int_of_string ndim, mat)}
	| (int_reg as vdim) w_space+ (vec_f_reg as vecf) {let vec = str_to_vec_f vecf in CONS_VF (int_of_string vdim, vec)}
	| (int_reg as mdim) w_space* ','? w_space* (int_reg as ndim) w_space* (w_space+|bsln_reg) w_space* (mat_f_reg as matf) {let mat = str_to_mat_f matf in CONS_MF (int_of_string mdim, int_of_string ndim, mat)}
	(* Handling Operations and Comparisons *) 
	(* Handling Brackets and Braces *)
	| '(' { LPAREN }
	| ')' { RPAREN }
	| '{' { LBRACE }
	| '}' { RBRACE }
	| '[' { LSQUARE }
	| ']' { RSQUARE }
	(* Handling Operations and Comparisons *)
	| '=' { EQ }
	| '<' { LT }
	| '>' { GT }
	| "!=" { NEQ }
	| "<=" { LE }
	| ">=" { GE }
	| '+' { ADD }
	| '-' { SUB }
	| '*' { MUL }
	| '/' { DIV }
	(* Handling Control and Branch Tokens *)
	| ":=" { ASSIGN }
	| ';' {SEMICOLON}
	| ":" {COLON}
	| "?" {QMARK}
	| ',' {COMMA}
	(* Handling Comments (Ignored) *)
	| comm_reg { token lexbuf }	
	(* Handling EOF *)
  	| eof { EOF }
	| _ as c { raise (Illogical_Lex (Printf.sprintf "Unrecognized Token: %c" c)) }