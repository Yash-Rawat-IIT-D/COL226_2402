(* token.mli - Interface for tokens and other utilities for .mll / OCaml lex specifications *)

type vector_fl = float list
type vector_int = int list
type matrix_fl = vector_fl list
type matrix_int = vector_int list

type token =
	(* Handling file input and print functionalities *)
	| PRINT | INPUT | FNAME of string

	(* Hanadling of Parenthisis and Braces *)

	| LPAREN  | RPAREN 	(* Bracket = ( Or ) *)
	| LBRACE  | RBRACE 	(*Logical Operations Brace = { Or } *)
	| LSQUARE | RSQUARE (* Square Bracket = [ Or ]] *)

	(* Handling of Constants and Identifiers,  Constructors for different types *)
	| INT_T | FLOAT_T | BOOL_T | VECTOR_N_T | VECTOR_F_T | MATRIX_N_T | MATRIX_F_T

	| CONS_N of int |	CONS_F of float | CONS_B of bool | IDENT of string
	
	| CONS_VN of int * vector_int | CONS_VF of int * vector_fl

	| CONS_MN of int * int * matrix_int | CONS_MF of int * int * matrix_fl

	(* Handling Operations on Boolean *)
	
	| NOT | AND | OR
	
	(* Handling Common Operations on Integer and Floats *)
	| ADD | MUL | SUB | DIV | ABS

	(* Handling Integer Specfic Methods *)
	| MODULO | EQ | NEQ | LT | GT | LE | GE

	(* Handling Operations on Vectors *)

	| ADD_VEC | SCAL_VEC | DOT_PROD | ANGLE_VEC | MAG_VEC | DIM_VEC 

	(* Handling Operations on Matrices *)

	| ADD_MAT | SCAL_MAT | MAT_MUL_MAT | TRP_MAT | DET_MAT

	(* Handling Control and Branch Constructs *)

	| ASSIGN | IF | THEN |  ELSE | ELSE_IF | WHILE | FOR | RETURN | BREAK | CONTINUE

	| SEMICOLON | COMMA 

	(* Handling EOF *)

	| EOF