(* token.mli - Interface for tokens and other utilities for .mll / OCaml lex specifications *)

type vector = float list
type matrix = vector list

type token =
	(* Handling file input and print functionalities *)
	| PRINT  
	| INPUT
	| FNAME of string

	(* Hanadling of Parenthisis and Braces *)

	| LPAREN 	(* Opening Bracket = ( *)
 	| RPAREN 	(* Closing Bracket = ) *)
	| LBRACE 	(* Opening Brace = { *)
	| RBRACE 	(* Closing Brace = } *)
	| LSQUARE	(* Opening Square Bracket = [ *)
	| RSQUARE 	(* Closing Square Bracket = ] *)

	(* Handling of Constants and Identifiers,  Constructors for different types *)
	| INT_T
	| FLOAT_T
	| BOOL_T
	| VECTOR_T
	| MATRIX_T

	| INT of int
	|	FLOAT of float
	| BOOL of bool
	| IDENT of string

	(* Handling of Identifiers *)
	| EOF