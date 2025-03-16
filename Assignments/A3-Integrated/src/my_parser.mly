(*===================================================================================*)
                (* Parser Specifications - COL226 Assignment 3 - 2023CS50334 *) 
(*===================================================================================*)


%{
    open My_ast
%}

/*===================================================================================*/
//                             Token Declarations for Grammar 
/*===================================================================================*/

/* Tokens for Primitive Data Types */
%token <int>                          CONS_N
%token <float>                        CONS_F
%token <bool>                         CONS_B
%token <string>                       IDENT
%token <string>                       FNAME
%token <int * int list>               CONS_VN                 
%token <int * float list>             CONS_VF               
%token <int * int * int list list>    CONS_MN      
%token <int * int * float list list>  CONS_MF    

/* Type Tokens for Type Checking */
%token INT_T FLOAT_T BOOL_T VECTOR_N_T VECTOR_F_T MATRIX_N_T MATRIX_F_T

/* Tokens for Operations on Primitive Data Types */
%token ADD MUL SUB DIV ABS
%token MODULO EQ NEQ LT GT LE GE

/* Tokens for Operations on Vectors */
%token ADD_VEC SCAL_VEC DOT_PROD ANGLE_VEC MAG_VEC DIM_VEC

/* Tokens for Operations on Matrices */
%token ADD_MAT SCAL_MAT MAT_MUL_MAT TRP_MAT DET_MAT INV

/* Tokens for Logical and Unary Operations */
%token NOT AND OR
%token NEG

/* Tokens for Control Flow and Structural Constructs */
%token LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE
%token IF THEN ELSE ELSE_IF WHILE FOR RETURN BREAK CONTINUE PRINT INPUT
%token SEMICOLON ASSIGN COMMA COLON QMARK EOF

/*===================================================================================*/
//                             Operator Precedence and Associativity
/*===================================================================================*/

// Lowest precedence for logical operations 
%left OR
%left AND

// Comparison operations higher than logical operators 
%left EQ NEQ LT GT LE GE

// Arithmetic operations (ADD, SUB) next 
%left ADD SUB ADD_VEC ADD_MAT

// Multiplication and vector/matrix-specific operations come next 
%left MUL DIV MODULO SCAL_VEC SCAL_MAT MAT_MUL_MAT DOT_PROD ANGLE_VEC

// Unary operations have the highest precedence 
%right NOT NEG MAG_VEC DIM TRP_MAT DET INV
%right QMARK COLON 

%nonassoc THEN
%nonassoc ELSE_IF
%nonassoc ELSE

/*===================================================================================*/
//                               Entry Point 
/*===================================================================================*/
%start program
%type <My_ast.program> program

%%

(*===================================================================================*)
// 					            Grammar Rules for the Parser
(*===================================================================================*)

inv_vec_mem:
  | CONS_N COMMA CONS_F { () } 
  | CONS_F COMMA CONS_N { () }
  | CONS_N COMMA inv_vec_mem { () }
  | CONS_F COMMA inv_vec_mem { () }
;

inv_vec:
    | CONS_N LSQUARE RSQUARE              { raise (Type_Mismatch "Empty Vector/Row of matrix not allowed") }
    | CONS_N LSQUARE inv_vec_mem RSQUARE  { raise (Type_Mismatch "Mixed Typing Detected in Vector") }
;

inv_mat_rows:
	| LSQUARE inv_vec_mem RSQUARE COMMA inv_mat_rows { () }
	| LSQUARE inv_vec_mem RSQUARE                    { () }  
;

inv_mat:
	| CONS_N CONS_N LSQUARE inv_mat_rows RSQUARE { raise (Type_Mismatch "Invalid matrix definition") }
;

/*===================================================================================*/
// A generator for the AST Nodes (base cases) for complex expressions
/*===================================================================================*/

constant:
  | CONS_N { VAL(INT_V($1)) }
  | CONS_F { VAL(FLT_V($1)) }
  | CONS_B { VAL(BL_V($1)) }
  | CONS_VN { 
      let (dim, v) = $1 in
      if not (vec_dim_check dim v) then
          raise (Dimension_Mismatch (
              "Expected vector of dimension " ^ string_of_int dim ^
              ", but got dimension " ^ string_of_int (List.length v)
          ))
      else
          VAL (NVEC_V v)
  }

  | CONS_VF { 
      let (dim, v) = $1 in
      if not (vec_dim_check dim v) then
          raise (Dimension_Mismatch (
              "Expected vector of dimension " ^ string_of_int dim ^
              ", but got dimension " ^ string_of_int (List.length v)
          ))
      else
          VAL (FVEC_V v)
  }

  | CONS_MN { 
      let (rows, cols, m) = $1 in
      if not (mat_dim_check rows cols m) then
          raise (Dimension_Mismatch (
              "Expected matrix with " ^ string_of_int rows ^ 
              " rows and " ^ string_of_int cols ^ 
              " columns, but found incorrect dimensions"
          ))
      else
          VAL (NMAT_V m)
  }

  | CONS_MF { 
      let (rows, cols, m) = $1 in
      if not (mat_dim_check rows cols m) then
          raise (Dimension_Mismatch (
              "Expected matrix with " ^ string_of_int rows ^ 
              " rows and " ^ string_of_int cols ^ 
              " columns, but found inconsistent dimensions"
          ))
      else
          VAL (FMAT_V m)
  }
;

/*===================================================================================*/
// Expression Rules
/*===================================================================================*/

expr:
  | constant { $1 }
  | IDENT { IDF($1) }
  | expr AND expr               { BIN_OP (And, $1, $3) }
  | expr OR expr                { BIN_OP (Or, $1, $3) }
  | expr ADD expr               { BIN_OP (Add, $1, $3) }
  | expr SUB expr               { BIN_OP (Sub, $1, $3) }
  | expr MUL expr               { BIN_OP (Mul, $1, $3) }
  | expr DIV expr               { BIN_OP (Div, $1, $3) }
  | expr MODULO expr            { BIN_OP (Modulo, $1, $3) }
  | expr EQ expr                { BIN_OP (Eq, $1, $3) }
  | expr NEQ expr               { BIN_OP (Neq, $1, $3) }
  | expr LT expr                { BIN_OP (Lt, $1, $3) }
  | expr GT expr                { BIN_OP (Gt, $1, $3) }
  | expr LE expr                { BIN_OP (Leq, $1, $3) }
  | expr GE expr                { BIN_OP (Geq, $1, $3) }
  | NOT expr                    { UN_OP (Not, $2) }
  | NEG expr                    { UN_OP (Neg, $2) }
  | TRP_MAT expr                { UN_OP (Trp_Mat, $2) }
  | DET expr                    { UN_OP (Det, $2) }
  | INV expr                    { UN_OP (Inv, $2) }
  | INPUT LPAREN RPAREN         { Input None }
  | INPUT LPAREN FNAME RPAREN   { Input (Some $3) }
  | expr QMARK expr COLON expr  { COND ($1, $3, $5) }
  | LPAREN expr RPAREN          { $2 }
;

/*===================================================================================*/
// Statement Rules
/*===================================================================================*/

stmt:
  | INT_T IDENT ASSIGN expr SEMICOLON               { Assign (Some T_INT, $2, $4) }
  | FLOAT_T IDENT ASSIGN expr SEMICOLON             { Assign (Some T_FLOAT, $2, $4) }
  | BOOL_T IDENT ASSIGN expr SEMICOLON              { Assign (Some T_BOOL, $2, $4) }
  | PRINT LPAREN expr RPAREN SEMICOLON              { Print $3 }
  | RETURN expr SEMICOLON                           { Return $2 }
  | BREAK SEMICOLON                                 { Break }
  | CONTINUE SEMICOLON                              { Continue }
  | LBRACE stmt_list RBRACE                         { Block $2 }
;

stmt_list:
  |                        { [] }
  | stmt stmt_list         { $1 :: $2 }
;

/*===================================================================================*/
// Program Definition
/*===================================================================================*/

program:
  stmt_list EOF { $1 }
;
