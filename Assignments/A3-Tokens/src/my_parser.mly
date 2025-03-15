%{
    open My_ast
    open Token
%}

(*===================================================================================*)
// 													Token Declarations for Grammar 
(*===================================================================================*)


// Tokens for Primitive Data Types 
%token <int>                          CONS_N
%token <float>                        CONS_F
%token <bool>                         CONS_B
%token <string>                       IDENT
%token <string>                       FNAME
%token <int * int list>               CONS_VN                 
%token <int * float list>             CONS_VF               
%token <int * int * int list list>    CONS_MN      
%token <int * int * float list list>  CONS_MF    

/* Needed only for enforcing and checking Yacc level type consistency
   Also passed to the AST for type checking and evaluation */
%token INT_T FLOAT_T BOOL_T VECTOR_N_T VECTOR_F_T MATRIX_N_T MATRIX_F_T

// Tokens for Operations on Primitive Data Types 
%token ADD MUL SUB DIV ABS
%token MODULO EQ NEQ LT GT LE GE

// Tokens for Operations on Vectors 
%token ADD_VEC SCAL_VEC DOT_PROD ANGLE_VEC MAG_VEC DIM_VEC

// Tokens for Operations on Matrices 
%token ADD_MAT SCAL_MAT MAT_MUL_MAT TRP_MAT DET_MAT INV

// Tokens for Logical and Unary Operations 
%token NOT AND OR
%token NEG

// Tokens for Control Flow and Branching Constructs and Structural Constructs 
%token LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE
%token IF THEN ELSE ELSE_IF WHILE FOR RETURN BREAK CONTINUE PRINT INPUT
%token SEMICOLON ASSIGN COMMA EOF

// Operator Precedence and Associativity 
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
%right NOT NEG MAG_VEC DIM TRANSP DET INV

%nonassoc THEN
%nonassoc ELSE_IF
%nonassoc ELSE

(*===================================================================================*)
// 													Grammar Rules for the Parser
(*===================================================================================*)

%start program
%type <My_ast.program> program

// Rules for handling type specification
type_spec:
    | INT_T { T_INT }
    | FLOAT_T { T_FLOAT }
    | BOOL_T { T_BOOL }
    | VECTOR_N_T { T_VEC_N }
    | VECTOR_F_T { T_FLOAT_N }
    | MATRIX_N_T { T_MAT_N }
    | MATRIX_F_T { T_MAT_F }

// Rules for handling rejecting incorrect vectors and matrices (mixed typing)
inv_vec_mem:
	| CONS_N COMMA CONS_F 
	| CONS_F COMMA CONS_N
	| CONS_N COMMA inv_vec_mem
	| CONS_F COMMA inv_vec_mem

inv_vec:
	| CONS_N LSQUARE RSQUARE { raise TypeMismatch "Empty Vector/Row of matrix not allowed" }
	| CONS_N LSQUARE inv_vec_mem RSQUARE { raise TypeMismatch "Mixed Typing Detected in Vector" }

inv_mat_rows:
	| LSQUARE inv_vec_mem RSQUARE COMMA inv_mat_rows
	| LSQUARE inv_vec_mem RSQUARE

inv_mat:
	| CONS_N CONS_N LSQUARE inv_mat_rows RSQUARE { raise (TypeMismatch "Invalid matrix definition") } 

// A generator for the AST Nodes (base cases) for complex expressions
constant:   
		// Primitve Data Types such as Int, Float and Bool
    | CONS_N { VAL(INT_V($1)) }
    | CONS_F { VAL(FLT_V($1)) }
    | CONS_B { VAL(BL_V($1)) }

    // Vector and Matrix Constants
    | CONS_VN { 
				let (dim, v) = $1 in
          if not (vector_dim_check dim v) then
            raise (DimensionMismatch (
              "Expected vector of dimesnion " ^ string_of_int dim ^", but got dimesnion " ^ string_of_int (List.length v)
          	))
          else
            VAL (NVEC_V v)
        }

    | CONS_VF { 
				let (dim, v) = $1 in
          if not (vector_dim_check dim v) then
            raise (DimensionMismatch (
              "Expected vector of dimesnion " ^ string_of_int dim ^
              ", but got dimesnion " ^ string_of_int (List.length v)
            ))
          else
            VAL (FVEC_V v)
        }

    | CONS_MN { 
				let (rows, cols, m) = $1 in
          if not (matrix_dim_check rows cols m) then
            raise (DimensionMismatch (
              "Expected matrix with " ^ string_of_int rows ^ 
              " rows and " ^ string_of_int cols ^ 
              " columns, but found incorrect dimensions"
            ))
          else
            VAL (NMAT_V m)
        }

    | CONS_MF { 
				let (rows, cols, m) = $1 in
          if not (matrix_dim_check rows cols m) then
            raise (DimensionMismatch (
              "Expected matrix with " ^ string_of_int rows ^ 
              " rows and " ^ string_of_int cols ^ 
              " columns, but found inconsistent dimensions"
            ))
          else
            VAL (FMAT_V m)
        }




// Rules for handling the AST Nodes and creation of complex AST of expressions
expr:
	// Base Case of the AST
	| constant { $1 }
	| IDENT { IDF($1) }
  // Binary Logical Operations
  | expr AND expr             { BIN_OP (And, $1, $3) }
  | expr OR expr              { BIN_OP (Or, $1, $3) }
  // Binary Numerical Operations
  | expr ADD expr             { BIN_OP (Add, $1, $3) }
  | expr SUB expr             { BIN_OP (Sub, $1, $3) }
  | expr MUL expr             { BIN_OP (Mul, $1, $3) }
  | expr DIV expr             { BIN_OP (Div, $1, $3) }
  | expr MODULO expr             { BIN_OP (Modulo, $1, $3) }
  // Binary Comparison Operations
  | expr EQ expr              { BIN_OP (Eq, $1, $3) }
  | expr NEQ expr             { BIN_OP (Neq, $1, $3) }
  | expr LT expr              { BIN_OP (Lt, $1, $3) }
  | expr GT expr              { BIN_OP (Gt, $1, $3) }
  | expr LE expr              { BIN_OP (Leq, $1, $3) }
  | expr GE expr              { BIN_OP (Geq, $1, $3) }
  // Binary Vector and Matrix Operations
  | expr ADD_VEC expr         { BIN_OP (AddVec, $1, $3) }
  | expr SCAL_VEC expr        { BIN_OP (Scal_Vec, $1, $3) }
  | expr DOT_PROD expr        { BIN_OP (DotProd, $1, $3) }
  | expr ANGLE_VEC expr           { BIN_OP (Angle, $1, $3) }
  | expr ADD_MAT expr         { BIN_OP (AddMat, $1, $3) }
  | expr SCAL_MAT expr        { BIN_OP (Scal_Mat, $1, $3) }
  | expr MAT_MUL_MAT expr     { BIN_OP (MatMulMat, $1, $3) }
  // Unary Operations 
  | NOT expr                  { UN_OP (Not, $2) }
  | NEG expr                  { UN_OP (Neg, $2) }
  | MAG_VEC expr                 { UN_OP (Mag_v, $2) }
  | DIM expr                  { UN_OP (Dim, $2) }
  | TRP_MAT expr               { UN_OP (Transp, $2) }
  | DET expr                  { UN_OP (Det, $2) }
  | INV expr                  { UN_OP (Inv, $2) }
  // Input Expression
  // Input and Output Statements
  | INPUT LPAREN RPAREN       { Input None }
  | INPUT LPAREN FNAME RPAREN { Input (Some $3) }
  // Cond Expression
  | IF expr THEN expr ELSE expr { COND ($2, $4, $6) }
  // Parenthesized Expressions
  | LPAREN expr RPAREN        { $2 }
  | _                         { raise (SyntaxError "Invalid Expression") }

// Rules for generating statements and handling control flow constructs
stmt_list:
    |                        { [] }
    | stmt stmt_list         { $2 @ [$1] }

stmt:
  // Assignment Statement with Type Specification
  | INT_T IDENT ASSIGN expr SEMICOLON               { Assign (Some T_INT, $2, $4) }
  | FLOAT_T IDENT ASSIGN expr SEMICOLON             { Assign (Some T_FLOAT, $2, $4) }
  | BOOL_T IDENT ASSIGN expr SEMICOLON              { Assign (Some T_BOOL, $2, $4) }
  | VECTOR_N_T IDENT ASSIGN expr SEMICOLON          { Assign (Some T_VEC_N, $2, $4) }
  | VECTOR_F_T IDENT ASSIGN expr SEMICOLON          { Assign (Some T_FLOAT_N, $2, $4) }
  | MATRIX_N_T IDENT ASSIGN expr SEMICOLON          { Assign (Some T_MAT_N, $2, $4) }
  | MATRIX_F_T IDENT ASSIGN expr SEMICOLON          { Assign (Some T_MAT_F, $2, $4) }
  // Assignment Statement without Type Specification
  | expr ASSIGN expr SEMICOLON { 
      match $1 with
        | IDF id -> Assign (None, id, $3)
        | _ -> raise (SyntaxError "Invalid Assignment - LHS must be an identifier")
    }
  // For and While Loops
  | FOR LPAREN stmt expr SEMICOLON stmt RPAREN stmt   {For ($3, $4, $6, $8)}
  | WHILE LPAREN expr RPAREN stmt                     { While ($3, $5) }
  // Print Statement
  | PRINT LPAREN expr RPAREN SEMICOLON                { Print $3 }
  // Return Break and Continue Handling 
  | RETURN expr SEMICOLON                             { Return $2 }
  | BREAK SEMICOLON                                   { Break }
  | CONTINUE SEMICOLON                                { Continue }
  // If-Else Statement
  | IF LPAREN expr RPAREN stmt ELSE_IF stmt ELSE stmt { Ifte ($3, $5, Some (Ifte ($7, $9, None))) }

  | IF LPAREN expr RPAREN stmt ELSE stmt              { Ifte ($3, $5, Some $7) }
  | IF LPAREN expr RPAREN stmt                        { Ifte ($3, $5, None) }
  // Block Statement - A sequence of statements
  | LBRACE stmt_list RBRACE                           { Block $2 }

// Program is a sequence of statements followed by EOF
program:
  stmt_list EOF { $1 }
