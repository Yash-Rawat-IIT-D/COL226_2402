type token =
  | CONS_N of (
# 15 "src/my_parser.mly"
        int
# 6 "src/my_parser.mli"
)
  | CONS_F of (
# 16 "src/my_parser.mly"
        float
# 11 "src/my_parser.mli"
)
  | CONS_B of (
# 17 "src/my_parser.mly"
        bool
# 16 "src/my_parser.mli"
)
  | IDENT of (
# 18 "src/my_parser.mly"
        string
# 21 "src/my_parser.mli"
)
  | FNAME of (
# 19 "src/my_parser.mly"
        string
# 26 "src/my_parser.mli"
)
  | CONS_VN of (
# 20 "src/my_parser.mly"
        int * int list
# 31 "src/my_parser.mli"
)
  | CONS_VF of (
# 21 "src/my_parser.mly"
        int * float list
# 36 "src/my_parser.mli"
)
  | CONS_MN of (
# 22 "src/my_parser.mly"
        int * int * int list list
# 41 "src/my_parser.mli"
)
  | CONS_MF of (
# 23 "src/my_parser.mly"
        int * int * float list list
# 46 "src/my_parser.mli"
)
  | INT_T
  | FLOAT_T
  | BOOL_T
  | VECTOR_N_T
  | VECTOR_F_T
  | MATRIX_N_T
  | MATRIX_F_T
  | ADD
  | MUL
  | SUB
  | DIV
  | ABS
  | MODULO
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | ADD_VEC
  | SCAL_VEC
  | DOT_PROD
  | ANGLE_VEC
  | MAG_VEC
  | DIM_VEC
  | ADD_MAT
  | SCAL_MAT
  | MAT_MUL_MAT
  | TRP_MAT
  | DET_MAT
  | INV
  | NOT
  | AND
  | OR
  | NEG
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | IF
  | THEN
  | ELSE
  | ELSE_IF
  | WHILE
  | FOR
  | RETURN
  | BREAK
  | CONTINUE
  | PRINT
  | INPUT
  | SEMICOLON
  | ASSIGN
  | COMMA
  | COLON
  | QMARK
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> My_ast.program
