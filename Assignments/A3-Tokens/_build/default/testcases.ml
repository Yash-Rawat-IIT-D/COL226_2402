open Token
open My_lexer
(* Helper function to tokenize a string and return the token list *)
let tokenize str =
  let lexbuf = Lexing.from_string str in
  let rec get_tokens acc =
    let token = My_lexer.token lexbuf in
    if token = EOF then List.rev (token :: acc)
    else get_tokens (token :: acc)
  in
  get_tokens []

(* Basic token tests *)
let%test "Integer tokens" = tokenize "42" = [CONS_N 42; EOF]
let%test "Negative integer" = tokenize "-42" = [CONS_N (-42); EOF]
let%test "Float tokens" = tokenize "3.14" = [CONS_F 3.14; EOF]
let%test "Scientific notation" = tokenize "1.2e3" = [CONS_F 1200.0; EOF]
let%test "Negative float" = tokenize "-3.14" = [CONS_F (-3.14); EOF]

(* Parentheses and braces *)
let%test "Parentheses" = tokenize "()" = [LPAREN; RPAREN; EOF]
let%test "Braces" = tokenize "{}" = [LBRACE; RBRACE; EOF]
let%test "Square brackets" = tokenize "[]" = [LSQUARE; RSQUARE; EOF]

(* Operators *)
let%test "Arithmetic operators" = tokenize "+ - * /" = [ADD; SUB; MUL; DIV; EOF]
let%test "Comparison operators" = tokenize "< > <= >= = !=" = [LT; GT; LE; GE; EQ; NEQ; EOF]
let%test "Boolean operators" = tokenize "& | !" = [AND; OR; NOT; EOF]

(* Keywords *)
let%test "Control flow keywords" = tokenize "if else while for return" = [IF; ELSE; WHILE; FOR; RETURN; EOF]
let%test "Type keywords" = tokenize "int float bool vector_n vector_f matrix_n matrix_f" = [INT_T; FLOAT_T; BOOL_T; VECTOR_N_T; VECTOR_F_T; MATRIX_N_T; MATRIX_F_T; EOF]

(* Constants and identifiers *)
let%test "Boolean constants" = tokenize "true false" = [CONS_B true; CONS_B false; EOF]
let%test "Identifier" = tokenize "variable_name" = [IDENT "variable_name"; EOF]
let%test "File name" = tokenize "\"filename.txt\"" = [FNAME "filename.txt"; EOF]

(* Punctuation *)
let%test "Punctuation" = tokenize "; ," = [SEMICOLON; COMMA; EOF]

(* Vector and matrix constants *)
let%test "Integer vector" = tokenize "[1, 2, 3]" = [CONS_VN (3, [1; 2; 3]); EOF]
let%test "Float vector" = tokenize "[1.1, 2.2, 3.3]" = [CONS_VF (3, [1.1; 2.2; 3.3]); EOF]
let%test "Integer matrix" = tokenize "[[1, 2], [3, 4]]" = [CONS_MN (2, 2, [[1; 2]; [3; 4]]); EOF]
let%test "Float matrix" = tokenize "[[1.1, 2.2], [3.3, 4.4]]" = [CONS_MF (2, 2, [[1.1; 2.2]; [3.3; 4.4]]); EOF]

(* Combined tests *)
let%test "Simple expression" = tokenize "x = 5 + 3;" = [IDENT "x"; ASSIGN; CONS_N 5; ADD; CONS_N 3; SEMICOLON; EOF]
let%test "Vector operation" = tokenize "dot_product([1, 2], [3, 4])" = [IDENT "dot_product"; LPAREN; CONS_VN (2, [1; 2]); COMMA; CONS_VN (2, [3; 4]); RPAREN; EOF]
let%test "If statement" = tokenize "if (x > 0) { return true; }" = [IF; LPAREN; IDENT "x"; GT; CONS_N 0; RPAREN; LBRACE; RETURN; CONS_B true; SEMICOLON; RBRACE; EOF]

(* Comment handling *)
let%test "Single line comment" = tokenize "x = 5; // This is a comment\n y = 6;" = [IDENT "x"; ASSIGN; CONS_N 5; SEMICOLON; IDENT "y"; ASSIGN; CONS_N 6; SEMICOLON; EOF]
let%test "Multi line comment" = tokenize "x = 5; /* This is a \n multiline comment */ y = 6;" = [IDENT "x"; ASSIGN; CONS_N 5; SEMICOLON; IDENT "y"; ASSIGN; CONS_N 6; SEMICOLON; EOF]

(* Edge cases *)
let%test "Empty input" = tokenize "" = [EOF]
let%test "Whitespace only" = tokenize "   \t\n   " = [EOF]
let%test "Consecutive operators" = tokenize "a++b" = [IDENT "a"; ADD; ADD; IDENT "b"; EOF]
let%test "Mixed tokens" = tokenize "f(x) = 2.5 * x + [1, 2, 3];" = 
  [IDENT "f"; LPAREN; IDENT "x"; RPAREN; ASSIGN; CONS_F 2.5; MUL; 
   IDENT "x"; ADD; CONS_VN (3, [1; 2; 3]); SEMICOLON; EOF]