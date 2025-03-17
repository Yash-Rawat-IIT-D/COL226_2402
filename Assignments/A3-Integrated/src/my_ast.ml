(*===================================================================================*)
                (* AST Specifications - COL226 Assignment 3 - 2023CS50334 *) 
(*===================================================================================*)


(*  Type Definitions  *)
type vector_fl = float list
type vector_int = int list
type matrix_fl = vector_fl list
type matrix_int = vector_int list

(* Exceptions to be used by grammar, accessed via my_ast.ml *)
exception Syntax_Error of string  
exception Type_Mismatch of string
exception Dimension_Mismatch of string
exception UndefinedVariable of string
exception Undefined_Expression of string

(* Exceptions accessed by prog using my_ast.ml*)
exception Var_Not_Found of string
exception Empty_Env of string  
exception Type_Error of string
exception Undefined_Var of string
exception Division_by_zero of string

(*===================================================================================*)
                          (* Vector and Matrix Helper *)
(*===================================================================================*)

let vec_dim vec = List.length vec
let mat_dim mat = match mat with 
  | [] -> raise (Dimension_Mismatch ("Empty Matrix not allowed"))
  | row::_ -> (List.length mat, List.length row)

let vec_dim_check exp_dim vec =
  List.length vec = exp_dim

let mat_dim_check exp_rows exp_cols mat =
  let rows = List.length mat in
  if rows <> exp_rows then false
  else
    let cols_matched = List.fold_left (fun acc row -> acc && (List.length row = exp_cols)) true mat in
    cols_matched

let add_vec_n v1 v2 =
  if vec_dim v1 <> vec_dim v2 then raise (Dimension_Mismatch ("Vector dimensions do not match for addition")) 
  else
    List.map2 (+) v1 v2

let add_vec_f v1 v2 =
  if List.length v1 <> List.length v2 then
    raise (Dimension_Mismatch "Vector dimensions do not match for addition")
  else
    List.map2 (+.) v1 v2

let add_mat_n m1 m2 =
  if List.length m1 <> List.length m2 then
    raise (Dimension_Mismatch("Matrix row dimensions not same for addition"))
  else if List.length (List.hd m1) <> List.length (List.hd m2) then
    raise (Dimension_Mismatch("Matrix column dimensions not same for addition"))
  else
    List.map2 (fun r1 r2 -> add_vec_n r1 r2) m1 m2

let add_mat_f m1 m2 =
  if List.length m1 <> List.length m2 then
    raise (Dimension_Mismatch("Matrix row dimensions not same for addition"))
  else if List.length (List.hd m1) <> List.length (List.hd m2) then
    raise (Dimension_Mismatch("Matrix column dimensions not same for addition"))
  else
    List.map2 (fun r1 r2 -> add_vec_f r1 r2) m1 m2

(* Values representing the primary data types in our PL *)
(* Otherwise the basecases of our AST are now handled in a separate specification *)

type value = 
  | INT_V of int | FLT_V of float | BL_V of bool
  | NVEC_V of int list | FVEC_V of float list 
  | NMAT_V of int list list | FMAT_V of float list list 
  | FILE_V of string

(* For handling type of assignments, and runtime checks for type consistency *)
(* Eg : Compatibility of dimensions and types for matrix multiplication *)
type typ = T_INT | T_FLOAT | T_BOOL
          | T_VEC_N | T_VEC_F | T_MAT_N | T_MAT_F | T_INP

type etyp = 
  | E_INT | E_FLOAT | E_BOOL
  | E_VEC_N of int | E_VEC_F of int 
  | E_MAT_N of int * int | E_MAT_F of int * int
  | E_INP

(* Majority operations of primary data types as supported by our PL fall in the \
   Category of Unary or binary operations , So to provide another abstract template for our 
   AST expressions, we will have a separate specification for the operators 
   
  Appropriate overloading will be handled at the time of type checking and evaluation
  of the AST's *)

(* Binary Operators *)

type bin_op = 
  | Add | Mul | Sub | Div | And | Or
  | Modulo | Eq | Neq | Geq | Leq | Gt | Lt 
  | Dot_Prod | Angle | Add_Vec | Scal_Vec 
  | Add_Mat  | Scal_Mat  | Mat_Mul_Mat 

(* Unary Operators *)
  
type un_op = Not | Neg 
  | Mag_v | Dim | Trp_Mat | Det | Inv 

(* AST for the Expressions *)
type exp =  
  | Input of string option (* Assigning some variable value in current state of program *)
  | IDF of string
  | VAL of value
  | BIN_OP of bin_op * exp * exp
  | UN_OP of un_op * exp
  | COND of exp * exp * exp

type stmt =
  | Assign of typ option * string * exp
  | Ifte of exp * stmt * stmt option
  | While of exp * stmt
  | For of stmt * exp * stmt * stmt (* Variable , Initialisation, Condition and Update *)
  | Return of exp
  | Break
  | Continue
  | Print of exp
  | Block of stmt list  (* One of the most important ideas of a program *)


type program = stmt list

(*===================================================================================*)
                              (* Utility Functions *)
(*===================================================================================*)

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

let print_value = function
  | INT_V i -> Printf.printf "%d\n" i
  | FLT_V f -> Printf.printf "%f\n" f
  | BL_V b -> Printf.printf "%b\n" b
  | NVEC_V v -> print_vector_int (List.length v) v
  | FVEC_V v -> print_vector_fl (List.length v) v
  | NMAT_V m -> 
      let rows = List.length m in
      let cols = if rows > 0 then List.length (List.hd m) else 0 in
      print_matrix_int rows cols m
  | FMAT_V m -> 
      let rows = List.length m in
      let cols = if rows > 0 then List.length (List.hd m) else 0 in
      print_matrix_fl rows cols m
  | FILE_V s -> Printf.printf "%s"s 

let convert_to_etype = function
  | T_INT -> E_INT
  | T_FLOAT -> E_FLOAT
  | T_BOOL -> E_BOOL
  | T_VEC_N -> E_VEC_N 0
  (* Default dimension, will be checked during assignment *)
  | T_VEC_F -> E_VEC_F 0
  | T_MAT_N -> E_MAT_N (0, 0)
  | T_MAT_F -> E_MAT_F (0, 0)
  | T_INP -> E_INP

let compatible_types t1 t2 =
  match t1, t2 with
  | E_INT, E_INT | E_FLOAT, E_FLOAT | E_BOOL, E_BOOL -> true
  | E_VEC_N d1, E_VEC_N d2 -> d1 = 0 || d2 = 0 || d1 = d2
  | E_VEC_F d1, E_VEC_F d2 -> d1 = 0 || d2 = 0 || d1 = d2
  | E_MAT_N (r1, c1), E_MAT_N (r2, c2) -> 
      (r1 = 0 && c1 = 0) || (r2 = 0 && c2 = 0) || (r1 = r2 && c1 = c2)
  | E_MAT_F (r1, c1), E_MAT_F (r2, c2) -> 
      (r1 = 0 && c1 = 0) || (r2 = 0 && c2 = 0) || (r1 = r2 && c1 = c2)
  | _ -> false
(* Convert float vector to string *)
let string_of_vector_fl dim vec =
  let elements = String.concat ", " (List.map string_of_float vec) in
  Printf.sprintf "CONS_VF(%d [%s])" dim elements

(* Convert float matrix to string *)
let string_of_matrix_fl dim_m dim_n mat =
  let rows = List.map (fun row -> 
    let elements = String.concat ", " (List.map string_of_float row) in
    Printf.sprintf "[%s]" elements
  ) mat in
  Printf.sprintf "CONS_MF(%d %d [%s])" dim_m dim_n (String.concat ", " rows)

(* Convert int vector to string *)
let string_of_vector_int dim vec =
  let elements = String.concat ", " (List.map string_of_int vec) in
  Printf.sprintf "CONS_VN(%d [%s])" dim elements

(* Convert int matrix to string *)
let string_of_matrix_int dim_m dim_n mat =
  let rows = List.map (fun row -> 
    let elements = String.concat ", " (List.map string_of_int row) in
    Printf.sprintf "[%s]" elements
  ) mat in
  Printf.sprintf "CONS_MN(%d %d [%s])" dim_m dim_n (String.concat ", " rows)

let string_of_value v = match v with
  | INT_V i -> string_of_int i
  | FLT_V f -> string_of_float f
  | BL_V b -> string_of_bool b
  | NVEC_V v -> string_of_vector_int (List.length v) v
  | FVEC_V v -> string_of_vector_fl (List.length v) v
  | NMAT_V m -> 
      let dim_m = List.length m in
      let dim_n = if dim_m > 0 then List.length (List.hd m) else 0 in
      string_of_matrix_int dim_m dim_n m
  | FMAT_V m ->
      let dim_m = List.length m in
      let dim_n = if dim_m > 0 then List.length (List.hd m) else 0 in
      string_of_matrix_fl dim_m dim_n m
  | FILE_V s -> "INPUT(" ^ s ^ ")"

let string_of_typ t = match t with 
| T_INT -> "integer"
| T_FLOAT -> "float"
| T_BOOL -> "boolean"
| T_VEC_N -> "integer vector"
| T_VEC_F -> "float vector"
| T_MAT_N -> "integer matrix"
| T_MAT_F -> "float matrix"
| T_INP -> "input"

let string_of_etype et = match et with
| E_INT -> "integer"
| E_FLOAT -> "float"
| E_BOOL -> "boolean"
| E_VEC_N dim -> "integer vector[" ^ string_of_int dim ^ "]"
| E_VEC_F dim -> "float vector[" ^ string_of_int dim ^ "]"
| E_MAT_N (rows, cols) -> "integer matrix[" ^ string_of_int rows ^ "," ^ string_of_int cols ^ "]"
| E_MAT_F (rows, cols) -> "float matrix[" ^ string_of_int rows ^ "," ^ string_of_int cols ^ "]"
| E_INP -> "input"

let string_of_binop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | And -> "and"
  | Or -> "or"
  | Eq -> "="
  | Neq -> "!="
  | Gt -> ">"
  | Lt -> "<"
  | Geq -> ">="
  | Leq -> "<="
  | Dot_Prod -> "dot_prod"
  | Angle -> "angle"
  | Add_Vec -> "add_v"
  | Scal_Vec -> "scal_v"
  | Add_Mat -> "add_m"
  | Scal_Mat -> "scal_m"
  | Mat_Mul_Mat -> "mat_mul"

(* Convert unary operator to string *)
let string_of_unop op =
  match op with
  | Not -> "not"
  | Neg -> "-"
  | Mag_v -> "mag_v"
  | Dim -> "dim"
  | Trp_Mat -> "trp_mat"
  | Det -> "det"
  | Inv -> "inv"

let rec string_of_exp e = match e with
  | IDF s -> s
  | VAL v -> string_of_value v
  | BIN_OP (op, e1, e2) ->
      "(" ^ string_of_exp e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_exp e2 ^ ")"
  | UN_OP (op, e) ->
      "(" ^ string_of_unop op ^ " " ^ string_of_exp e ^ ")"
  | COND (e1, e2, e3) ->
      "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3
  | Input None -> "input()"
  | Input (Some s) -> "input(\"" ^ s ^ "\")"

(* For converting statements into strings *)
let rec string_of_stmt = function
  | Assign (_, name, expr) ->
      name ^ " := " ^ string_of_exp expr
  | Ifte (cond, then_branch, Some else_branch) ->
      "if " ^ string_of_exp cond ^ " then " ^ string_of_stmt then_branch ^ " else " ^ string_of_stmt else_branch
  | Ifte (cond, then_branch, None) ->
      "if " ^ string_of_exp cond ^ " then " ^ string_of_stmt then_branch
  | While (cond, body) ->
      "while " ^ string_of_exp cond ^ " do " ^ string_of_stmt body
  | For (init, cond, update, body) ->
      "for (" ^ string_of_stmt init ^ "; " ^ string_of_exp cond ^ "; " ^ string_of_stmt update ^ ") do " ^ string_of_stmt body
  | Print expr ->
      "print " ^ string_of_exp expr
  | Return expr ->
      "return " ^ string_of_exp expr
  | Block stmts -> 
    "{\n" ^ 
    (String.concat "\n" (List.map string_of_stmt stmts)) ^ 
    "\n}"
  | Break -> "break;"
  | Continue -> "continue;"
  
(* Converts a list of statements (the program) to string *)
let string_of_program stmts =
  String.concat "\n" (List.map string_of_stmt stmts)
