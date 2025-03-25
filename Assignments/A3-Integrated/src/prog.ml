(*===================================================================================*)
            (* Program Specifications - COL226 Assignment 3 - 2023CS50334 *)  
              (* prog.ml - Handles variable scoping and type checking *)
(*===================================================================================*)



(* prog.ml - Implements the type-checking and variable environment/scoping *)

open Lexing
open My_ast
open My_lexer
open My_parser

(*===================================================================================*)

(* We will be using stack based approach to simulate the scoping principle of variables *)

type env_frame = (string * etyp * value) list 
type environment = env_frame list

exception File_Error of string
(*===================================================================================*)

(*===================================================================================*)
                    (* Handling Variable Type and Value Lookup *)
(*===================================================================================*)

(* Token Printing *)
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
  | CONS_VN (n, v) -> token_print_vector_int n v
  | CONS_VF (n, v) -> token_print_vector_fl n v
  | CONS_MN (m, n, mat) -> token_print_matrix_int m n mat
  | CONS_MF (m, n, mat) -> token_print_matrix_fl m n mat
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
  | INV -> print_endline "INV"
  | ASSIGN -> print_endline "ASSIGN"
  | IF -> print_endline "IF"
  | THEN -> print_endline "THEN"
  | ELSE -> print_endline "ELSE"
  | ELSE_IF -> print_endline "ELSE_IF"
  | WHILE -> print_endline "WHILE"
  | FOR -> print_endline "FOR"
  | BREAK -> print_endline "BREAK"
  | CONTINUE -> print_endline "CONTINUE"
  | SEMICOLON -> print_endline "SEMICOLON"
  | COLON -> print_endline "COLON"
  | QMARK -> print_endline "QUESTION_MARK"
  | COMMA -> print_endline "COMMA"
  | RETURN -> print_endline "RETURN"
  | EOF -> print_endline "EOF"
  | DEF_MF -> print_endline "DEF_MF"
  | DEF_MN -> print_endline "DEF_MN"
  | DEF_VN -> print_endline "DEF_VN"
  | DEF_VF -> print_endline "DEF_VF"
  | _ -> print_endline "Unrecognized Token"

(* Variable Lookup *)
let rec lookup_var v_name (env : environment) =   
  match env with 
  | [] -> raise(Var_Not_Found("Variable Not Found : " ^ v_name))
  | frame::rest_env ->
    (
      match List.find_opt (fun (name, _, _) -> name = v_name) frame with 
      | Some (_, v_typ, v_val) -> (v_typ, v_val)
      | None -> lookup_var v_name rest_env  (* Look in outer scopes *)
    )

(* Check if variable exists in the current scope only *)
let var_in_frame v_name (env : environment) = 
  match env with
  | [] -> false
  | frame::_ -> List.exists (fun (name, _, _) -> name = v_name) frame

let rec var_in_env v_name (env:environment) = match env with
  | [] -> false
  | frame::rest_frames -> 
    let exists_curr_frame = List.exists (fun (name, _, _) -> name = v_name) frame in
    if exists_curr_frame then
      true
    else
      var_in_env v_name rest_frames

(* Define or update variable in current scope *)
let define_var v_name v_typ v_val (env :environment) = 
  match env with
  | [] -> raise(Empty_Env("Empty Environment: No Scope"))
  | frame::rest_env ->
    let exists = (var_in_frame v_name env) in
    if exists then
      (* Variable exists in current scope, update it with type checking *)
      let new_frame = 
        List.map (fun (name, old_typ, old_val) -> 
          if name = v_name then
            if compatible_types old_typ v_typ 
              then (name, v_typ, v_val)
            else 
              raise (Type_Error("Cannot redefine variable " ^ v_name ^ "of type " 
              ^ (string_of_etype old_typ) ^ " with incompatible type :" ^(string_of_etype v_typ)))
          else 
            (name, old_typ, old_val)
        ) frame
      in
      new_frame :: rest_env
    else
      (* Add new binding to current frame *)
      ((v_name, v_typ, v_val) :: frame) :: rest_env

let rec update_var v_name v_typ v_val (env : environment) = 
  let rec update_var_helper vname vtyp vval = function
  | [] -> raise (Empty_Env "Empty Environment: No Scope , Variable was not found !")
  | frame :: rest_env ->
      let exists = List.exists (fun (name, _, _) -> name = vname) frame in
      if exists then
        (* Variable exists in current scope, update it with type checking *)
        let new_frame = 
          List.map (fun (name, old_typ, old_val) -> 
            if name = vname then
              if compatible_types old_typ vtyp 
                then (name, vtyp, vval)
              else 
                raise (Type_Error("Cannot redefine variable " ^ v_name ^ "of type " 
              ^ (string_of_etype old_typ) ^ " with incompatible type :" ^(string_of_etype v_typ)))
            else 
              (name, old_typ, old_val)
          ) frame
        in
        new_frame :: rest_env
      else
        (* Continue searching in outer scopes *)
        frame :: update_var_helper vname vtyp vval rest_env
  in
    update_var_helper v_name v_typ v_val env

(* ------------------------- Scope Handling ------------------------- *)

(* Pushing Onto Current Scope *)
let push_scope (env : environment) =
  [] :: env

(* Popping Current Scope *)
let pop_scope (env : environment) =
  match env with
  | _ :: rest -> rest
  | [] -> failwith "No scope to pop on stack"

  (* Initialize the environment *)
let init_env () = [[]]

(*===================================================================================*)
        (* Helper Functions for Type-Checking and Eval_Expr Implementation *)
(*===================================================================================*)

(* Helper function for addition *)
let add_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> INT_V (i1 + i2)
  | FLT_V f1, FLT_V f2 -> FLT_V (f1 +. f2)
  | INT_V i1, FLT_V f2 -> FLT_V ((float_of_int i1) +. f2) (* Supporting type promotion *) 
  | FLT_V f1, INT_V i2 -> FLT_V (f1 +. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Add (Scalar Addition) - Only Floats and Integers Supported")

(* Helper function for subtraction *)
let sub_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> INT_V (i1 - i2)
  | FLT_V f1, FLT_V f2 -> FLT_V (f1 -. f2)
  | INT_V i1, FLT_V f2 -> FLT_V ((float_of_int i1) -. f2) (* Supporting type promotion *)
  | FLT_V f1, INT_V i2 -> FLT_V (f1 -. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Sub (Scalar Subtraction) - Only Floats and Integers Supported")

(* Helper function for multiplication *)
let mul_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> INT_V (i1 * i2)
  | FLT_V f1, FLT_V f2 -> FLT_V (f1 *. f2)
  | INT_V i1, FLT_V f2 -> FLT_V ((float_of_int i1) *. f2) (* Supporting type promotion *)
  | FLT_V f1, INT_V i2 -> FLT_V (f1 *. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Mul (Scalar Multiplication) - Only Floats and Integers Supported")

(* Helper function for division *)
let div_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> 
      if i2 = 0 then raise (Division_by_zero "Integer division by zero unallowed")
      else INT_V (i1 / i2)
  | FLT_V f1, FLT_V f2 -> 
      if f2 = 0.0 then raise (Division_by_zero "Float division by zero unallowed")
      else FLT_V (f1 /. f2)
  | INT_V i1, FLT_V f2 -> 
      if f2 = 0.0 then raise (Division_by_zero "Float division by zero unallowed")
      else FLT_V ((float_of_int i1) /. f2)
  | FLT_V f1, INT_V i2 -> 
      if i2 = 0 then raise (Division_by_zero "Float division by zero unallowed")
      else FLT_V (f1 /. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Div (Scalar Division) - Only Floats and Integers Supported ")

(* Helper function for modulo *)
let mod_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> 
      if i2 = 0 then raise (Division_by_zero "Modulo by zero")
      else INT_V (i1 mod i2)
  | _,_ -> raise (Type_Error "Modulo operation only supported for integers")

let add_vec_helper val1 val2 = match val1, val2 with
	| NVEC_V vec1, NVEC_V vec2 -> NVEC_V (add_vec_n vec1 vec2)
	| FVEC_V vec1, FVEC_V vec2 -> FVEC_V (add_vec_f vec1 vec2)
  | NVEC_V vec1, FVEC_V vec2 -> FVEC_V (add_vec_f (vec_int_to_float vec1) vec2)
  | FVEC_V vec1, NVEC_V vec2 -> FVEC_V (add_vec_f vec1 (vec_int_to_float vec2))
	| _, _ -> raise (Type_Error "Invalid types for vector addition")

let add_mat_helper val1 val2 = match val1, val2 with
	| NMAT_V mat1, NMAT_V mat2 -> NMAT_V (add_mat_n mat1 mat2)
	| FMAT_V mat1, FMAT_V mat2 -> FMAT_V (add_mat_f mat1 mat2)
	| _, _ -> raise (Type_Error "Invalid types for matrix addition")

let scal_vec_helper val1 val2 = match val1, val2 with 
	| INT_V s, NVEC_V v -> NVEC_V (scal_n_vec_n s v)
	| FLT_V s, NVEC_V v -> FVEC_V (scal_f_vec_n s v)
	| INT_V s, FVEC_V v -> FVEC_V (scal_n_vec_f s v)
	| FLT_V s, FVEC_V v -> FVEC_V (scal_f_vec_f s v)
	| _, _ -> raise (Type_Error "Invalid types for scalar-vector multiplication") 

let scal_mat_helper val1 val2 = match val1, val2 with
	| INT_V s, NMAT_V m -> NMAT_V (scal_n_mat_n s m)
	| FLT_V s, NMAT_V m -> FMAT_V (scal_f_mat_n s m)
	| INT_V s, FMAT_V m -> FMAT_V (scal_n_mat_f s m)
	| FLT_V s, FMAT_V m -> FMAT_V (scal_f_mat_f s m)
	| _, _ -> raise (Type_Error "Invalid types for scalar-matrix multiplication") 

let dot_prod_helper val1 val2 = match val1, val2 with
	| NVEC_V vec1, NVEC_V vec2 -> INT_V (dot_prod_n vec1 vec2)
	| FVEC_V vec1, FVEC_V vec2 -> FLT_V (dot_prod_f vec1 vec2)
  | NVEC_V vec1, FVEC_V vec2 -> FLT_V (dot_prod_f (vec_int_to_float vec1) vec2)
  | FVEC_V vec1, NVEC_V vec2 -> FLT_V (dot_prod_f vec1 (vec_int_to_float vec2))
	| _, _ -> raise (Type_Error "Invalid types for vector addition")

let angle_helper val1 val2 = match val1, val2 with
	| NVEC_V vec1, NVEC_V vec2 -> FLT_V (angle_vec_n vec1 vec2)
	| FVEC_V vec1, FVEC_V vec2 -> FLT_V (angle_vec_f vec1 vec2)
	| _, _ -> raise (Type_Error "Invalid types for vector angle")		

(* Involves a lot of type promotion *)
let mat_mul_mat_helper val1 val2 = match val1, val2 with
	| NMAT_V m1, NMAT_V m2 -> NMAT_V(mat_mul_mat_n m1 m2)
	| FMAT_V m1, FMAT_V m2 -> FMAT_V(mat_mul_mat_f m1 m2)
  | NMAT_V m1, FMAT_V m2 -> FMAT_V(mat_mul_mat_f (mat_int_to_float m1) m2) 
  | FMAT_V m1, NMAT_V m2 -> FMAT_V(mat_mul_mat_f m1 (mat_int_to_float m2))
  | NMAT_V m1, NVEC_V v1 -> NVEC_V(mat_mul_vec_n m1 v1)
  | FMAT_V m1, FVEC_V v1 -> FVEC_V(mat_mul_vec_f m1 v1)
  | NMAT_V m1, FVEC_V v1 -> FVEC_V(mat_mul_vec_f (mat_int_to_float m1) v1)
  | FMAT_V m1, NVEC_V v1 -> FVEC_V(mat_mul_vec_f m1 (vec_int_to_float v1))
  | NVEC_V v1, NMAT_V m1 -> NVEC_V(vec_mul_mat_n v1 m1)
  | FVEC_V v1, FMAT_V m1 -> FVEC_V(vec_mul_mat_f v1 m1)  
  | NVEC_V v1, FMAT_V m1 -> FVEC_V(vec_mul_mat_f (vec_int_to_float v1) m1)
  | FVEC_V v1, NMAT_V m1 -> FVEC_V(vec_mul_mat_f v1 (mat_int_to_float m1)) 
	| _, _ -> raise (Type_Error "Invalid types for vector/matrix multiplication")	

let x_slice_helper (env:environment) (s : string) (i : int) = 
  let (s_etyp,s_val) = lookup_var s env in
    match s_etyp, s_val with 
      | E_VEC_N n, NVEC_V vec -> INT_V (vec_n_i vec i)
      | E_VEC_F n, FVEC_V vec -> FLT_V (vec_f_i vec i)
      | E_MAT_N (m,n), NMAT_V mat -> NVEC_V (mat_n_i mat i)
      | E_MAT_F (m,n), FMAT_V mat -> FVEC_V (mat_f_i mat i)
      | _,_ -> raise(Type_Error("Invalid Type : Indexing only allowed by matrix and vectors"))
let xy_slice_helper (env:environment) (s : string) (i : int) (j :int) = 
  let (s_etyp,s_val) = lookup_var s env in
    match s_etyp,s_val with 
    | E_MAT_N (m,n), NMAT_V mat -> INT_V (mat_n_i_j mat i j)
    | E_MAT_F (m,n), FMAT_V mat -> FLT_V (mat_f_i_j mat i j)
    | _,_ -> raise(Type_Error("Invalid Type : Double Indexing only allowed by matrix"))

let read_from_terminal () =
  (* Read from stdin and parse into appropriate value *)
  print_string "> ";
  flush stdout;
  let input_line = read_line () in
  input_line

let pseudo_file_lex input_str typ_opt =
  (* Create a lexing buffer from the input string *)
  let lexbuf = Lexing.from_string input_str in
  
  (* Get the first token *)
  let token = My_lexer.token lexbuf in (* No EOF since string parsed first !*)
  match token, typ_opt with
  | CONS_N n, Some T_INT -> VAL (INT_V n)
  | CONS_F f, Some T_FLOAT -> VAL (FLT_V f)
  | CONS_B b, Some T_BOOL -> VAL (BL_V b)
  | CONS_VN (dim, vec), Some T_VEC_N -> 
    if not (vec_dim_check dim vec) then
      raise (Dimension_Mismatch (
        "Expected vector of dimension " ^ string_of_int dim ^
        ", but got dimension " ^ string_of_int (vec_dim vec)
      ))
    else
      VAL (NVEC_V vec)
  | CONS_VF (dim, vec), Some T_VEC_F -> 
      if not (vec_dim_check dim vec) then
        raise (Dimension_Mismatch (
          "Expected vector of dimension " ^ string_of_int dim ^
          ", but got dimension " ^ string_of_int (vec_dim vec)
        ))
      else
        VAL (FVEC_V vec)
  | CONS_MN (rows, cols, mat), Some T_MAT_N -> 
      if not (mat_dim_check rows cols mat) then
        raise (Dimension_Mismatch (
          "Expected matrix with " ^ string_of_int rows ^ 
          " rows and " ^ string_of_int cols ^ 
          " columns, but found incorrect dimensions"
        ))
      else
        VAL (NMAT_V mat)
  | CONS_MF (rows, cols, mat), Some T_MAT_F -> 
      if not (mat_dim_check rows cols mat) then
        raise (Dimension_Mismatch (
          "Expected matrix with " ^ string_of_int rows ^ 
          " rows and " ^ string_of_int cols ^ 
          " columns, but found inconsistent dimensions"
        ))
      else
        VAL (FMAT_V mat)
  | _, None -> raise (Type_Error "Type annotation required for input from a file")
  | EOF,_ -> raise (Type_Error ("Input file might be empty !"))
  | _,_ -> raise (File_Error ("Unexpected Error occured while reading from input file !"))
 
let type_bin_op_num_helper bin_op t1 t2 = match t1 , t2 with 
  | E_INT, E_INT -> E_INT
  | E_FLOAT,E_INT | E_INT, E_FLOAT | E_FLOAT, E_FLOAT -> E_FLOAT
  | _,_ -> raise (Type_Error(err_string_of_binop bin_op))

let type_bin_op_comp_helper bin_op t1 t2 = match t1, t2 with
  | E_INT, E_INT -> E_BOOL
  | _,_ -> raise (Type_Error(err_string_of_binop bin_op))
let type_bin_op_bool_helper bin_op t1 t2 = match t1, t2 with 
  | E_BOOL, E_BOOL -> E_BOOL
  | _,_ -> raise (Type_Error(err_string_of_binop bin_op))

let type_add_vec_helper t1 t2 = match t1, t2 with 
  | E_VEC_N d1, E_VEC_N d2 ->
    ( if(d1 <> d2) 
      then raise (Dimension_Mismatch ("Integer Vector addition requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_VEC_N d1 )
  | E_VEC_F d1, E_VEC_F d2 -> 
    ( if(d1 <> d2) 
      then raise (Dimension_Mismatch ("Float Vector addition requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_VEC_F d1 )
  | E_VEC_N d1, E_VEC_F d2 ->
    ( if(d1 <> d2)
      then raise (Dimension_Mismatch ("Vector addition requires equal dimensions: Found " ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_VEC_F d1 )  (* Promote to float vector *)
  | E_VEC_F d1, E_VEC_N d2 ->
    ( if(d1 <> d2)
      then raise (Dimension_Mismatch ("Vector addition requires equal dimensions: Found " ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_VEC_F d1 )  (* Promote to float vector *)
  | _,_ -> raise (Type_Error (err_string_of_binop Add_Vec))

let type_dot_prod_helper t1 t2 = match t1, t2 with 
  | E_VEC_N d1, E_VEC_N d2 ->
    ( if(d1 <> d2) 
      then raise (Dimension_Mismatch ("Integer Vector dot product requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_INT )
  | E_VEC_F d1, E_VEC_F d2 -> 
    ( if(d1 <> d2) 
      then raise (Dimension_Mismatch ("Float Vector dot product requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_FLOAT)
  | E_VEC_N d1, E_VEC_F d2 ->
    ( if(d1 <> d2)
      then raise (Dimension_Mismatch ("Mixed Vector dot product requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_FLOAT)  (* Promote to float result *)
  | E_VEC_F d1, E_VEC_N d2 ->
      ( if(d1 <> d2)
        then raise (Dimension_Mismatch ("Mixed Vector dot product requires equal dimensions: Found" ^ 
                                          string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_FLOAT)  (* Promote to float result *)
  | _,_ -> raise (Type_Error (err_string_of_binop Dot_Prod))

let type_angle_helper t1 t2 = match t1, t2 with
  | E_VEC_N d1, E_VEC_N d2 ->
    ( if(d1 <> d2) 
      then raise (Dimension_Mismatch ("Integer Vector angle requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_FLOAT)
  | E_VEC_F d1, E_VEC_F d2 -> 
    ( if(d1 <> d2) 
      then raise (Dimension_Mismatch ("Float Vector angle requires equal dimensions: Found" ^ 
                                        string_of_int d1 ^ " , " ^ string_of_int d2))
      else E_FLOAT)
  | _,_ -> raise (Type_Error (err_string_of_binop Angle))

let type_add_mat_helper t1 t2 = match t1, t2 with 
  | E_MAT_N (r1, c1), E_MAT_N (r2, c2) ->
    ( if((r1 <> r2 ||c1 <> c2)) 
      then
        raise (Dimension_Mismatch ("Integer Matrix addition requires equal dimensions: Found " ^ 
              string_of_int r1 ^ "x" ^ string_of_int c1 ^ " , " ^ 
              string_of_int r2 ^ "x" ^ string_of_int c2))   
      else
        E_MAT_N (r1,c1))
  | E_MAT_F (r1, c1), E_MAT_F (r2, c2) ->
    ( if((r1 <> r2 ||c1 <> c2)) 
      then
        raise (Dimension_Mismatch ("Float Matrix addition requires equal dimensions: Found " ^ 
              string_of_int r1 ^ "x" ^ string_of_int c1 ^ " , " ^ 
              string_of_int r2 ^ "x" ^ string_of_int c2))   
      else
        E_MAT_F (r1,c1))
  | _,_ -> raise (Type_Error (err_string_of_binop Add_Mat))

let type_mat_mul_mat_helper t1 t2 = match t1, t2 with
  | E_MAT_N (r1, c1), E_MAT_N (r2, c2) ->
    ( if(r1 <> c2)
      then 
        raise (Dimension_Mismatch ("Integer Matrix multiplication requires compatible dimensions: Found " ^ 
              string_of_int c1 ^ " columns in first matrix and " ^ 
              string_of_int r2 ^ " rows in second matrix"))
      else
        E_MAT_N (r1, c2))
  | E_MAT_F (r1, c1), E_MAT_F (r2, c2) ->
    ( if(r1 <> c2)
      then 
        raise (Dimension_Mismatch ("Float Matrix multiplication requires compatible dimensions: Found " ^ 
              string_of_int c1 ^ " columns in first matrix and " ^ 
              string_of_int r2 ^ " rows in second matrix"))
      else
        E_MAT_F (r1, c2))
  | E_MAT_N (r1, c1), E_MAT_F (r2, c2) ->
      ( if(c1 <> r2)
        then 
          raise (Dimension_Mismatch ("Mixed Matrix multiplication requires compatible dimensions: Found " ^ 
                string_of_int c1 ^ " columns in first matrix and " ^ 
                string_of_int r2 ^ " rows in second matrix"))
        else
          E_MAT_F (r1, c2))  (* Result is promoted to float *)
  | E_MAT_F (r1, c1), E_MAT_N (r2, c2) ->
      ( if(c1 <> r2)
        then 
          raise (Dimension_Mismatch ("Mixed Matrix multiplication requires compatible dimensions: Found " ^ 
                string_of_int c1 ^ " columns in first matrix and " ^ 
                string_of_int r2 ^ " rows in second matrix"))
        else
          E_MAT_F (r1, c2))  (* Result is promoted to float *)
  | E_MAT_N (r1, c1), E_VEC_N d2 ->
    ( if(c1 <> d2)
      then
        raise (Dimension_Mismatch ("Integer Matrix-Vector multiplication requires compatible dimensions: Found " ^ 
              string_of_int c1 ^ " columns in matrix and " ^ 
              string_of_int d2 ^ " elements in vector"))
      else
        E_VEC_N r1)
  | E_MAT_F (r1, c1), E_VEC_F d2 ->
    ( if(c1 <> d2)
      then
        raise (Dimension_Mismatch ("Float Matrix-Vector multiplication requires compatible dimensions: Found " ^ 
              string_of_int c1 ^ " columns in matrix and " ^ 
              string_of_int d2 ^ " elements in vector"))
      else
        E_VEC_F r1)
  | E_MAT_N (r1, c1), E_VEC_F d2 ->
    ( if(c1 <> d2)
      then
        raise (Dimension_Mismatch ("Mixed Matrix-Vector multiplication requires compatible dimensions: Found " ^ 
              string_of_int c1 ^ " columns in matrix and " ^ 
              string_of_int d2 ^ " elements in vector"))
      else
        E_VEC_F r1)  (* Result is promoted to float *)
  | E_MAT_F (r1, c1), E_VEC_N d2 ->
    ( if(c1 <> d2)
      then
        raise (Dimension_Mismatch ("Mixed Matrix-Vector multiplication requires compatible dimensions: Found " ^ 
              string_of_int c1 ^ " columns in matrix and " ^ 
              string_of_int d2 ^ " elements in vector"))
      else
        E_VEC_F r1)  (* Result is promoted to float *)
  | E_VEC_N d1, E_MAT_N (r2, c2) ->
    ( if(d1 <> r2)
      then
        raise (Dimension_Mismatch ("Integer Vector-Matrix multiplication requires compatible dimensions: Found " ^ 
              string_of_int d1 ^ " elements in vector and " ^ 
              string_of_int r2 ^ " rows in matrix"))
      else
        E_VEC_N c2)
  | E_VEC_F d1, E_MAT_F (r2, c2) ->
    ( if(d1 <> r2)
      then
        raise (Dimension_Mismatch ("Float Vector-Matrix multiplication requires compatible dimensions: Found " ^ 
              string_of_int d1 ^ " elements in vector and " ^ 
              string_of_int r2 ^ " rows in matrix"))
      else
        E_VEC_F c2)
  | E_VEC_N d1, E_MAT_F (r2, c2) ->
    ( if(d1 <> r2)
      then
        raise (Dimension_Mismatch ("Mixed Vector-Matrix multiplication requires compatible dimensions: Found " ^ 
              string_of_int d1 ^ " elements in vector and " ^ 
              string_of_int r2 ^ " rows in matrix"))
      else
        E_VEC_F c2)  (* Result is promoted to float *)
  | E_VEC_F d1, E_MAT_N (r2, c2) ->
    ( if(d1 <> r2)
      then
        raise (Dimension_Mismatch ("Mixed Vector-Matrix multiplication requires compatible dimensions: Found " ^ 
              string_of_int d1 ^ " elements in vector and " ^ 
              string_of_int r2 ^ " rows in matrix"))
      else
        E_VEC_F c2)  (* Result is promoted to float *)
  | _,_ -> raise (Type_Error "Invalid types for matrix-vector multiplication")

let type_inv_helper t = match t with
  | E_MAT_N (r, c) ->
    ( if(r <> c)
      then 
        raise (Dimension_Mismatch ("Integer Matrix inverse requires square matrix, got dimensions " ^ 
              string_of_int r ^ "x" ^ string_of_int c))
      else
        E_MAT_F(r,c))
  | E_MAT_F (r, c) -> 
    ( if(r <> c)
      then 
        raise (Dimension_Mismatch ("Float Matrix inverse requires square matrix, got dimensions " ^ 
              string_of_int r ^ "x" ^ string_of_int c))
      else
        E_MAT_F(r,c))
  | _ -> raise (Type_Error (err_string_of_unop Inv))

let type_det_helper t = match t with
 | E_MAT_N (r, c) ->
  ( if(r <> c)
    then 
      raise (Dimension_Mismatch ("Integer Matrix Determinant requires square matrix, got " ^ 
             string_of_int r ^ "x" ^ string_of_int c))
    else
      E_INT)
 | E_MAT_F (r, c) ->
  ( if(r <> c)
    then 
      raise (Dimension_Mismatch ("Float Matrix Determinant requires square matrix, got " ^ 
              string_of_int r ^ "x" ^ string_of_int c))
    else
      E_FLOAT)
 | _ -> raise (Type_Error (err_string_of_unop Det))

let print_environment (env : environment) =
( let print_frame frame index =
    Printf.printf "Frame %d:\n" index;
    List.iter (fun (name, typ, value) ->
      Printf.printf "  %s : %s = " name (string_of_etype typ);
      print_value value;
      print_newline ()
    ) frame
  in

  List.iteri (fun i frame -> 
    print_frame frame i;
    if i < List.length env - 1 then print_newline ()
  ) env )

let type_x_slice_helper (env:environment) (s : string) (e_etyp : etyp) =
  let (s_etyp,s_val) = lookup_var s env in
  ( match s_etyp,e_etyp with 
    | E_VEC_N n, E_INT -> E_INT 
    | E_VEC_F n, E_INT -> E_FLOAT 
    | E_MAT_N (m,n), E_INT -> E_VEC_N n 
    | E_MAT_F (m,n), E_INT -> E_VEC_F n
    | _,_ -> raise(Type_Mismatch("Invalid Type : Integer Indexing only allowed by matrix and vectors"))
  )
let type_xy_slice_helper (env:environment) (s : string) (e1_etyp : etyp) (e2_etyp : etyp)=
  let (s_etyp,s_val) = lookup_var s env in
    ( match s_etyp, e1_etyp, e2_etyp with 
      | E_MAT_N (m,n), E_INT, E_INT -> E_INT 
      | E_MAT_F (m,n), E_INT, E_INT -> E_FLOAT
      | _,_,_ -> raise(Type_Mismatch("Invalid Type : Double Integer Indexing only allowed by matrix"))
    )

(*===================================================================================*)
                  (* Type Checking and Eval_Expr Implementation *)
(*===================================================================================*)
  
let rec eval_expr env = function
  (* Base cases - values evaluate to themselves *)
  | VAL v -> v

  (* Variable lookup - get value from environment *)
  | IDF id -> 
      (try 
         let (_, value) = lookup_var id env in
         value
       with Var_Not_Found _ -> 
         raise (Undefined_Var ("Undefined variable: " ^ id)))
  
  | X_Slice(s,e) ->
    ( let v1 = eval_expr env e in
      match v1 with
        | INT_V i -> x_slice_helper env s i
        | _ -> raise(Type_Error("Indexing Expression must have type : int"))
    )
  
  | XY_Slice(s,e1,e2) ->
    ( let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in 
      match v1,v2 with 
        | INT_V i, INT_V j -> xy_slice_helper env s i j
        | INT_V i, _ -> raise(Type_Error("Indexing Expression for column must have type : int"))
        | _, INT_V j -> raise(Type_Error("Indexing Expression for row must have type : int"))
        | _,_ -> raise(Type_Error("Indexing Expression for row and column must have type : int"))
    )
  (* Binary operations *)
  | BIN_OP (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op, v1, v2 with
        | Add, val1, val2 -> add_helper val1 val2
        | Sub, val1, val2-> sub_helper val1 val2
        | Mul, val1, val2 -> mul_helper val1 val2
        | Div, val1, val2 -> div_helper val1 val2
        | Modulo, val1, val2 -> mod_helper val1 val2
        | And, BL_V b1, BL_V b2 -> BL_V (b1 && b2)
        | Or, BL_V b1, BL_V b2 -> BL_V (b1 || b2)
        | Eq, INT_V i1, INT_V i2 -> BL_V (i1 = i2)
        | Neq, INT_V i1, INT_V i2 -> BL_V (i1 <> i2)
        | Lt, INT_V i1, INT_V i2 -> BL_V (i1 < i2)
        | Gt, INT_V i1, INT_V i2 -> BL_V (i1 > i2)
        | Leq, INT_V i1, INT_V i2 -> BL_V (i1 <= i2)
        | Geq, INT_V i1, INT_V i2 -> BL_V (i1 >= i2)
				| Add_Vec, val1, val2 -> add_vec_helper val1 val2
				| Scal_Vec, val1, val2 -> scal_vec_helper val1 val2
				| Add_Mat, val1, val2 -> add_mat_helper val1 val2
				| Scal_Mat, val1, val2 -> scal_mat_helper val1 val2
        | Mat_Mul_Mat, val1,val2 -> mat_mul_mat_helper val1 val2
        | Dot_Prod, val1, val2 -> dot_prod_helper val1 val2
        | Angle, val1, val2 -> angle_helper val1 val2
        | Def_mn, INT_V m, INT_V n -> NMAT_V((def_mat_n m n))
        | Def_mf, INT_V m, INT_V n -> FMAT_V((def_mat_f m n))
        | _ -> 
          let _ = print_environment env in
          raise (Type_Error("Type mismatch in binary operation - "^string_of_binop op^" "^string_of_exp e1^" "^string_of_exp e2)))

  (* Unary operations *)
  | UN_OP (op, e) ->
      let v = eval_expr env e in
      ( match op, v with
          | Not, BL_V b -> BL_V (not b)
          | Neg, INT_V i -> INT_V (-i)
          | Neg, FLT_V f -> FLT_V (-.f)
          | Abs, INT_V i -> INT_V (abs i)
          | Abs, FLT_V f -> FLT_V (abs_float f) 
          | Mag_v, NVEC_V vec -> FLT_V (mag_vec_n vec)
          | Mag_v, FVEC_V vec -> FLT_V (mag_vec_f vec)
          | Dim, NVEC_V vec -> INT_V (vec_dim vec)
          | Dim, FVEC_V vec -> INT_V (vec_dim vec)
          | Dim, NMAT_V mat -> let (m,_) = mat_dim(mat) in INT_V (m)
          | Dim, FMAT_V mat -> let (m,_) = mat_dim(mat) in INT_V (m)
          | Trp_Mat, NMAT_V mat -> NMAT_V (transpose_matrix_n mat)
          | Trp_Mat, FMAT_V mat -> FMAT_V (transpose_matrix_f mat)
          | Det, NMAT_V mat -> INT_V (determinant_n mat)
          | Det, FMAT_V mat -> FLT_V (determinant_f mat)
          | Inv, NMAT_V mat -> FMAT_V (inverse_matrix_n mat)
          | Inv, FMAT_V mat -> FMAT_V (inverse_matrix_f mat)
          | Def_vn, INT_V dim -> NVEC_V(def_vec_n dim)
          | Def_vf, INT_V dim -> FVEC_V(def_vec_f dim)
          | _ -> raise (Type_Error "Type mismatch in unary operation or eval not implemented"))
  
  (* Conditional expression *)
  | COND (cond, then_expr, else_expr) ->
      (match eval_expr env cond with
       | BL_V true -> eval_expr env then_expr
       | BL_V false -> eval_expr env else_expr
       | _ -> raise (Type_Error "Condition must evaluate to a boolean"))

  (* Input handling (simplified) *)
  | Input fname_opt -> 
    (
      match fname_opt with 
      | None -> FILE_V ""
      | Some fname -> FILE_V fname
    )
 
(* Helper function to check type compatibility *)
let type_defmat_n_helper (env:environment) (e1 : exp) (e2 : exp) = 
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env  e2 in
  match v1,v2 with 
  | INT_V m, INT_V n -> (
    match m,n with 
    | 0,_ -> raise (Type_Error("def_mn : Row Dimension of matrix should be positive")) 
    | _,0 -> raise (Type_Error("def_mn : Column Dimension of matrix should be positive"))
    | row,col -> (E_MAT_N(row,col))
  )
  | _,_ -> raise (Type_Error("def_mn only accepts integer expressions as input")) 
    
let type_defmat_f_helper (env: environment) (e1:exp) (e2:exp) = 
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env  e2 in
  match v1,v2 with 
  | INT_V m, INT_V n -> (
    match m,n with 
    | 0,_ -> raise (Type_Error("def_mf : Row Dimension of matrix should be positive")) 
    | _,0 -> raise (Type_Error("def_mf : Column Dimension of matrix should be positive"))
    | row,col -> (E_MAT_F(row,col))
  )
  | _,_ -> raise (Type_Error("def_mf only accepts integer expressions as input"))

let type_defvec_n_helper (env:environment) (e:exp) = 
  let v = eval_expr env e in
  match v with 
  | INT_V m -> (
    match m with
    | 0 -> raise (Type_Error("def_vn : Row Dimension of matrix should be positive"))
    | dim -> E_VEC_N dim  
    )
  | _ -> raise (Type_Error("def_vn : Only accepts integer expressions as input"))

let type_defvec_f_helper(env:environment) (e:exp) =
  let v = eval_expr env e in 
  match v with 
  | INT_V m -> (
    match m with
    | 0 -> raise (Type_Error("def_vf : Row Dimension of matrix should be positive"))
    | dim -> E_VEC_F dim  
    )
  | _ -> raise (Type_Error("def_vn
  f : Only accepts integer expressions as input"))

let rec type_of_exp env = function
  (* Base cases *)
  | VAL (INT_V _) -> E_INT
  | VAL (FLT_V _) -> E_FLOAT
  | VAL (BL_V _) -> E_BOOL
  | VAL (NVEC_V v) -> E_VEC_N (vec_dim v)
  | VAL (FVEC_V v) -> E_VEC_F (vec_dim v)
  | VAL (NMAT_V m) -> 
      let (rows, cols) = mat_dim m in
      E_MAT_N (rows, cols)
  | VAL (FMAT_V m) -> 
      let (rows, cols) = mat_dim m in
      E_MAT_F (rows, cols)
  | IDF v -> (
      try
        let (typ, _) = lookup_var v env in
        typ
      with Var_Not_Found _ -> raise (Undefined_Var ("Variable " ^ v ^ " not defined"))
    )
  | X_Slice(s,e) ->
      let t = type_of_exp env e in
      type_x_slice_helper env s t
  | XY_Slice(s,e1,e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      type_xy_slice_helper env s t1 t2
  (* Binary Operations *)
  | BIN_OP (Add, e1, e2) -> 
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Add in 
      type_bin_op_num_helper b_op t1 t2

  | BIN_OP (Sub, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Sub in 
      type_bin_op_num_helper b_op t1 t2

  | BIN_OP (Mul, e1, e2) -> 
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Sub in 
      type_bin_op_num_helper b_op t1 t2

  | BIN_OP (Div, e1, e2) -> 
    let t1 = type_of_exp env e1 in
    let t2 = type_of_exp env e2 in
    let b_op = Div in 
    type_bin_op_num_helper b_op t1 t2 

  | BIN_OP (Modulo, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT -> E_INT
      | _ -> raise (Type_Error (err_string_of_binop Modulo)))

  (* Logical Operations *)
  | BIN_OP (And, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = And in
      type_bin_op_bool_helper b_op t1 t2

  | BIN_OP (Or, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Or in
      type_bin_op_bool_helper b_op t1 t2

  (* Comparison Operations *)
  | BIN_OP (Eq, e1, e2) | BIN_OP (Neq, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Eq in
      type_bin_op_comp_helper b_op t1 t2

  | BIN_OP (Lt, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Lt in
      type_bin_op_comp_helper b_op t1 t2 
  | BIN_OP (Gt, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Gt in
      type_bin_op_comp_helper b_op t1 t2
  | BIN_OP (Leq, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Leq in
      type_bin_op_comp_helper b_op t1 t2
  | BIN_OP (Geq, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      let b_op = Geq in
      type_bin_op_comp_helper b_op t1 t2
  (* Vector Operations with Dimension Checking *)
  | BIN_OP (Add_Vec, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      type_add_vec_helper t1 t2

  | BIN_OP (Dot_Prod, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      type_dot_prod_helper t1 t2

  | BIN_OP (Angle, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      type_angle_helper t1 t2

  | BIN_OP (Scal_Vec, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_VEC_N d -> E_VEC_N d
      | E_FLOAT, E_VEC_N d -> E_VEC_F d
      | E_INT, E_VEC_F d -> E_VEC_F d
      | E_FLOAT, E_VEC_F d -> E_VEC_F d
      | _ -> raise (Type_Error (err_string_of_binop Scal_Vec)))

  (* Matrix Operations with Dimension Checking *)
  | BIN_OP (Add_Mat, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      type_add_mat_helper t1 t2

  | BIN_OP (Scal_Mat, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
        | E_INT, E_MAT_N (r, c) -> E_MAT_N (r, c)
        | E_FLOAT, E_MAT_N (r, c) -> E_MAT_F (r, c)
        | E_INT, E_MAT_F (r, c) -> E_MAT_F (r, c)
        | E_FLOAT, E_MAT_F (r, c) -> E_MAT_F (r, c)
        | _ -> raise (Type_Error (err_string_of_binop Scal_Mat)))

  | BIN_OP (Mat_Mul_Mat, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      type_mat_mul_mat_helper t1 t2 

  | BIN_OP (Def_mn, e1, e2) ->(
      type_defmat_n_helper env e1 e2
    )

  | BIN_OP (Def_mf, e1, e2) ->(
      type_defmat_f_helper env e1 e2
  )
  (* Unary Operations *)
  | UN_OP (Not, e) ->
      let t = type_of_exp env e in
      if t = E_BOOL then E_BOOL
      else raise (Type_Error (err_string_of_unop Not))

  | UN_OP (Neg, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_INT -> E_INT
       | E_FLOAT -> E_FLOAT
       | _ -> raise (Type_Error (err_string_of_unop Neg)))

  | UN_OP (Mag_v, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_VEC_N n -> E_FLOAT
       | E_VEC_F f -> E_FLOAT
       | _ -> raise (Type_Error (err_string_of_unop Mag_v)))

  | UN_OP (Dim, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_VEC_N n -> E_INT
       | E_VEC_F f -> E_INT
       | E_MAT_F (m,n) -> E_INT
       | E_MAT_N (m,n) -> E_INT
       | _ -> raise (Type_Error (err_string_of_unop Dim)))

  | UN_OP (Trp_Mat, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_MAT_N (r, c) -> E_MAT_N (c, r)  (* Transpose swaps rows and columns *)
       | E_MAT_F (r, c) -> E_MAT_F (c, r)
       | _ -> raise (Type_Error (err_string_of_unop Trp_Mat)))

  | UN_OP (Det, e) ->
      let t = type_of_exp env e in
      type_det_helper t

  | UN_OP (Inv, e) ->
      let t = type_of_exp env e in
      type_inv_helper t
  
  | UN_OP (Abs, e) ->
      let t = type_of_exp env e in 
      ( match t with
          | E_INT -> E_INT
          | E_FLOAT -> E_FLOAT
          | _ -> raise (Type_Error(err_string_of_unop Abs)))
  
  | UN_OP (Def_vn,e) -> (
    type_defvec_n_helper env e
  )

  (* Conditional Expression *)
  | COND (cond, then_expr, else_expr) ->
      let t_cond = type_of_exp env cond in
      if t_cond <> E_BOOL then
        raise (Type_Error "Condition must be boolean")
      else
        let t_then = type_of_exp env then_expr in
        let t_else = type_of_exp env else_expr in
        if t_then = t_else then t_then
        else raise (Type_Error "Both branches of conditional must have the same type")

  | Input _ -> E_INP
  
  | _ -> raise(Undefined_Expression("Given expression is undefined or unimplemented in type checking of exp"))

let convert_to_etype exp_typ exp_val = match exp_typ, exp_val with
  | T_INT  , INT_V _ -> E_INT
  | T_FLOAT, FLT_V _ -> E_FLOAT
  | T_BOOL , BL_V _ -> E_BOOL
  | T_VEC_N, NVEC_V v -> E_VEC_N (vec_dim v)
  | T_VEC_F, FVEC_V v -> E_VEC_F (vec_dim v)
  | T_MAT_N, NMAT_V m -> let (row,col) = mat_dim m in E_MAT_N(row,col)
  | T_MAT_F, FMAT_V m -> let (row,col) = mat_dim m in E_MAT_F(row,col)
  | _,_ -> raise(Type_Error("Cross Conversion of Incorrect Types ! " ^( string_of_type (def_typ_val exp_val))^ " to " ^(string_of_type exp_typ)))

let sl_assign_helper (env:environment) (e1:exp) (e2_etyp : etyp) (e2_val : value) = 
  match e1 with 
  | X_Slice(s,e) -> 
    (
      if not (var_in_env s env)
      then raise (Var_Not_Found("Indexing called on Undefined variable " ^ s))
      else
        let _ = type_of_exp env e in (* Type Check before eval *)
        let i_val = eval_expr env e in
        match i_val with 
          | INT_V i ->  ( let (lhs_etyp,lhs_val) = lookup_var s env in
                          let var_frame_check = var_in_frame s env in
                          let new_val = (match lhs_etyp,lhs_val,e2_etyp,e2_val with
                                          | E_VEC_N n1 , NVEC_V vec, E_INT, INT_V new_val -> NVEC_V (update_vec_n_i vec new_val i)
                                          | E_VEC_F n1 , FVEC_V vec, E_INT, INT_V new_val -> FVEC_V (update_vec_f_i vec (float_of_int new_val) i)
                                          | E_VEC_F n1 , FVEC_V vec, E_FLOAT, FLT_V new_vaL ->   FVEC_V (update_vec_f_i vec new_vaL i)
                                          | E_MAT_N(m1,n1), NMAT_V mat, E_VEC_N d, NVEC_V new_row -> 
                                            ( if (d = n1) then  NMAT_V (update_mat_n_i mat new_row i)
                                              else raise(Dimension_Mismatch("Trying to assing a row of dimension "^string_of_int d^
                                                        " to a int matrix of dimension ["^string_of_int m1^","^string_of_int n1^"]"))
                                            )
                                          | E_MAT_F(m1,n1), FMAT_V mat, E_VEC_N d, NVEC_V new_row ->
                                            ( if (d = n1) then FMAT_V (update_mat_f_i mat (vec_int_to_float new_row) i)
                                              else raise(Dimension_Mismatch("Trying to assing a row of dimension "^string_of_int d^
                                                        " to a float matrix of dimension ["^string_of_int m1^","^string_of_int n1^"]")) 
                                            ) 
                                          | E_MAT_F(m1,n1), FMAT_V mat, E_VEC_F d, FVEC_V new_row ->
                                            ( if (d = n1) then FMAT_V (update_mat_f_i mat new_row i)
                                              else raise(Dimension_Mismatch("Trying to assing a row of dimension "^string_of_int d^
                                                        " to a float matrix of dimension ["^string_of_int m1^","^string_of_int n1^"]")) 
                                            )
                                          | _,_,_,_ -> raise(Type_Error("Invalid Indexing Assignment - Values Mismatch"))) in
                          if(var_frame_check)
                          then define_var s lhs_etyp new_val env
                          else update_var s lhs_etyp new_val env
                        )   
          | _ -> raise(Type_Error("Indexing Expression must evaluate to an int"))
    )
  | XY_Slice(s,e1,e2) -> 
    (
      if not (var_in_env s env)
        then raise (Var_Not_Found("Indexing called on Undefined variable " ^ s))
        else
          let (_,_) = (type_of_exp env e1, type_of_exp env e2) in (* Type Check before evaluation *)
          let i_val = eval_expr env e1 in
          let j_val = eval_expr env e2 in
          match i_val,j_val with
          | INT_V i, INT_V j -> ( let (lhs_etyp,lhs_val) = lookup_var s env in
                                  let var_frame_check = var_in_frame s env in
                                  let new_val = (match lhs_etyp,lhs_val,e2_etyp,e2_val with
                                                  | E_MAT_N(m1,n1), NMAT_V mat, E_INT, INT_V new_val -> NMAT_V (update_mat_n_i_j mat new_val i j)
                                                    
                                                  | E_MAT_F(m1,n1), FMAT_V mat, E_FLOAT, FLT_V new_val -> FMAT_V (update_mat_f_i_j mat new_val i j)
                                                  | _,_,_,_ -> raise(Type_Error("Invalid Indexing Assignment - Values Mismatch"))) in
                                  if(var_frame_check)
                                  then define_var s lhs_etyp new_val env
                                  else update_var s lhs_etyp new_val env
                                )
          | _,INT_V j -> raise(Type_Error("Row Indexing Expression must evaluate to an int"))
          | INT_V i, _ -> raise(Type_Error("Column Indexing Expression must evaluate to an int"))
          | _,_ -> raise(Type_Error("Row and Column Indexing Expression must evaluate to an int")) 

    )
  | _ -> raise(Type_Error("sl_assign called by a non matrix/vector indexed type expression !")) 
  
(* Statement evaluation function with statement level type checking involved *)
let rec eval_stmt env = function
  | Assign (typ_opt, id, expr) ->
      let expr_type = type_of_exp env expr in (* Type Check before evaluation *)
      let expr_value = eval_expr env expr in
      (  match expr_value with
          | FILE_V s ->
              (* Handle file input *)
              let input_content = 
                if s = "" then 
                  (print_string "====> "; flush stdout; read_line()) 
                else 
                  (let ch = open_in s in
                   let rec read_all acc =
                     try
                       let line = input_line ch in
                       read_all (acc ^ line ^ "\n")
                     with End_of_file ->
                       close_in ch;
                       acc
                   in
                   let content = read_all "" in
                   if content = "" then "" else content)
              in
              let processed_exp = pseudo_file_lex input_content typ_opt in
              let processed_exp_type = type_of_exp env processed_exp in (* Again Type Check before evaluation *)
              let processed_val = eval_expr env processed_exp in              
              (* Determine expected type *)
              let var_exists = var_in_env id env in
              let var_env_check = var_in_env id env in
              let var_frame_check = var_in_frame id env in
              let expected_type = 
                if var_exists then
                  let (existing_typ, _) = lookup_var id env in
                  existing_typ
                else
                  match typ_opt with
                  | Some t -> convert_to_etype t processed_val
                  | None -> raise (Type_Error ("Type annotation required for new variable " ^ id))
              in
              if compatible_types processed_exp_type expected_type then
                if typ_opt <> None then
                  (* This is a variable declaration with type annotation - always create in current scope *)
                  define_var id expected_type processed_val env
                else if var_frame_check then
                  (* Variable exists in current frame - update it there *)
                  define_var id expected_type processed_val env
                else if var_env_check then
                  (* Variable exists in outer scope - update it there *)
                  update_var id expected_type processed_val env
                else
                  (* New variable without type annotation - not allowed *)
                  raise (Type_Error ("Type annotation required for new variable " ^ id))
              else
                raise (Type_Error ("Type mismatch in assignment to " ^ id))
                
          | _ ->
            let var_exists = var_in_env id env in
            let var_env_check = var_in_env id env in
            let var_frame_check = var_in_frame id env in
            let expected_type = 
              if var_exists then
                let (existing_typ, _) = lookup_var id env in
                existing_typ
              else
                match typ_opt with
                | Some t -> convert_to_etype t expr_value
                | None -> raise (Type_Error ("Type annotation required for new variable " ^ id))
            in
            if compatible_types expr_type expected_type then
              if typ_opt <> None then
                (* This is a variable declaration with type annotation - always create in current scope *)
                define_var id expected_type expr_value env
              else if var_frame_check then
                (* Variable exists in current frame - update it there *)
                define_var id expected_type expr_value env
              else if var_env_check then
                (* Variable exists in outer scope - update it there *)
                update_var id expected_type expr_value env
              else
                (* New variable without type annotation - not allowed *)
                raise (Type_Error ("Type annotation required for new variable " ^ id))
            else
              raise (Type_Error ("Type mismatch in assignment to " ^ id))
      )        
  | Print expr ->
      let value = eval_expr env expr in
      print_value value;
      env
  | Sl_Assign(e1,e2) ->
    (
      let e2_typ = type_of_exp env e2 in
      let e2_val = eval_expr env e2 in 
      sl_assign_helper env e1 e2_typ  e2_val
    ) 
  | Return expr ->
      (* Store the return value for later use *)
      (* No Function Calls so not needed *)
      (* let value = eval_expr env expr in *)
      env
      
  | Break | Continue ->
      env
      
  | Block stmts ->
      let block_env = push_scope env in
      let final_env = List.fold_left 
        (fun env stmt -> eval_stmt env stmt) 
        block_env stmts in
      pop_scope final_env
        
  | Ifte (cond, then_stmt, else_opt) ->
      let cond_val = eval_expr env cond in
     (  match cond_val with
          | BL_V true -> eval_stmt env then_stmt
          | BL_V false -> 
              (match else_opt with
              | Some else_stmt -> eval_stmt env else_stmt
              | None -> env)
          | _ -> raise (Type_Error "Condition must evaluate to a boolean")
     )
     
  | While (cond, body) ->
      let rec loop env =
        let cond_val = eval_expr env cond in
        match cond_val with
        | BL_V true -> 
            let new_env = eval_stmt env body in
            loop new_env
        | BL_V false -> env
        | _ -> raise (Type_Error "Condition must evaluate to a boolean")
      in
      loop env
      
  | For (init, cond, update, body) ->
      let init_env = eval_stmt env init in
      let rec loop env =
        let cond_val = eval_expr env cond in
        match cond_val with
        | BL_V true -> 
            let body_env = eval_stmt env body in
            let update_env = eval_stmt body_env update in
            loop update_env
        | BL_V false -> env
        | _ -> raise (Type_Error "Condition must evaluate to a boolean")
      in
      loop init_env
  
(* Evaluate a complete program *)
let eval_prog prog =
  let env = init_env () in
  let rec eval_stmts env = function
    | [] -> env
    | stmt :: rest ->
        let new_env = eval_stmt env stmt in
        eval_stmts new_env rest
  in
  eval_stmts env prog