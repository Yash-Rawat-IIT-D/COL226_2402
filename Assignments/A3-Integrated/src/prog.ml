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

(*===================================================================================*)

(*===================================================================================*)
                    (* Handling Variable Type and Value Lookup *)
(*===================================================================================*)

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
              raise (Type_Error("Cannot redefine variable " ^ v_name ^ " with incompatible type"))
          else 
            (name, old_typ, old_val)
        ) frame
      in
      new_frame :: rest_env
    else
      (* Add new binding to current frame *)
      ((v_name, v_typ, v_val) :: frame) :: rest_env

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
  | _,_ -> raise (Type_Error "Invalid type of values for Add (Scalar Addition)")

(* Helper function for subtraction *)
let sub_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> INT_V (i1 - i2)
  | FLT_V f1, FLT_V f2 -> FLT_V (f1 -. f2)
  | INT_V i1, FLT_V f2 -> FLT_V ((float_of_int i1) -. f2) (* Supporting type promotion *)
  | FLT_V f1, INT_V i2 -> FLT_V (f1 -. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Sub (Scalar Subtraction)")

(* Helper function for multiplication *)
let mul_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> INT_V (i1 * i2)
  | FLT_V f1, FLT_V f2 -> FLT_V (f1 *. f2)
  | INT_V i1, FLT_V f2 -> FLT_V ((float_of_int i1) *. f2) (* Supporting type promotion *)
  | FLT_V f1, INT_V i2 -> FLT_V (f1 *. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Mul (Scalar Multiplication)")

(* Helper function for division *)
let div_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> 
      if i2 = 0 then raise (Division_by_zero "Integer division by zero")
      else INT_V (i1 / i2)
  | FLT_V f1, FLT_V f2 -> 
      if f2 = 0.0 then raise (Division_by_zero "Float division by zero")
      else FLT_V (f1 /. f2)
  | INT_V i1, FLT_V f2 -> 
      if f2 = 0.0 then raise (Division_by_zero "Float division by zero")
      else FLT_V ((float_of_int i1) /. f2)
  | FLT_V f1, INT_V i2 -> 
      if i2 = 0 then raise (Division_by_zero "Float division by zero")
      else FLT_V (f1 /. (float_of_int i2))
  | _,_ -> raise (Type_Error "Invalid type of values for Div (Scalar Division)")

(* Helper function for modulo *)
let mod_helper val1 val2 = match val1, val2 with
  | INT_V i1, INT_V i2 -> 
      if i2 = 0 then raise (Division_by_zero "Modulo by zero")
      else INT_V (i1 mod i2)
  | _,_ -> raise (Type_Error "Modulo operation only supported for integers")

let add_vec_helper val1 val2 = match val1, val2 with
	| NVEC_V vec1, NVEC_V vec2 -> NVEC_V (add_vec_n vec1 vec2)
	| FVEC_V vec1, FVEC_V vec2 -> FVEC_V (add_vec_f vec1 vec2)
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
	| _, _ -> raise (Type_Error "Invalid types for vector addition")

let angle_helper val1 val2 = match val1, val2 with
	| NVEC_V vec1, NVEC_V vec2 -> FLT_V (angle_vec_n vec1 vec2)
	| FVEC_V vec1, FVEC_V vec2 -> FLT_V (angle_vec_f vec1 vec2)
	| _, _ -> raise (Type_Error "Invalid types for vector addition")		

let mat_mul_mat_helper val1 val2 = match val1 val2 with
	| INT_V s, NMAT_V m -> NMAT_V (scal_n_mat_n s m)
	| FLT_V s, NMAT_V m -> FMAT_V (scal_f_mat_n s m)
	| INT_V s, FMAT_V m -> FMAT_V (scal_n_mat_f s m)
	| FLT_V s, FMAT_V m -> FMAT_V (scal_f_mat_f s m)
	| _, _ -> raise (Type_Error "Invalid types for scalar-matrix multiplication")	

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
  let token = My_lexer.token lexbuf in
  
  (* Check if there's only one token (plus EOF) *)
  let second_token = My_lexer.token lexbuf in
  if second_token <> EOF then
    raise (Type_Error "Invalid input format: expected a single value (Invalid Input format)")
  else
    (* Convert the token to the appropriate value based on the expected type *)
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
    | _ -> raise (Type_Error ("Input doesn't match expected type"))

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
  | _,_ -> raise (Type_Error "Invalid types for matrix multiplication")

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
        | _ -> raise (Type_Error "Type mismatch in binary operation"))

  (* Unary operations *)
  | UN_OP (op, e) ->
      let v = eval_expr env e in
      ( match op, v with
          | Not, BL_V b -> BL_V (not b)
          | Neg, INT_V i -> INT_V (-i)
          | Neg, FLT_V f -> FLT_V (-.f)
          | Mag_v, NVEC_V vec -> FLT_V (mag_vec_n vec)
          | Mag_v, FVEC_V vec -> FLT_V (mag_vec_f vec)
          | Dim, NVEC_V vec -> INT_V (vec_dim vec)
          | Dim, FVEC_V vec -> INT_V (vec_dim vec)
          | Trp_Mat, NMAT_V mat -> NMAT_V (transpose_matrix_n mat)
          | Trp_Mat, FMAT_V mat -> FMAT_V (transpose_matrix_f mat)
          | Det, NMAT_V mat -> INT_V (determinant_n mat)
          | Det, FMAT_V mat -> FLT_V (determinant_f mat)
          | Inv, NMAT_V mat -> FMAT_V (inverse_matrix_n mat)
          | Inv, FMAT_V mat -> FMAT_V (inverse_matrix_f mat)
          | _ -> raise (Type_Error "Type mismatch in unary operation"))
  
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
  | _,_ -> raise(Type_Error("Cross Conversion of Incorrect Types !"))


(* Statement evaluation function with statement level type checking involved *)
let rec eval_stmt env = function
  | Assign (typ_opt, id, expr) ->
      let expr_value = eval_expr env expr in
      (  match expr_value with
          | FILE_V s ->
              (* Handle file input *)
              let input_content = 
                if s = "" then (print_string "> "; flush stdout; read_line()) 
                else (let ch = open_in s in let content = input_line ch in close_in ch; content)
              in
              let processed_exp = pseudo_file_lex input_content typ_opt in
              let processed_exp_type = type_of_exp env processed_exp in
              let processed_val = eval_expr env processed_exp in              
              (* Determine expected type *)
              let expected_type = 
                if var_in_frame id env then
                  (* Get existing type if variable exists *)
                  let (existing_typ, _) = lookup_var id env in
                  existing_typ
                else
                  (* Use explicit type or infer from expression *)
                  match typ_opt with
                  | Some t -> convert_to_etype t processed_val
                  | None -> raise (Type_Error ("Type annotation required for new variable " ^ id))
              in
              
              (* Check type compatibility and define/update variable *)
              if compatible_types processed_exp_type expected_type then
                define_var id expected_type processed_val env
              else
                raise (Type_Error ("Type mismatch in assignment to " ^ id))
                
          | _ ->
              (* Handle normal expr_value *)
              let expr_type = type_of_exp env expr in
              
              (* Determine expected type *)
              let expected_type = 
                if var_in_frame id env then
                  (* Get existing type if variable exists *)
                  let (existing_typ, _) = lookup_var id env in
                  existing_typ
                else
                  (* Use explicit type or infer from expression *)
                  match typ_opt with
                  | Some t -> convert_to_etype t expr_value
                  | None -> raise (Type_Error ("Type annotation required for new variable " ^ id))
              in
              
              (* Check type compatibility and define/update variable *)
              if compatible_types expr_type expected_type then
                define_var id expected_type expr_value env
              else
                raise (Type_Error ("Type mismatch in assignment to " ^ id))
      )        
  | Print expr ->
      let value = eval_expr env expr in
      print_value value;
      env
      
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