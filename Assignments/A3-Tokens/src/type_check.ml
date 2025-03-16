open My_ast
open Env

(*===================================================================================*)
                        (* Exception for Type Mismatch *)
(*===================================================================================*)

exception Type_Error of string
exception Undefined_Var of string
(*===================================================================================*)
                     (* Exception for Type Check of AST's *)
(*===================================================================================*)

let rec type_of_expr env = function
  (* Base cases *)
  | VAL (INT_V _) -> T_INT
  | VAL (FLT_V _) -> T_FLOAT
  | VAL (BL_V _) -> T_BOOL
  | VAL (NVEC_V _) -> T_VEC_N
  | VAL (FVEC_V _) -> T_VEC_F
  | VAL (NMAT_V _) -> T_MAT_N
  | VAL (FMAT_V _) -> T_MAT_F

  (* Identifier lookup *)
  | IDF v ->
      (try
         let (typ, _) = lookup_var v env in
         typ
       with Var_Not_Found _ -> raise (UndefinedVariable ("Variable " ^ v ^ " not defined")))

  (* Binary Operations *)
  | BIN_OP (Add, e1, e2) -> 
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_INT
       | T_FLOAT, T_FLOAT -> T_FLOAT
       | _ -> raise (Type_Error "Type mismatch in binary addition"))

  | BIN_OP (Sub, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_INT
       | T_FLOAT, T_FLOAT -> T_FLOAT
       | _ -> raise (Type_Error "Type mismatch in binary subtraction"))

  | BIN_OP (Mul, e1, e2) -> 
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_INT
       | T_FLOAT, T_FLOAT -> T_FLOAT
       | _ -> raise (Type_Error "Type mismatch in binary multiplication"))

  | BIN_OP (Div, e1, e2) -> 
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_INT   (* Integer division *)
       | T_FLOAT, T_FLOAT -> T_FLOAT
       | _ -> raise (Type_Error "Type mismatch in binary division"))

  | BIN_OP (Modulo, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_INT
       | _ -> raise (Type_Error "Modulo only allowed for integer types"))

  (* Binary Logical Operations *)
  | BIN_OP (And, e1, e2) ->
      if type_of_expr env e1 = T_BOOL && type_of_expr env e2 = T_BOOL then T_BOOL
      else raise (Type_Error "Type mismatch in binary AND")

  | BIN_OP (Or, e1, e2) ->
      if type_of_expr env e1 = T_BOOL && type_of_expr env e2 = T_BOOL then T_BOOL
      else raise (Type_Error "Type mismatch in binary OR")

  (* Binary Comparison Operations (ONLY FOR INTEGERS) *)
  | BIN_OP (Eq, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_BOOL
       | _ -> raise (Type_Error "Equality comparison allowed only for integer types"))

  | BIN_OP (Neq, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_BOOL
       | _ -> raise (Type_Error "Inequality comparison allowed only for integer types"))

  | BIN_OP (Geq, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_BOOL
       | _ -> raise (Type_Error "Invalid types for '>=' comparison - Only integers allowed"))

  | BIN_OP (Leq, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_BOOL
       | _ -> raise (Type_Error "Invalid types for '<=' comparison - Only integers allowed"))

  | BIN_OP (Gt, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_BOOL
       | _ -> raise (Type_Error "Invalid types for '>' comparison - Only integers allowed"))

  | BIN_OP (Lt, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_INT, T_INT -> T_BOOL
       | _ -> raise (Type_Error "Invalid types for '<' comparison - Only integers allowed"))
  (* Vector and Matrix OperatT_VEC_N ions *)
  | BIN_OP (Add_Vec, e1, e2) ->
    let t1 = type_of_expr env e1 in
    let t2 = type_of_expr env e2 in
    (match t1, t2 with
     | T_VEC_N , T_VEC_N -> T_VEC_F
     | T_VEC_F, T_VEC_F -> T_VEC_F
     | _ -> raise (Type_Error "Invalid types for scalar-vector addition"))
  (* Vector and Matrix Operations *)
  | BIN_OP (Scal_Vec, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_FLOAT, T_VEC_N -> T_VEC_F
       | T_FLOAT, T_VEC_F -> T_VEC_F
       | _ -> raise (Type_Error "Invalid types for scalar-vector multiplication"))

  | BIN_OP (Dot_Prod, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_VEC_N, T_VEC_N -> T_FLOAT
       | T_VEC_F, T_VEC_F -> T_FLOAT
       | _ -> raise (Type_Error "Invalid types for dot product"))

  | BIN_OP (Angle, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
       | T_VEC_N, T_VEC_N -> T_FLOAT
       | T_VEC_F, T_VEC_F -> T_FLOAT
       | _ -> raise (Type_Error "Invalid types for angle calculation"))

  | BIN_OP (Add_Mat, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
        | T_MAT_N , T_MAT_N -> T_MAT_F
        | T_MAT_F, T_MAT_F -> T_MAT_F
        | _ -> raise (Type_Error "Invalid types for scalar-matrix addition"))
  
  | BIN_OP (Scal_Mat, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
        | T_FLOAT, T_MAT_N -> T_MAT_F
        | T_FLOAT, T_MAT_F -> T_MAT_F
        | _ -> raise (Type_Error "Invalid types for scalar-matrix multiplication"))
  | BIN_OP (Mat_Mul_Mat, e1, e2) ->
      let t1 = type_of_expr env e1 in
      let t2 = type_of_expr env e2 in
      (match t1, t2 with
        | T_MAT_N , T_MAT_N -> T_MAT_F
        | T_MAT_F, T_MAT_F -> T_MAT_F
        | _ -> raise (Type_Error "Invalid types for matrix-matrix addition"))

  (* Unary Operations *)
  | UN_OP (Not, e) ->
      if type_of_expr env e = T_BOOL then T_BOOL
      else raise (Type_Error "Invalid type for NOT")

  | UN_OP (Neg, e) ->
      (match type_of_expr env e with
       | T_INT -> T_INT
       | T_FLOAT -> T_FLOAT
       | _ -> raise (Type_Error "Invalid type for NEG"))

  | UN_OP (Mag_v, e) ->
      (match type_of_expr env e with
       | T_VEC_N -> T_FLOAT
       | T_VEC_F -> T_FLOAT
       | _ -> raise (Type_Error "Invalid type for magnitude"))

  | UN_OP (Dim, e) ->
      (match type_of_expr env e with
       | T_VEC_N -> T_INT
       | T_VEC_F -> T_INT
       | _ -> raise (Type_Error "Invalid type for dimension"))

  | UN_OP (Transp, e) ->
      (match type_of_expr env e with
        | T_MAT_N -> T_MAT_N
        | T_MAT_F -> T_MAT_F
        | _ -> raise (Type_Error "Invalid type for matrix transpose"))
    
  | UN_OP (Det, e) ->
      (match type_of_expr env e with
        | T_MAT_N -> T_INT
        | T_MAT_F -> T_FLOAT
        | _ -> raise (Type_Error "Invalid type for matrix determinant"))
  
  | UN_OP (Inv, e) ->
      (match type_of_expr env e with
        | T_MAT_N -> T_MAT_F
        | T_MAT_F -> T_MAT_F
        | _ -> raise (Type_Error "Invalid type for matrix inverse"))
  (* Handling Conditional Branches *)
  | COND (e1, e2, e3) ->
    let t1 = type_of_expr env e1 in
    let t2 = type_of_expr env e2 in
    let t3 = type_of_expr env e3 in
    if t1 = T_BOOL then
      if t2 = t3 then t2
      else raise (Type_Error "Branches of conditional must have the same type")
    else
      raise (Type_Error "Condition must be of type BOOL")
	| Input _ -> T_INP