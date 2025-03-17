(*===================================================================================*)
            (* Program Specifications - COL226 Assignment 3 - 2023CS50334 *)  
              (* prog.ml - Handles variable scoping and type checking *)
(*===================================================================================*)



(* prog.ml - Implements the type-checking and variable environment/scoping *)

open My_ast
open My_lexer

(*===================================================================================*)

(* We will be using stack based approach to simulate the scoping principle of variables *)

type env_frame = (string * typ * value) 
type environment = env_frame list

(*===================================================================================*)

(*===================================================================================*)
                    (* Handling Variable Type and Value Lookup *)
(*===================================================================================*)
let rec lookup_var v_name env =   
  match env with 
  | [] -> raise(Var_Not_Found("Variable Not Found : " ^ v_name))
  | eframe::xenv ->
    (
      match List.find_opt (fun (v,_,_) -> v = v_name) eframe with 
      | Some (_,v_typ,v_val) -> (v_typ,v_val)
      | None -> lookup_var v_name xenv  (* Looks in older / Outer Scopes *)
    )

(*===================================================================================*)
                (* Updating or adding new variable to current scope *)
(*===================================================================================*)    

let define_var v_name v_typ v_val env = 
  match env with
  | [] -> raise(Empty_Env("Empty Environment : No Scope"))
  | eframe::xenv ->
    (
      match List.find_opt (fun (v, _, _) -> v = v_name) eframe with
      | Some (_, old_typ, _) ->
          if old_typ = v_typ then
            (* Update with same type *)
            let new_frame = 
              List.map (fun (v, t, _) -> if v = v_name then (v, t, v_val) else (v, t, v_val)) eframe
            in
            new_frame :: xenv
          else
            raise (Type_Error("Cannot redefine variable " ^ v_name ^ " with different type"))
      | None ->
          (* Add new binding *)
          let new_frame = (v_name, v_typ, v_val) :: eframe in
          new_frame :: xenv
    )

let rec update_var v_name new_v_val env =
  match env with 
  | [] -> raise(Var_Not_Found("Variable Not Found : " ^ v_name))
  | eframe::xenv ->
    (
      if List.exists (fun (v, _, _) -> v = v_name) eframe then
        let new_frame =
          List.map
            (fun (v, v_typ, v_val) -> if v = v_name then (v, v_typ, new_v_val) else (v, v_typ, v_val))
            eframe
        in
        new_frame :: xenv
      else
        eframe :: update_var v_name new_v_val env
    )

(* ------------------------- Scope Handling ------------------------- *)

(* Pushing Onto Current Scope *)
let push_scope env =
  [] :: env

(* Popping Current Scope *)
let pop_scope env =
  match env with
  | _ :: rest -> rest
  | [] -> failwith "No scope to pop on stack"

  (* Initialize the environment *)
let init_env () = [[]]

(*===================================================================================*)
                        (* Type Checking and Eval_Expr Implementation *)
(*===================================================================================*)


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
    | CONS_VN (dim, vec), Some T_VEC_N -> VAL (NVEC_V vec)
    | CONS_VF (dim, vec), Some T_VEC_F -> VAL (FVEC_V vec)
    | CONS_MN (rows, cols, mat), Some T_MAT_N -> VAL (NMAT_V mat)
    | CONS_MF (rows, cols, mat), Some T_MAT_F -> VAL (FMAT_V mat)
    | _, None -> raise (Type_Error "Type annotation required for input")
    | _ -> raise (Type_Error ("Input doesn't match expected type"))

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
       | Add, INT_V i1, INT_V i2 -> INT_V (i1 + i2)
       | Add, FLT_V f1, FLT_V f2 -> FLT_V (f1 +. f2)
       | Sub, INT_V i1, INT_V i2 -> INT_V (i1 - i2)
       | Sub, FLT_V f1, FLT_V f2 -> FLT_V (f1 -. f2)
       | Mul, INT_V i1, INT_V i2 -> INT_V (i1 * i2)
       | Mul, FLT_V f1, FLT_V f2 -> FLT_V (f1 *. f2)
       | Div, INT_V i1, INT_V i2 -> 
           if i2 = 0 then raise (Division_By_Zero "Integer division by zero")
           else INT_V (i1 / i2)
       | Div, FLT_V f1, FLT_V f2 -> 
           if f2 = 0.0 then raise (Division_By_Zero "Float division by zero")
           else FLT_V (f1 /. f2)
       | Modulo, INT_V i1, INT_V i2 -> 
           if i2 = 0 then raise (Division_By_Zero "Modulo by zero")
           else INT_V (i1 mod i2)
       | And, BL_V b1, BL_V b2 -> BL_V (b1 && b2)
       | Or, BL_V b1, BL_V b2 -> BL_V (b1 || b2)
       | Eq, INT_V i1, INT_V i2 -> BL_V (i1 = i2)
       | Neq, INT_V i1, INT_V i2 -> BL_V (i1 <> i2)
       | Lt, INT_V i1, INT_V i2 -> BL_V (i1 < i2)
       | Gt, INT_V i1, INT_V i2 -> BL_V (i1 > i2)
       | Leq, INT_V i1, INT_V i2 -> BL_V (i1 <= i2)
       | Geq, INT_V i1, INT_V i2 -> BL_V (i1 >= i2)
       | _ -> raise (Type_Error "Type mismatch in binary operation"))

  (* Unary operations *)
  | UN_OP (op, e) ->
      let v = eval_expr env e in
      (match op, v with
       | Not, BL_V b -> BL_V (not b)
       | Neg, INT_V i -> INT_V (-i)
       | Neg, FLT_V f -> FLT_V (-.f)
       | Mag_v, NVEC_V vec -> 
           (* Calculate magnitude of integer vector *)
           let sum_of_sq = List.fold_left (fun acc x -> acc + x * x) 0 vec in
           FLT_V (sqrt (float_of_int sum_of_sq))
       | Mag_v, FVEC_V vec -> 
           (* Calculate magnitude of float vector *)
           let sum_of_sq = List.fold_left (fun acc x -> acc +. x *. x) 0.0 vec in
           FLT_V (sqrt sum_of_sq)
       | Dim, NVEC_V vec -> INT_V (List.length vec)
       | Dim, FVEC_V vec -> INT_V (List.length vec)
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
      | None -> VAL (FILE_V "")
      | Some fname -> VAL (FILE_V fname)
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
  | VAL (FILE_V s) -> 
  | IDF v ->
    (
      try
        let (typ, _) = lookup_var v env in
        typ
      with Var_Not_Found _ -> raise (Undefined_Var ("Variable " ^ v ^ " not defined"))
    )
  (* Binary Operations *)
  | BIN_OP (Add, e1, e2) -> 
    let t1 = type_of_exp env e1 in
    let t2 = type_of_exp env e2 in
    (match t1, t2 with
     | E_INT, E_INT -> E_INT
     | E_INT, E_FLOAT | E_FLOAT, E_INT | E_FLOAT, E_FLOAT -> E_FLOAT
     | _ -> raise (Type_Error "Type mismatch in binary addition"))

  | BIN_OP (Sub, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT -> E_INT
      | E_INT, E_FLOAT | E_FLOAT, E_INT | E_FLOAT, E_FLOAT -> E_FLOAT
      | _ -> raise (Type_Error "Type mismatch in binary subtraction"))

  | BIN_OP (Mul, e1, e2) -> 
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT -> E_INT
      | E_INT, E_FLOAT | E_FLOAT, E_INT | E_FLOAT, E_FLOAT -> E_FLOAT
      | _ -> raise (Type_Error "Type mismatch in binary multiplication"))

  | BIN_OP (Div, e1, e2) -> 
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT -> E_INT
      | E_INT, E_FLOAT | E_FLOAT, E_INT | E_FLOAT, E_FLOAT -> E_FLOAT
      | _ -> raise (Type_Error "Type mismatch in binary division"))

  | BIN_OP (Modulo, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT -> E_INT
      | _ -> raise (Type_Error "Modulo only allowed for integer types"))

  (* Logical Operations *)
  | BIN_OP (And, e1, e2) | BIN_OP (Or, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      if t1 = E_BOOL && t2 = E_BOOL then E_BOOL
      else raise (Type_Error "Logical operations require boolean operands")

  (* Comparison Operations *)
  | BIN_OP (Eq, e1, e2) | BIN_OP (Neq, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT | E_FLOAT, E_FLOAT -> E_BOOL
      | _ -> raise (Type_Error "Equality comparison requires operands of the same numeric type"))

  | BIN_OP (Lt, e1, e2) | BIN_OP (Gt, e1, e2) | BIN_OP (Leq, e1, e2) | BIN_OP (Geq, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_INT | E_FLOAT, E_FLOAT -> E_BOOL
      | _ -> raise (Type_Error "Comparison requires operands of the same numeric type"))

  (* Vector Operations with Dimension Checking *)
  | BIN_OP (Add_Vec, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_VEC_N d1, E_VEC_N d2 when d1 = d2 -> E_VEC_F d1
      | E_VEC_F d1, E_VEC_F d2 when d1 = d2 -> E_VEC_F d1
      | E_VEC_N d1, E_VEC_N d2 -> 
          raise (Dimension_Mismatch ("Vector addition requires equal dimensions: " ^ 
                  string_of_int d1 ^ " vs " ^ string_of_int d2))
      | E_VEC_F d1, E_VEC_F d2 -> 
          raise (Dimension_Mismatch ("Vector addition requires equal dimensions: " ^ 
                  string_of_int d1 ^ " vs " ^ string_of_int d2))
      | _ -> raise (Type_Error "Invalid types for vector addition"))

  | BIN_OP (Dot_Prod, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_VEC_N d1, E_VEC_N d2 when d1 = d2 -> E_FLOAT
      | E_VEC_F d1, E_VEC_F d2 when d1 = d2 -> E_FLOAT
      | E_VEC_N d1, E_VEC_N d2 -> 
          raise (Dimension_Mismatch ("Dot product requires equal dimensions: " ^ 
                  string_of_int d1 ^ " vs " ^ string_of_int d2))
      | E_VEC_F d1, E_VEC_F d2 -> 
          raise (Dimension_Mismatch ("Dot product requires equal dimensions: " ^ 
                  string_of_int d1 ^ " vs " ^ string_of_int d2))
      | _ -> raise (Type_Error "Invalid types for dot product"))

  | BIN_OP (Angle, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_VEC_N d1, E_VEC_N d2 when d1 = d2 -> E_FLOAT
      | E_VEC_F d1, E_VEC_F d2 when d1 = d2 -> E_FLOAT
      | E_VEC_N d1, E_VEC_N d2 -> 
          raise (Dimension_Mismatch ("Angle calculation requires equal dimensions: " ^ 
                  string_of_int d1 ^ " vs " ^ string_of_int d2))
      | E_VEC_F d1, E_VEC_F d2 -> 
          raise (Dimension_Mismatch ("Angle calculation requires equal dimensions: " ^ 
                  string_of_int d1 ^ " vs " ^ string_of_int d2))
      | _ -> raise (Type_Error "Invalid types for angle calculation"))

  | BIN_OP (Scal_Vec, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_VEC_N d -> E_VEC_N d
      | E_FLOAT, E_VEC_N d -> E_VEC_F d
      | E_INT, E_VEC_F d -> E_VEC_F d
      | E_FLOAT, E_VEC_F d -> E_VEC_F d
      | _ -> raise (Type_Error "Invalid types for scalar-vector multiplication"))

  (* Matrix Operations with Dimension Checking *)
  | BIN_OP (Add_Mat, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_MAT_N (r1, c1), E_MAT_N (r2, c2) when r1 = r2 && c1 = c2 -> E_MAT_F (r1, c1)
      | E_MAT_F (r1, c1), E_MAT_F (r2, c2) when r1 = r2 && c1 = c2 -> E_MAT_F (r1, c1)
      | E_MAT_N (r1, c1), E_MAT_N (r2, c2) -> 
          raise (Dimension_Mismatch ("Matrix addition requires equal dimensions: " ^ 
                  string_of_int r1 ^ "x" ^ string_of_int c1 ^ " vs " ^ 
                  string_of_int r2 ^ "x" ^ string_of_int c2))
      | E_MAT_F (r1, c1), E_MAT_F (r2, c2) -> 
          raise (Dimension_Mismatch ("Matrix addition requires equal dimensions: " ^ 
                  string_of_int r1 ^ "x" ^ string_of_int c1 ^ " vs " ^ 
                  string_of_int r2 ^ "x" ^ string_of_int c2))
      | _ -> raise (Type_Error "Invalid types for matrix addition"))

  | BIN_OP (Scal_Mat, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_INT, E_MAT_N (r, c) -> E_MAT_N (r, c)
      | E_FLOAT, E_MAT_N (r, c) -> E_MAT_F (r, c)
      | E_INT, E_MAT_F (r, c) -> E_MAT_F (r, c)
      | E_FLOAT, E_MAT_F (r, c) -> E_MAT_F (r, c)
      | _ -> raise (Type_Error "Invalid types for scalar-matrix multiplication"))

  | BIN_OP (Mat_Mul_Mat, e1, e2) ->
      let t1 = type_of_exp env e1 in
      let t2 = type_of_exp env e2 in
      (match t1, t2 with
      | E_MAT_N (r1, c1), E_MAT_N (r2, c2) when c1 = r2 -> E_MAT_F (r1, c2)
      | E_MAT_F (r1, c1), E_MAT_F (r2, c2) when c1 = r2 -> E_MAT_F (r1, c2)
      | E_MAT_N (r1, c1), E_MAT_N (r2, c2) -> 
          raise (Dimension_Mismatch ("Matrix multiplication requires compatible dimensions: " ^ 
                  string_of_int c1 ^ " columns in first matrix vs " ^ 
                  string_of_int r2 ^ " rows in second matrix"))
      | E_MAT_F (r1, c1), E_MAT_F (r2, c2) -> 
          raise (Dimension_Mismatch ("Matrix multiplication requires compatible dimensions: " ^ 
                  string_of_int c1 ^ " columns in first matrix vs " ^ 
                  string_of_int r2 ^ " rows in second matrix"))
      | _ -> raise (Type_Error "Invalid types for matrix multiplication")) 
  (* Unary Operations *)
  | UN_OP (Not, e) ->
      let t = type_of_exp env e in
      if t = E_BOOL then E_BOOL
      else raise (Type_Error "NOT operation requires a boolean operand")

  | UN_OP (Neg, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_INT -> E_INT
       | E_FLOAT -> E_FLOAT
       | _ -> raise (Type_Error "Negation requires numeric operand"))

  | UN_OP (Mag_v, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_VEC_N _ -> E_FLOAT
       | E_VEC_F _ -> E_FLOAT
       | _ -> raise (Type_Error "Magnitude operation requires vector operand"))

  | UN_OP (Dim, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_VEC_N _ -> E_INT
       | E_VEC_F _ -> E_INT
       | _ -> raise (Type_Error "Dimension operation requires vector operand"))

  | UN_OP (Trp_Mat, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_MAT_N (r, c) -> E_MAT_N (c, r)  (* Transpose swaps rows and columns *)
       | E_MAT_F (r, c) -> E_MAT_F (c, r)
       | _ -> raise (Type_Error "Transpose operation requires matrix operand"))

  | UN_OP (Det, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_MAT_N (r, c) when r = c -> E_INT  (* Determinant requires square matrix *)
       | E_MAT_F (r, c) when r = c -> E_FLOAT
       | E_MAT_N (r, c) -> 
           raise (Dimension_Mismatch ("Determinant requires square matrix, got " ^ 
                   string_of_int r ^ "x" ^ string_of_int c))
       | E_MAT_F (r, c) -> 
           raise (Dimension_Mismatch ("Determinant requires square matrix, got " ^ 
                   string_of_int r ^ "x" ^ string_of_int c))
       | _ -> raise (Type_Error "Determinant operation requires matrix operand"))

  | UN_OP (Inv, e) ->
      let t = type_of_exp env e in
      (match t with
       | E_MAT_N (r, c) when r = c -> E_MAT_F (r, c)  (* Inverse requires square matrix *)
       | E_MAT_F (r, c) when r = c -> E_MAT_F (r, c)
       | E_MAT_N (r, c) -> 
           raise (Dimension_Mismatch ("Matrix inverse requires square matrix, got " ^ 
                   string_of_int r ^ "x" ^ string_of_int c))
       | E_MAT_F (r, c) -> 
           raise (Dimension_Mismatch ("Matrix inverse requires square matrix, got " ^ 
                   string_of_int r ^ "x" ^ string_of_int c))
       | _ -> raise (Type_Error "Inverse operation requires matrix operand"))

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
  
  | _ -> raise(Undefined_Expression("Given expression is undefined"))

(* Statement evaluation function *)
let rec eval_stmt env = function
  | Assign (typ_opt, id, expr) ->
      let value = eval_expr env expr in
      (match value with
        | FILE_V s ->
          ( let input_content = if s = "" then (print_string "> "; flush stdout; read_line()) 
                                else  (let ch = open_in s in let content = input_line ch in close_in ch; content) in
            let processed_val = pseudo_file_lex s typ_opt in
            let expr_type = type_of_exp env processed_val in
            let expected_type = match typ_opt with
              | Some t -> convert_to_etype t
              | None -> expr_type
            in
            if compatible_types expr_type expected_type then 
              define_var id expr_type expected_type
            else
              raise (Type_Error ("Type mismatch in assignment to " ^ id ^ 
                                ": expected " ^ string_of_etype expected_type ^ 
                                " but got " ^ string_of_etype expr_type))    
          )

        | _ ->
        ( let expr_type = type_of_exp env expr in
          let expected_type = match typ_opt with
            | Some t -> convert_to_etype t
            | None -> expr_type
          in
          if compatible_types expr_type expected_type then
            define_var id expr_type value env
          else
            raise (Type_Error ("Type mismatch in assignment to " ^ id ^ 
                              ": expected " ^ string_of_etype expected_type ^ 
                              " but got " ^ string_of_etype expr_type))
        )
      )
  | Print expr ->
      let value = eval_expr env expr in
      print_value value;
      env
      
  | Return expr ->
      (* Store the return value for later use *)
      let value = eval_expr env expr in
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