(* Vector and Matrix Specifications *)
open Token 

(* Exceptions to be used by grammar, accessed via my_ast.ml *)  
exception TypeMismatch of string
exception DimensionMismatch of string
exception InvalidOperation of string
exception UndefinedVariable of string
exception InvalidCondition of string

(*===================================================================================*)
                          (* Vector and Matrix Helper *)
(*===================================================================================*)

let vec_dim vec = List.length vec
let mat_dim mat = match mat with 
  | [] -> raise (DimensionMismatch ("Empty Matrix not allowed"))
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
  if vec_dim v1 <> vec_dim v2 then raise (DimensionMismatch ("Vector dimensions do not match for addition")) 
  else
    List.map2 (+) v1 v2

let add_vec_f v1 v2 =
  if List.length v1 <> List.length v2 then
    raise (DimensionMismatch "Vector dimensions do not match for addition")
  else
    List.map2 (+.) v1 v2

let add_mat_n m1 m2 =
  if List.length m1 <> List.length m2 then
    raise (DimensionMismatch("Matrix row dimensions not same for addition"))
  else if List.length (List.hd m1) <> List.length (List.hd m2) then
    raise (DimensionMismatch("Matrix column dimensions not same for addition"))
  else
    List.map2 (fun r1 r2 -> add_vec_n r1 r2) m1 m2

let add_mat_f m1 m2 =
  if List.length m1 <> List.length m2 then
    raise (DimensionMismatch("Matrix row dimensions not same for addition"))
  else if List.length (List.hd m1) <> List.length (List.hd m2) then
    raise (DimensionMismatch("Matrix column dimensions not same for addition"))
  else
    List.map2 (fun r1 r2 -> add_vec_f r1 r2) m1 m2

(* Values representing the primary data types in our PL *)
(* Otherwise the basecases of our AST are now handled in a separate specification *)

type value = 
  INT_V of int | FLT_V of float | BL_V of bool
  | NVEC_V of int list | FVEC_V of float list 
  | NMAT_V of int list list | FMAT_V of float list list 

(* For handling type of assignments, and runtime checks for type consistency *)
(* Eg : Compatibility of dimensions and types for matrix multiplication *)
type typ = T_INT | T_FLOAT | T_BOOL
        | T_VEC_N | T_FLOAT_N | T_MAT_N | T_MAT_F

(* Majority operations of primary data types as supported by our PL fall in the \
   Category of Unary or binary operations , So to provide another abstract template for our 
   AST expressions, we will have a separate specification for the operators 
   
  Appropriate overloading will be handled at the time of type checking and evaluation
  of the AST's *)

(* Binary Operators *)

type bin_op = Add | Mul | Sub | Div | And | Or
  | Modulo | Eq | Neq | Geq | Leq | Gt | Lt 
  | Dot_Prod | Angle | Scal_Vec | Scal_Mat 

(* Unary Operators *)
  
type un_op = Not | Neg 
  | Mag_v | Dim | Transp | Det | Inv 

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


