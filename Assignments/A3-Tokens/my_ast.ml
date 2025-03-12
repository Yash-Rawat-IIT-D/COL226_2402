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
  | Dot_Prod | Angle 

(* Unary Operators *)
  
type un_op = Not | Neg 
  | Magn | Dim | Transp | Det | Inv 

(* *)
type exp =  
  | IDF of string
  | VAL of value
  | BIN_OP of bin_op * exp * exp
  | UN_OP of un_op * exp
  | COND of exp * exp * exp

type stmt =
  | Assign of string * exp
  | Ifte of exp * stmt * stmt option
  | While of exp * stmt
  | For of string * exp * exp * exp * stmt (* Variable , Initialisation, Condition and Update *)
  | Return of exp
  | Break
  | Continue
  | Input of string option * string (* Assigning some variable value in current state of program *)
  | Print of exp
  | Blk of stmt list  (* One of the most important ideas of a program *)


