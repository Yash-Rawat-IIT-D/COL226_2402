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
exception Non_Invertible_Mat of string
(*===================================================================================*)
                          (* Vector and Matrix Helper *)
(*===================================================================================*)

let vec_dim vec = List.length vec
let mat_dim mat = match mat with 
  | [] -> raise (Dimension_Mismatch ("Empty Matrix not allowed"))
  | row::_ -> (List.length mat, List.length row)

let vec_dim_check exp_dim vec =
  List.length vec = exp_dim

let vec_i vec i =   
  let dim = vec_dim vec in
  if i < 0 || i >= dim then
    raise (Dimension_Mismatch ("Vector index out of bounds: " ^ string_of_int i ^ " is not in range [0," ^ string_of_int (dim-1) ^ "]"))
  else
    List.nth vec i

let vec_n_i (vec:vector_int) (i:int) = 
  try
    vec_i vec i
  with
  | Dimension_Mismatch _ -> let dim = vec_dim vec in raise(Dimension_Mismatch("Invalid Indexing into int vector of dimension : " 
  ^ string_of_int dim ^ ", access index provided :" ^ string_of_int i))
let vec_f_i (vec:vector_fl) (i:int) = 
  try
    vec_i vec i
  with
  | Dimension_Mismatch _ -> let dim = vec_dim vec in raise(Dimension_Mismatch("Invalid Indexing into float vector of dimension : " 
  ^ string_of_int dim ^ ", access index provided :" ^ string_of_int i))

let mat_dim_check exp_rows exp_cols mat =
  let rows = List.length mat in
  if rows <> exp_rows then false
  else
    let cols_matched = List.fold_left (fun acc row -> acc && (List.length row = exp_cols)) true mat in
    cols_matched

let mat_i mat i = 
  let (row,_) = mat_dim mat in
  if i < 0 || i >= row then
    raise (Dimension_Mismatch ("Matrix index for Row out of bounds: " ^ string_of_int i ^ " is not in range [0," ^ string_of_int (row-1) ^ "]"))
  else
    List.nth mat i
let mat_n_i (mat : matrix_int) (i:int) = 
  try 
    mat_i mat i
  with
    | Dimension_Mismatch _ -> let (row,col) = mat_dim mat in raise(Dimension_Mismatch("Invalid Row Indexing into int matrix of dimensions : [" 
    ^ string_of_int row ^ "," ^ string_of_int col ^ "], access row index provided :" ^ string_of_int i))
let mat_f_i (mat : matrix_fl) (i:int) = 
  try 
    mat_i mat i
  with
    | Dimension_Mismatch _ -> let (row,col) = mat_dim mat in raise(Dimension_Mismatch("Invalid Row Indexing into float matrix of dimensions : [" 
    ^ string_of_int row ^ "," ^ string_of_int col ^ "], access row index provided :" ^ string_of_int i))

let mat_i_j mat i j = 
  let (row,col) = mat_dim mat in
  if j < 0 || j >= col then
    raise (Dimension_Mismatch ("Matrix column index out of bounds: " ^ string_of_int j ^ " is not in range [0," ^ string_of_int (col-1) ^ "]"))
  else
    if i < 0 ||  i>= row then
      raise (Dimension_Mismatch ("Matrix row index out of bounds: " ^ string_of_int i ^ " is not in range [0," ^ string_of_int (row-1) ^ "]"))
    else
      List.nth (List.nth mat i) j

let mat_n_i_j (mat : matrix_int) (i:int) (j:int) = 
  try
    mat_i_j mat i j
  with
  | Dimension_Mismatch s -> 
    let (m,n) = mat_dim mat in
    raise(Dimension_Mismatch("Invalid Indexing into int matrix of dimensions : [" 
    ^ string_of_int m ^ "," ^ string_of_int n ^ "], row index provided :" ^ string_of_int i))
let mat_f_i_j (mat : matrix_fl) (i:int) (j:int) = 
  try 
    mat_i_j mat i j
  with
  | Dimension_Mismatch s -> 
    let (m,n) = mat_dim mat in
    raise(Dimension_Mismatch("Invalid Indexing into float matrix of dimensions : [" 
    ^ string_of_int m ^ "," ^ string_of_int n ^ "], row index provided :" ^ string_of_int i)) 
let update_vec_i dest_vec value i =
  let len = List.length dest_vec in
  if i < 0 || i >= len then
    raise (Dimension_Mismatch ("Vector index out of bounds: " ^ string_of_int i ^ 
                               " is not in range [0," ^ string_of_int (len - 1) ^ "]"))
  else
    List.mapi (fun j x -> if j = i then value else x) dest_vec
  
let update_vec_n_i (dest_vec : vector_int) (value: int) (i:int) = update_vec_i dest_vec value i
let update_vec_f_i (dest_vec : vector_fl) (value: float) (i:int) = update_vec_i dest_vec value i
let update_mat_i dest_mat row i =
  let row_count = List.length dest_mat in
  if i < 0 || i >= row_count then
    raise (Dimension_Mismatch ("Matrix row index out of bounds: " ^ string_of_int i ^ 
                               " is not in range [0," ^ string_of_int (row_count - 1) ^ "]"))
  else
    List.mapi (fun j r -> if j = i then row else r) dest_mat
let update_mat_n_i (dest_mat : matrix_int) (row: vector_int) (i:int) = update_mat_i dest_mat row i 
let update_mat_f_i (dest_mat : matrix_fl) (row: vector_fl) (i:int) = update_mat_i dest_mat row i 

let update_mat_i_j dest_mat value i j =
  let row_count = List.length dest_mat in
  if i < 0 || i >= row_count then
    raise (Dimension_Mismatch ("Matrix row index out of bounds: " ^ string_of_int i ^ 
                               " is not in range [0," ^ string_of_int (row_count - 1) ^ "]"))
  else
    let row = List.nth dest_mat i in
    let col_count = List.length row in
    if j < 0 || j >= col_count then
      raise (Dimension_Mismatch ("Matrix column index out of bounds: " ^ string_of_int j ^ 
                                 " is not in range [0," ^ string_of_int (col_count - 1) ^ "]"))
    else
      let new_row = List.mapi (fun k x -> if k = j then value else x) row in
      List.mapi (fun k r -> if k = i then new_row else r) dest_mat

let update_mat_n_i_j (dest_mat : matrix_int) (value: int) (i:int) (j:int) = update_mat_i_j dest_mat value i j
let update_mat_f_i_j (dest_mat : matrix_fl) (value: float) (i:int) (j:int) = update_mat_i_j dest_mat value i j 
let mag_vec_n v = let sum_of_sq = List.fold_left (fun acc x -> acc + x * x) 0 v in
                  sqrt (float_of_int sum_of_sq)

let mag_vec_f v = let sum_of_sq = List.fold_left (fun acc x -> acc +. x *. x) 0.0 v in
                  sqrt sum_of_sq

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

(* Vector scalar multiplication *)
let scal_n_vec_n scalar vec =
  List.map (fun x -> scalar * x) vec

let scal_f_vec_n scalar vec =
  List.map (fun x -> scalar *. (float_of_int x)) vec
    
let scal_f_vec_f scalar vec =
  List.map (fun x -> scalar *. x) vec

let scal_n_vec_f scalar vec = 
  scal_f_vec_f (float_of_int scalar) vec

(* Matrix scalar multiplication *)
let scal_n_mat_n scalar mat =
  List.map (fun row -> scal_n_vec_n scalar row) mat

let scal_f_mat_n scalar mat =
  List.map (fun row -> scal_f_vec_n scalar row) mat
    
let scal_f_mat_f scalar mat =
  List.map (fun row -> scal_f_vec_f scalar row) mat

let scal_n_mat_f scalar mat = 
  scal_f_mat_f (float_of_int scalar) mat

let dot_prod_n v1 v2 =
  if vec_dim v1 <> vec_dim v2 then 
    raise (Dimension_Mismatch "Vector dimensions do not match for dot product")
  else
    List.fold_left (+) 0 (List.map2 ( * ) v1 v2)
  
let dot_prod_f v1 v2 =
  if vec_dim v1 <> vec_dim v2 then 
    raise (Dimension_Mismatch "Vector dimensions do not match for dot product")
  else
    List.fold_left (+.) 0.0 (List.map2 ( *. ) v1 v2)

let angle_vec_n v1 v2 =
  if vec_dim v1 <> vec_dim v2 then 
    raise (Dimension_Mismatch "Vector dimensions do not match for angle calculation")
  else
    let dot = float_of_int (dot_prod_n v1 v2) in
    let mag1 = sqrt (float_of_int (dot_prod_n v1 v1)) in
    let mag2 = sqrt (float_of_int (dot_prod_n v2 v2)) in
    if mag1 = 0.0 || mag2 = 0.0 then
      raise (Division_by_zero "Cannot calculate angle with zero vector")
    else
      acos (dot /. (mag1 *. mag2))

let angle_vec_f v1 v2 =
  if vec_dim v1 <> vec_dim v2 then 
    raise (Dimension_Mismatch "Vector dimensions do not match for angle calculation")
  else
    let dot = dot_prod_f v1 v2 in
    let mag1 = sqrt (dot_prod_f v1 v1) in
    let mag2 = sqrt (dot_prod_f v2 v2) in
    if mag1 = 0.0 || mag2 = 0.0 then
      raise (Division_by_zero "Cannot calculate angle with zero vector")
    else
      acos (dot /. (mag1 *. mag2))

(* Transpose a matrix - works for both integer and float matrices *)
let transpose_matrix mat =
  if mat = [] then []
  else
    let (rows, cols) = mat_dim mat in
    List.init cols (fun col ->
      List.init rows (fun row ->
        List.nth (List.nth mat row) col
      )
    )
(* Aliases for type-spec operations *)
let transpose_matrix_n = transpose_matrix
let transpose_matrix_f = transpose_matrix

(* Matrix multiplication *)
let mat_mul_mat_n m1 m2 =
  let (rows1, cols1) = mat_dim m1 in
  let (rows2, cols2) = mat_dim m2 in
  
  if cols1 <> rows2 then
    raise (Dimension_Mismatch "Matrix dimensions incompatible for multiplication")
  else
    let m2_t = transpose_matrix m2 in
    List.map (fun row1 ->
      List.map (fun col2 ->
        List.fold_left (+) 0 (List.map2 ( * ) row1 col2)
      ) m2_t
    ) m1

let mat_mul_mat_f m1 m2 =
  let (rows1, cols1) = mat_dim m1 in
  let (rows2, cols2) = mat_dim m2 in
  
  if cols1 <> rows2 then
    raise (Dimension_Mismatch "Matrix dimensions incompatible for multiplication")
  else
    let m2_t = transpose_matrix m2 in
    List.map (fun row1 ->
      List.map (fun col2 ->
        List.fold_left (+.) 0.0 (List.map2 ( *. ) row1 col2)
      ) m2_t
    ) m1
let submatrix mat row col =
  mat
  |> List.mapi (fun i r -> 
                  if i <> row then 
                    r |> List.filteri (fun j _ -> j <> col) 
                  else [] ) 
  |> List.filter (fun r -> r <> [])
(* Aliases for type-spec operations *)
let submatrix_n = submatrix
let submatrix_f = submatrix

let rec determinant_n mat =
  let (rows, cols) = mat_dim mat in
  
  (* Check if square matrix *)
  if rows <> cols then
    raise (Dimension_Mismatch "Determinant requires square matrix")
  else 
    match rows with
    | 0 -> raise (Dimension_Mismatch "Empty matrix has no determinant")
    | 1 -> List.hd (List.hd mat) (* 1x1 matrix *)
    | 2 -> (match mat with
            |[[a; b]; [c; d]] -> a * d - b * c
            | _ -> raise(Dimension_Mismatch("Expected Two By Two Matrix in the case"))
            )
    | n ->
        (* For larger matrices, use cofactor expansion along first row *)
        let first_row = List.hd mat in
        
        (* Calculate determinant using cofactor expansion *)
        List.mapi (fun j element ->
          let sign = if j mod 2 = 0 then 1 else -1 in
          let sub = submatrix mat 0 j in
          sign * element * determinant_n sub
        ) first_row
        |> List.fold_left (+) 0

(* Determinant calculation for float matrices *)
let rec determinant_f mat =
  let (rows, cols) = mat_dim mat in
  
  (* Check if square matrix *)
  if rows <> cols then
    raise (Dimension_Mismatch "Determinant requires square matrix")
  else 
    match rows with
    | 0 -> raise (Dimension_Mismatch "Empty matrix has no determinant")
    | 1 -> List.hd (List.hd mat) (* 1x1 matrix *)
    | 2 -> (match mat with
              |[[a; b]; [c; d]] -> a *. d -. b *. c
              | _ -> raise(Dimension_Mismatch("Expected Two By Two Matrix in the case"))
          )
    | n ->
        (* For larger matrices, use cofactor expansion along first row *)
        let first_row = List.hd mat in
        
        (* Calculate determinant using cofactor expansion *)
        List.mapi (fun j element ->
          let sign = if j mod 2 = 0 then 1.0 else -1.0 in
          let sub = submatrix mat 0 j in
          sign *. element *. determinant_f sub
        ) first_row
        |> List.fold_left (+.) 0.0

(* Calculate the cofactor of a matrix element mat[i][j] *)
let cofactor_n mat i j =
  let sub = submatrix mat i j in
  let det = determinant_n sub in
  if (i + j) mod 2 = 0 then det else -1 * det

let cofactor_f mat i j =
  let sub = submatrix mat i j in
  let det = determinant_f sub in
  if (i + j) mod 2 = 0 then det else -1.0 *. det

(* Calculate the adjoint of a matrix *)
let adjoint_n mat =
  let n = List.length mat in
  List.init n (fun i ->
    List.init n (fun j ->
      cofactor_n mat j i  (* Note: j,i instead of i,j for transpose *)
    )
  )

let adjoint_f mat =
  let n = List.length mat in
  List.init n (fun i ->
    List.init n (fun j ->
      cofactor_f mat j i  (* Note: j,i instead of i,j for transpose *)
    )
  )

(* Calculate inverse of a matrix using cofactor method *)
let inverse_matrix_n mat =
  let n = List.length mat in
  if n <> List.length (List.hd mat) then
    raise (Dimension_Mismatch "Inverse requires square matrix")
  else 
    let det = determinant_n mat in
    if det = 0 then
      raise (Division_by_zero "Matrix is not invertible (determinant is zero)")
    else
      let adj = adjoint_n mat in
      List.map (fun row ->
        List.map (fun elem -> float_of_int elem /. float_of_int det) row
      ) adj

let inverse_matrix_f mat =
  let n = List.length mat in
  if n <> List.length (List.hd mat) then
    raise (Dimension_Mismatch "Inverse requires square matrix")
  else 
    let det = determinant_f mat in
    if det = 0.0 then
      raise (Division_by_zero "Matrix is not invertible (determinant is zero)")
    else
      let adj = adjoint_f mat in
      List.map (fun row ->
        List.map (fun elem -> elem /. det) row
      ) adj      

(*===================================================================================*)
      (* Values, Operators types and ex-tended types in My Programming Language *)
(*===================================================================================*)

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
  | X_Slice of string * exp
  | XY_Slice of string * exp * exp
  | VAL of value
  | BIN_OP of bin_op * exp * exp
  | UN_OP of un_op * exp
  | COND of exp * exp * exp

type stmt =
  | Assign of typ option * string * exp
  | Sl_Assign of exp * exp
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
                      (* String and Print Utility Functions *)
(*===================================================================================*)

let print_vector_fl dim vec (as_token : bool) =
  let elements = String.concat ", " (List.map string_of_float vec) in
  if(as_token)
  then Printf.printf "CONS_VF(%d [%s])\n" dim elements
  else Printf.printf "%d [%s]\n" dim elements

let print_matrix_fl dim_m dim_n mat (as_token : bool) =
      let rows = List.map (fun row -> 
        let elements = String.concat ", " (List.map string_of_float row) in
        Printf.sprintf "[%s]" elements
      ) mat in
      if (as_token)
      then Printf.printf "CONS_MF(%d %d [%s])\n" (dim_m) (dim_n) (String.concat ", " rows) 
      else Printf.printf "%d %d [%s]\n" (dim_m) (dim_n) (String.concat ", " rows) 
let print_vector_int dim vec (as_token : bool) =
  let elements = String.concat ", " (List.map string_of_int vec) in
  if (as_token)
  then Printf.printf "CONS_VN(%d [%s])\n" dim elements
  else Printf.printf "%d [%s]\n" dim elements 

let print_matrix_int dim_m dim_n mat (as_token : bool) =
    let rows = List.map (fun row -> 
      let elements = String.concat ", " (List.map string_of_int row) in
      Printf.sprintf "[%s]" elements
    ) mat in
    if (as_token)
    then Printf.printf "CONS_MN(%d %d [%s])\n" (dim_m) (dim_n) (String.concat ", " rows)
    else Printf.printf "%d %d [%s]\n" (dim_m) (dim_n) (String.concat ", " rows)

(* Aliasing for printing required in the program and during debugging *)
let pretty_print_vector_fl dim vec = print_vector_fl dim vec false
let token_print_vector_fl dim vec = print_vector_fl dim vec true

let pretty_print_vector_int dim vec = print_vector_int dim vec false
let token_print_vector_int dim vec = print_vector_int dim vec true
let pretty_print_matrix_fl dim_m dim_n mat = print_matrix_fl dim_m dim_n mat false
let token_print_matrix_fl dim_m dim_n mat = print_matrix_fl dim_m dim_n mat true
let pretty_print_matrix_int dim_m dim_n mat = print_matrix_int dim_m dim_n mat false
let token_print_matrix_int dim_m dim_n mat = print_matrix_int dim_m dim_n mat true

let print_value = function
  | INT_V i -> Printf.printf "%d\n" i
  | FLT_V f -> Printf.printf "%f\n" f
  | BL_V b -> Printf.printf "%b\n" b
  | NVEC_V v -> pretty_print_vector_int (List.length v) v
  | FVEC_V v -> pretty_print_vector_fl (List.length v) v
  | NMAT_V m -> 
      let rows = List.length m in
      let cols = if rows > 0 then List.length (List.hd m) else 0 in
      pretty_print_matrix_int rows cols m
  | FMAT_V m -> 
      let rows = List.length m in
      let cols = if rows > 0 then List.length (List.hd m) else 0 in
      pretty_print_matrix_fl rows cols m
  | FILE_V s -> Printf.printf "%s"s 

let def_val_etype = function
| E_INT -> VAL(INT_V 1)
| E_FLOAT -> VAL(FLT_V 1.0)
| E_BOOL -> VAL(BL_V true)
| E_VEC_N n -> VAL(NVEC_V  (List.init n (fun _ -> 1)))
(* Default dimension, will be checked during type checking *)
| E_VEC_F f -> VAL(FVEC_V  (List.init f (fun _ -> 1.0)))
| E_MAT_N (r,c)-> VAL(NMAT_V (List.init r (fun _ -> List.init c (fun _ -> 1))))
| E_MAT_F (r,c)-> VAL(FMAT_V (List.init r (fun _ -> List.init c (fun _ -> 1.0))))
| E_INP -> VAL(FILE_V "")
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
  Printf.sprintf "%d [%s]" dim elements

(* Convert float matrix to string *)
let string_of_matrix_fl dim_m dim_n mat =
  let rows = List.map (fun row -> 
    let elements = String.concat ", " (List.map string_of_float row) in
    Printf.sprintf "[%s]" elements
  ) mat in
  Printf.sprintf "%d %d [%s]" dim_m dim_n (String.concat ", " rows)

(* Convert int vector to string *)
let string_of_vector_int dim vec =
  let elements = String.concat ", " (List.map string_of_int vec) in
  Printf.sprintf "%d [%s]" dim elements

(* Convert int matrix to string *)
let string_of_matrix_int dim_m dim_n mat =
  let rows = List.map (fun row -> 
    let elements = String.concat ", " (List.map string_of_int row) in
    Printf.sprintf "[%s]" elements
  ) mat in
  Printf.sprintf "%d %d [%s]" dim_m dim_n (String.concat ", " rows)

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

let string_of_type t = match t with 
| T_INT -> "integer"
| T_FLOAT -> "float"
| T_BOOL -> "boolean"
| T_VEC_N -> "integer_vector"
| T_VEC_F -> "float_vector"
| T_MAT_N -> "integer_matrix"
| T_MAT_F -> "float_matrix"
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

let err_string_of_binop op = match op with
  | Add -> "Type mismatch in binary addition (scalars)"
  | Sub -> "Type mismatch in binary subtraction (scalars)"
  | Mul -> "Type mismatch in binary multiplication (scalars)"
  | Div -> "Type mismatch in binary division (scalars)"
  | Modulo -> "Type mismatch in binary modulo (Expected int expressions)"
  | And -> "Type mismatch in binary and(&&) (Expected bool expressions)"
  | Or -> "Type mismatch in binary Or(||) (Expected bool expressions)"
  | Eq -> "Type mismatch in binary Eq(==) (Expected int expressions)"
  | Neq -> "Type mismatch in binary Neq(!=) (Expected int expressions)"
  | Gt -> "Type mismatch in binary Gt(>) (Expected int expressions)"
  | Lt -> "Type mismatch in binary Lt(<) (Expected bool expressions)"
  | Geq -> "Type mismatch in binary Geq(>=) (Expected int expressions)"
  | Leq -> "Type mismatch in binary Leq(==) (Expected int expressions)"
  | Dot_Prod -> "Type mismatch in binary Dot_Prod (Expected vector of same types !)"
  | Angle -> "Type mismatch in binary Angle (Expected vector of same types !)"
  | Add_Vec -> "Type mismatch in binary Add_Vec (Expected vector of same types !)"
  | Scal_Vec -> "Type mismatch in binary Scal_Vec (Scalar,Vector type expressions expected )"
  | Add_Mat -> "Type mismatch in binary Add_Mat (Expected matrix of same types !)"
  | Scal_Mat -> "Type mismatch in binary Scal_Mat (Scalar,Matrix of type expressions expected !)"
  | Mat_Mul_Mat -> "Type mismatch in binary Mat_mul_Mat (Expected Matrix of same types !)"

(* Convert unary operator to string *)
let string_of_unop op =
  match op with
  | Not -> "not"
  | Neg -> "neg"
  | Mag_v -> "mag_v"
  | Dim -> "dim"
  | Trp_Mat -> "trp_mat"
  | Det -> "det"
  | Inv -> "inv"

let err_string_of_unop op = match op with
  | Not -> "Type mismatch in unary Not (Expected bool expression)"
  | Neg -> "Type mismatch in unary Neg (Expected int or float expression)"
  | Mag_v -> "Type mismatch in unary Mag_v (Expected vector expression)"
  | Dim -> "Type mismatch in unary Dim (Expected vector or matrix expression)"
  | Trp_Mat -> "Type mismatch in unary Trp_Mat (Expected matrix expression)"
  | Det -> "Type mismatch in unary Det (Expected square matrix expression)"
  | Inv -> "Type mismatch in unary Inv (Expected invertible square matrix expression)"

let string_of_n_tabs (n:int) = String.make n '\t'
let rec string_of_exp e = match e with
  | IDF s -> s
  | VAL v -> string_of_value v
  | X_Slice(s,e) -> s ^ "[" ^ string_of_exp e ^ "]"
  | XY_Slice(s,e1,e2) -> s ^ "[" ^ string_of_exp e1 ^ "]["^ string_of_exp e2 ^ "]" 
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
  | Assign (typ_opt, name, expr) ->
    ( match typ_opt with
        | Some t -> (string_of_type t) ^ " " ^ name ^ " := " ^ string_of_exp expr
        | None -> name ^ " := " ^ string_of_exp expr
    )
  | Sl_Assign(e1,e2) -> string_of_exp e1 ^ " := " ^ string_of_exp e2
  | Ifte (cond, then_branch, Some else_branch) ->
      "if " ^ string_of_exp cond ^ " then " ^ string_of_stmt then_branch ^ " else " ^ string_of_stmt else_branch
  | Ifte (cond, then_branch, None) ->
      "if " ^ string_of_exp cond ^ " then " ^ string_of_stmt then_branch
  | While (cond, body) ->
      "while " ^ string_of_exp cond ^ " " ^ string_of_stmt body
  | For (init, cond, update, body) ->
      "for (" ^ string_of_stmt init ^ "; " ^ string_of_exp cond ^ "; " ^ string_of_stmt update ^ ") " ^ string_of_stmt body
  | Print expr ->
      "print" ^ "(" ^ (string_of_exp expr) ^ ")"
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
