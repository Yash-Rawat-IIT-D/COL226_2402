val f_eq_z : float -> bool
type myBool = T | F
val my_not : myBool -> myBool
val my_and : myBool -> myBool -> myBool
val my_or : myBool -> myBool -> myBool
val my_xor : myBool -> myBool -> myBool
val myBool_to_bool : myBool -> bool
val bool_to_myBool : bool -> myBool
type vector = float list
exception DimensionError
exception ZeroVectorError
val create : int -> float -> vector
val dim : vector -> int
val is_legal_dim : vector -> bool
val is_zero : vector -> bool
val is_zero_close : vector -> bool
val unit : int -> int -> vector
val scale : float -> vector -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : vector -> vector
val length : vector -> float
val in_domain_acos : float -> float
val angle : vector -> vector -> float
val v1_parallel_norm_v2 : vector -> vector -> float
val v1_plane_norm_v2 : vector -> vector -> vector
type expr =
    T
  | F
  | ConstS of float
  | ConstV of vector
  | Add of expr * expr
  | Sub of expr * expr
  | Inv of expr
  | ScalProd of expr * expr
  | DotProd of expr * expr
  | Mag of expr
  | Angle of expr * expr
  | IsZero of expr
  | Cond of expr * expr * expr
type types = Bool | Scalar | Vector of int
type values = B of myBool | S of float | V of vector
exception Wrong of expr
exception Foo
val eval : expr -> values
val type_of : expr -> types
val print_result : string -> bool -> unit
val test_type_of : unit -> unit
val test_eval : unit -> unit
