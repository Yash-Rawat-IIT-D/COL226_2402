exception DimensionError of string
type vector = float list
val create : int -> float -> vector
val dim : vector -> int
val is_zero : vector -> bool
val unit : int -> int -> vector
val scale : float -> float list -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : float list -> vector
val length : vector -> float
val angle : vector -> vector -> float
val n : int
val j : int
val c : float
val v : vector
val v1 : vector
val v2 : vector
val result : float
