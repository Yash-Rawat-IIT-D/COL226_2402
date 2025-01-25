type vector = float list
exception DimensionError
val create : int -> float -> vector
val dim : vector -> int
val is_zero : vector -> bool
val unit : int -> int -> vector
val scale : float -> vector -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : vector -> vector
val length : vector -> float
val angle : vector -> vector -> float
val v1 : vector
val v2 : vector
val v3 : vector
