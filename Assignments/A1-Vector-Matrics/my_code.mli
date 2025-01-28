type vector = float list
exception DimensionError
exception ZeroVectorError
val create : int -> float -> vector
val create_tail_rec : int -> float -> vector
val dim : vector -> int
val dim_tail_rec : vector -> int
val is_legal_dim : vector -> bool
val is_zero : vector -> bool
val is_zero_tail_rec : vector -> bool
val unit : int -> int -> vector
val scale : float -> vector -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : vector -> vector
val length : vector -> float
val in_domain_acos : float -> float
val angle : vector -> vector -> float
val create_tc_0 : vector
val create_tc_1 : vector
val create_tc_2 : vector
val n : int
val create_tc_3 : vector
val test_dim : unit -> unit
val test_is_zero : unit -> unit
val test_unit : unit -> unit
val test_scale : unit -> unit
val test_addv : unit -> unit
val test_dot : unit -> unit
