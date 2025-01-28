type vector = float list
exception DimensionError
exception ZeroVectorError
val create : int -> float -> vector
val dim : vector -> int
val is_legal_dim : vector -> bool
val is_zero : vector -> bool
val unit : int -> int -> vector
val scale : float -> vector -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : vector -> vector
val length : vector -> float
val in_domain_acos : float -> float
val fl_eq_scaled : float -> float -> bool
val angle : vector -> vector -> float
val test_create : unit -> unit
val test_dim : unit -> unit
val test_is_zero : unit -> unit
val test_unit : unit -> unit
val test_scale : unit -> unit
val test_addv : unit -> unit
val test_dot : unit -> unit
val test_inv : unit -> unit
val test_length : unit -> unit
val test_angle : unit -> unit
