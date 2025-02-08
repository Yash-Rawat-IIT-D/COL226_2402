open A2

let%test "type_of T" = (type_of T = Bool)
let%test "type_of F" = (type_of F = Bool)

let%test "type_of (ConstS 3.14)" = (type_of (ConstS 3.14) = Scalar)
let%test "type_of (ConstV [13.123; 45.75; 6.97; 8.1364])" = (type_of (ConstV [13.123; 45.75; 6.97; 8.1364]) = Vector 4)

let%test "type_of (Add (ConstS 2.0, ConstS 3.0))" = (type_of (Add (ConstS 2.0, ConstS 3.0)) = Scalar)
let%test "type_of (Add (T, F))" = (type_of (Add (T, F)) = Bool)

let%test "type_of (Sub (ConstS 5.5, ConstS 1.5))" = (type_of (Sub (ConstS 5.5, ConstS 1.5)) = Scalar)
let%test "type_of (Sub (ConstV v, ConstV v))" = (type_of (Sub (ConstV v, ConstV v)) = Vector (dim v))

let%test "type_of (Inv (ConstS (-2.5)))" = (type_of (Inv (ConstS (-2.5))) = Scalar)
let%test "type_of (Inv T)" = (type_of (Inv T) = Bool)

let%test "type_of (ScalProd (ConstS 2.0, ConstV v))" = (type_of (ScalProd (ConstS 2.0, ConstV v)) = Vector (dim v))
let%test "type_of (ScalProd (ConstV v, ConstS 3.0))" = (type_of (ScalProd (ConstV v, ConstS 3.0)) = Vector (dim v))

let%test "type_of (DotProd (ConstV v, ConstV v))" = (type_of (DotProd (ConstV v, ConstV v)) = Scalar)

let%test "type_of (Mag (ConstS 4.0))" = (type_of (Mag (ConstS 4.0)) = Scalar)
let%test "type_of (Mag (ConstV v))" = (type_of (Mag (ConstV v)) = Scalar)

let%test "type_of (Angle (ConstV v, ConstV v))" = (type_of (Angle (ConstV v, ConstV v)) = Scalar)

let%test "type_of (IsZero (ConstS 0.0))" = (type_of (IsZero (ConstS 0.0)) = Bool)
let%test "type_of (IsZero (ConstV v))" = (type_of (IsZero (ConstV v)) = Bool)

let%test "type_of (Cond (T, ConstS 1.0, ConstS 2.0))" = (type_of (Cond (T, ConstS 1.0, ConstS 2.0)) = Scalar)
let%test "type_of (Cond (F, ConstV v, ConstV v))" = (type_of (Cond (F, ConstV v, ConstV v)) = Vector (dim v))

(* Complex Expressions with AST height ~4-5 *)
let%test "type_of (Add (Sub (ConstS 1.0, ConstS 2.0), Inv (ConstS 3.0)))" = 
  (type_of (Add (Sub (ConstS 1.0, ConstS 2.0), Inv (ConstS 3.0))) = Scalar)

let%test "type_of (DotProd (ScalProd (ConstS 2.0, ConstV v), ConstV v))" = 
  (type_of (DotProd (ScalProd (ConstS 2.0, ConstV v), ConstV v)) = Scalar)

let%test "type_of (Cond (IsZero (Sub (ConstS 5.0, ConstS 5.0)), Add (ConstV v, ConstV v), ConstV v))" = 
  (type_of (Cond (IsZero (Sub (ConstS 5.0, ConstS 5.0)), Add (ConstV v, ConstV v), ConstV v)) = Vector (dim v))

let%test "type_of (Angle (DotProd (ConstV v, ConstV v), Mag (ConstV v)))" = 
  (type_of (Angle (DotProd (ConstV v, ConstV v), Mag (ConstV v))) = Scalar)

let%test "type_of (ScalProd (Cond (IsZero (ConstS 0.0), ConstS 2.0, ConstS -3.0), Add (ConstV v, Inv (ConstV v))))" = 
  (type_of (ScalProd (Cond (IsZero (ConstS 0.0), ConstS 2.0, ConstS -3.0), Add (ConstV v, Inv (ConstV v)))) = Vector (dim v))

let%test "type_of (Mag (DotProd (Add (ConstV v, ConstV v), Sub (ConstV v, ConstV v))))" = 
  (type_of (Mag (DotProd (Add (ConstV v, ConstV v), Sub (ConstV v, ConstV v)))) = Scalar)

let%test "type_of (Cond (IsZero (Angle (ConstV v, ConstV v)), ScalProd (ConstS 2.0, ConstV v), ConstV v))" = 
  (type_of (Cond (IsZero (Angle (ConstV v, ConstV v)), ScalProd (ConstS 2.0, ConstV v), ConstV v)) = Vector (dim v))

let%test "type_of (Inv (Add (Cond (T, ConstS 3.0, ConstS -3.0), Sub (ConstS 4.0, ConstS 2.0))))" = 
  (type_of (Inv (Add (Cond (T, ConstS 3.0, ConstS -3.0), Sub (ConstS 4.0, ConstS 2.0)))) = Scalar)

let%test "type_of (DotProd (ScalProd (Mag (ConstV v), ConstV v), Angle (ConstV v, ConstV v)))" = 
  (type_of (DotProd (ScalProd (Mag (ConstV v), ConstV v), Angle (ConstV v, ConstV v))) = Scalar)
