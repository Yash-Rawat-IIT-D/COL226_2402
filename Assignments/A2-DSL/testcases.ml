open A2

(* Test Cases of type boolean *)

let%test "type_of T" = (type_of T = Bool)
let%test "type_of F" = (type_of F = Bool)
let%test "type_of (IsZero (ScalProd (ConstS 2.45,ConstV (addv([5.23; -67.8; -324.34; -12.341], [3.58; 96.9; 8.037; 0.546])))))" 
= (type_of (IsZero (ScalProd (ConstS 2.45, ConstV (addv([5.23; -67.8; -324.34; -12.341], [3.58; 96.9; 8.037; 0.546]))))) = Bool)
let%test "type_of Add(Add(ScalProd(T,F),ScalProd(F,T)),Add(ScalProd(T,T),ScalProd(F,F)))" 
= (type_of (Add(Add(ScalProd(T,F),ScalProd(F,T)),Add(ScalProd(T,T),ScalProd(F,F)))) = Bool)
let%test "type_of type_of (Inv (ScalProd (T,Add(F,(ScalProd (T,Add(F,T))))))) = Bool"
= (type_of (Inv (ScalProd (T,Add(F,(ScalProd (T,Add(F,T))))))) = Bool)

(* Test Cases of type scalar *)

let%test "type_of (ConstS 3.1415)" = (type_of (ConstS 3.14) = Scalar)
let%test "type_of (Add (ConstS 127.550, ConstS 1289.134))" = (type_of (Add (ConstS 127.550, ConstS 1289.134)) = Scalar)
let%test "type_of (ScalProd (Add (ConstS 192.01, ConstS 57.44), ScalProd (ConstS 4.44, ConstS 321.76)))" = (type_of (Sub (Add (ConstS 192.01, ConstS 57.44), Add (ConstS 4.44, ConstS 321.76)) = Scalar))
let%test "type_of (Inv ( DotProd (ConstV [1.111; 2.222; 3.333], ConstV [4.444; 5.555; 6.666]))" = (type_of (Inv ( DotProd (ConstV [1.111; 2.222; 3.333], ConstV [4.444; 5.555; 6.666])) = Scalar))
let%test "type_of (Mag (ScalProd (ConstS -1.56, ConstV [26.47; 8.457; 8.23; 4.7; 3.24]))" = (type_of (Mag (ScalProd (ConstS -1.56, ConstV [26.47; 8.457; 8.23; 4.7; 3.24])) = Scalar))
let%test "type_of (DotProd (ScalProd (ConstS 5.43, ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS 1.11))" = (type_of (DotProd (ScalProd (ConstS 5.43, ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS 1.11)) = Scalar))

(* Test Cases of type vector *)

let%test "type_of (ConstV [1.0; 2.0; 3.0])" = (type_of (ConstV [1.0; 2.0; 3.0]) = Vector 3)
let%test "type_of (Add (ConstV [2.983; 457.07; 8.93; 0.245], ConstV [2.93; 6.78; 4.501; 2.655]))" = (type_of (Add (ConstV [2.983; 457.07; 8.93; 0.245], ConstV [2.93; 6.78; 4.501; 2.655]) = Vector 4))
let%test "type_of (Cond (IsZero (ConstS 0.0), (ConstV [1.0; 2.0; 3.0]), (ScalProd (ConstV,ConstS 5.66))))" = (type_of (Cond (IsZero (ConstS 0.0), ConstV [1.0; 2.0], ConstV [3.0; 4.0]) = Vector 2))
let%test "type_of (Inv (ScalProd (Add(ConstV [1.618; 2.718; 3.14]), ConstV [8.18; 2.331; 92.22])), ConstS 5.22))" = ((type_of (Inv (ScalProd (Add(ConstV [1.618; 2.718; 3.14]), ConstV [8.18; 2.331; 92.22])), ConstS 5.22)) = Vector 3)
