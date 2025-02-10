open A2

(* Test Cases of type E = boolean *)

let%test "type_of T" = (type_of T = Bool)
let%test "type_of F" = (type_of F = Bool)
let%test "type_of (IsZero (ScalProd (ConstS (2.45),ConstV (addv [5.23; -67.8; -324.34; -12.341] [3.58; 96.9; 8.037; 0.546]))))" = (type_of (IsZero (ScalProd (ConstS (2.45),ConstV (addv [5.23; -67.8; -324.34; -12.341] [3.58; 96.9; 8.037; 0.546])))) = Bool)
let%test "type_of (Inv (ScalProd (T,Add(F,(ScalProd (T,Add(F,T))))))) = Bool" = (type_of (Inv (ScalProd (T,Add(F,(ScalProd (T,Add(F,T))))))) = Bool)

(* Test Cases of type e = Scalar *)

let%test "type_of (ConstS (3.1415))" = (type_of (ConstS (3.1415)) = Scalar)
let%test "type_of (Add (ConstS (127.550), ConstS (1289.134)))" = (type_of (Add (ConstS (127.550), ConstS (1289.134))) = Scalar)
let%test "type_of (ScalProd (Add (ConstS (192.01), ConstS (57.44)), ScalProd (ConstS (4.44), ConstS (321.76))))" = (type_of (Sub ((Add (ConstS (192.01), ConstS (57.44))), (Add (ConstS (4.44), ConstS (321.76))))) = Scalar)
let%test "type_of (Inv ( DotProd (ConstV [1.111; 2.222; 3.333], ConstV [4.444; 5.555; 6.666]))" = (type_of (Inv ( DotProd ((ConstV [1.111; 2.222; 3.333]), (ConstV [4.444; 5.555; 6.666])))) = Scalar)
let%test "type_of (Mag (ScalProd (ConstS (-1.56), ConstV [26.47; 8.457; 8.23; 4.7; 3.24]))" = (type_of (Mag (ScalProd (ConstS (-1.56), ConstV [26.47; 8.457; 8.23; 4.7; 3.24]))) = Scalar)
let%test "type_of (ConstS (DotProd (ScalProd (ConstS (5.43), ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS (1.11))))" = (type_of (DotProd (ScalProd (ConstS (5.43), ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS (1.11)))) = Scalar)

(* Test Cases of type e =  Vector _ *)

let%test "type_of (ConstV [1.0; 2.0; 3.0])" = (type_of (ConstV [1.0; 2.0; 3.0]) = Vector (3))
let%test "type_of (ConstV (Add (ConstV [2.983; 457.07; 8.93; 0.245], ConstV [2.93; 6.78; 4.501; 2.655])))" = (type_of (Add (ConstV [2.983; 457.07; 8.93; 0.245], ConstV [2.93; 6.78; 4.501; 2.655])) = Vector(4))
let%test "type_of (Cond (IsZero (ConstS (0.0)), (ConstV [1.0; 2.0; 3.0]), (ScalProd (ConstV,ConstS (5.66)))))" = (type_of (Cond ((IsZero (ConstS (0.0))), (ConstV [1.0; 2.0]), (ConstV [3.0; 4.0]))) = Vector(2))
let%test "type_of (Inv (ScalProd (Add(ConstV [1.618; 2.718; 3.14]), ConstV [8.18; 2.331; 92.22])), ConstS (5.22)))" = (type_of (Inv (ScalProd ((Add((ConstV [1.618; 2.718; 3.14]), (ConstV [8.18; 2.331; 92.22]))), ConstS (5.22)))) = Vector(3))
let%test "type_of (Cond (IsZero ((ConstV [0.0000000123;0.00000000456]) (Add (ConstV (create (33, 7.57) , ConstV (create(33,0.16))))) (unit(33,4))))) = Vector(33)" = ((type_of (Cond ((IsZero ((ConstV [0.0000000123;0.00000000456])), (Add (ConstV (create 33 7.57) , (ConstV (create 33 0.16)))), (ConstV (unit 33 4)))))) = Vector(33))

(* Test Case of eval e --> B boolean *)

let %test "eval (IsZero ( ConstS ( 0.0 ) ) ) " = (eval (IsZero ( ConstS ( 0.0 ) ) ) = B true)
let %test "eval ( Inv ( ScalProd ( T , Add( F , ( ScalProd ( T , Add(F,T) ) ) ) ) ) ) " = (eval ( Inv ( ScalProd ( T , Add( F , ( ScalProd ( T , Add(F,T) ) ) ) ) ) ) = B false) 
let %test "eval ( IsZero ( ScalProd ( ConstS ( 2.45 ) , ConstV ( addv [ 5.23 ; -67.8 ; -324.34 ; -12.341 ] [ 3.58 ; 96.9 ; 8.037 ; 0.546 ] ) ) ) ) " = (eval ( IsZero ( ScalProd ( ConstS ( 2.45 ) , ConstV ( addv [ 5.23 ; -67.8 ; -324.34 ; -12.341 ] [ 3.58 ; 96.9 ; 8.037 ; 0.546 ] ) ) ) ) = B false)
let %test "(eval (Add( (Inv(ScalProd (T,T))) , (Add( (Inv(T)), (Inv(IsZero(ConstS (0.0)))))))) = B false)" = (eval (Add( (Inv(ScalProd (T,T))) , (Add( (Inv(T)), (Inv(IsZero(ConstS (0.0)))))))) = B false)
let %test "eval(Cond((Add((ScalProd(F,F)),(ScalProd(T,T)))),(Cond(((ScalProd((ScalProd(F,F)),(ScalProd(T,T))))),(F),(T))),(Cond(((Add((ScalProd(F,F)),(ScalProd(F,F))))),(F),(F)))))" = (eval(Cond((Add((ScalProd(F,F)),(ScalProd(T,T)))),(Cond(((ScalProd((ScalProd(F,F)),(ScalProd(T,T))))),(F),(T))),(Cond(((Add((ScalProd(F,F)),(ScalProd(F,F))))),(F),(F))))) = B true)



(* Test Case of eval e --> S float *)

let%test "eval (ConstS (2.71812)" = (eval (ConstS (2.71812)) = S 2.71812)
let%test "eval (Add (ConstS (1.0), ConstS (2.0))" = (eval (Add (ConstS (1.0), ConstS (2.0))) = S 3.0)
let%test "eval (Sub (ConstS (10.0), ConstS (4.5))) = S 5.5" = (eval (Sub (ConstS (10.0), ConstS (4.5))) = S 5.5)
let%test "eval (Add (ConstS (2.5), Add (ConstS (3.0), ConstS (4.0)))" = (eval (Add (ConstS (2.5), Add (ConstS (3.0), ConstS (4.0)))) = S 9.5)
let%test "eval (Inv (ConstS (4.0)))" = (eval (Inv (ConstS (4.0))) = S (-4.0))

let%test "eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = (eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = S 32.0)

(* Test Cae of eval e --> V Vector*)
let %test "eval ScalProd()" = ( eval(ScalProd((ConstV [13.45; 22.54; 23.198; 97.3]),(ConstS (0.0)))) = V [0.0; 0.0; 0.0; 0.0])
let %test "eval ( ConstV (addv [1.22; 2.33; 3.44] [1.22; 2.33; 3.44]) ) = V [2.44; 4.66; 6.88]" = (eval ( ConstV (addv [1.22; 2.33; 3.44] [1.22; 2.33; 3.44]) ) = V [2.44; 4.66; 6.88])
let %test "eval(Cond((IsZero( DotProd ((ConstV [3.0; 4.0; 12.0; 13.0]), (ConstV [3.0; 4.0; 12.0; -13.0])) )),ConstV [1.0], ConstV [0.0]))" = (eval(Cond((IsZero( DotProd ((ConstV [3.0; 4.0; 12.0; 13.0]), (ConstV [3.0; 4.0; 12.0; -13.0])) )),ConstV [1.0], ConstV [0.0])) = V [1.0])
let %test "eval ( Cond ( IsZero ( ConstV (addv [-4.00; 2.44; 1.14] [4.00; -2.44; -1.14] ) ) , ConstV [ 3.0 ; 4.0 ] , ConstV [ 1.0 ; 2.0 ] ) )" = eval ( Cond ( IsZero ( ConstV (addv [-4.00; 2.44; 1.14] [4.00; -2.44; -1.14] ) ) , ConstV [ 3.0 ; 4.0 ] , ConstV [ 1.0 ; 2.0 ] ) ) = V [ 3.0 ; 4.0 ]
let %test "eval(Add( Add( (ScalProd((ConstS(3.0)), (ConstV(unit 5 2)))), (ConstV [0.0; 0.0; 4.0; 0.0; 0.0])) ,(ScalProd((ConstS(5.0)), (ConstV(unit 5 4)))) )) = V [0.0; 3.0; 4.0; 5.0; 0.0]" = (eval(Add( Add( (ScalProd((ConstS(3.0)), (ConstV(unit 5 2)))), (ConstV [0.0; 0.0; 4.0; 0.0; 0.0])) ,(ScalProd((ConstS(5.0)), (ConstV(unit 5 4)))) )) = V [0.0; 3.0; 4.0; 5.0; 0.0])




(* Test Cases of type Wrong(expr) *)

(*

Note that implementation of eval involves the evaluation of the expression at the runtime itself
So if the expression (or any of its subtrees) are incorrect under typing assumptions,
then the evaluation of the expression will raise an exception of type Wrong(expr)

Hence we only need to test the type_of function for the Wrong(expr) case

*)

(* Incorrect Overloading of Operators *)

let %test "type_of (Add (ConstS (3.1415), ConstV [1.0; 2.0; 3.0]))" = 
(
  try 
    let _ = (type_of (Add (ConstS (3.1415), ConstV [1.0; 2.0; 3.0]))) 
    in false
    with
    | Wrong e -> e = (Add (ConstS (3.1415), ConstV [1.0; 2.0; 3.0]))  
    | _ -> false
)

let %test "type_of (Add (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.1; 0.2; 0.3; 0.4]))" = 
(
  try 
    let _ = (type_of (Add (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.1; 0.2; 0.3; 0.4])))
    in false
    with
    | Wrong e -> e = (Add (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.1; 0.2; 0.3; 0.4])) 
    | _ -> false
)

let %test "type_of (Add (Add(ConstS 8.31,ConstS 5.399), Add(T,F)))" = 
(
  try 
    let _ = (type_of (Add (Add(ConstS 8.31,ConstS 5.399), Add(T,F))))
    in false
    with
    | Wrong e -> e = (Add (Add(ConstS 8.31,ConstS 5.399), Add(T,F))) 
    | _ -> false
)

let %test "type_of (ScalProd (ConstV(addv (inv [-3.12, 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstV [3.14; 15.92; 65.3589]))" = 
(
  try 
    let _ = (type_of (ScalProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] ) ,ConstV [3.14; 15.92; 65.3589])))
    in false
    with
    | Wrong e -> e = ((ScalProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstV [3.14; 15.92; 65.3589]))) 
    | _ -> false
)

let %test "type_of (DotProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] )" = 
( try 
  let _ = type_of (DotProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstS 4.12))
  in false
  with
  | Wrong e -> e = (DotProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstS 4.12))
  | _ -> false
)

let %test "type_of (DotProd (ConstV(addv (inv [-3.12, 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstV [3.14;65.3589]))"=
( try 
  let _ = type_of (DotProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstV [3.14;65.3589]))
  in false
  with
  | Wrong e -> e = (DotProd (ConstV(addv (inv [-3.12; 323.435; 23.22]) [39.02; 18.047; 925.34] ) , ConstV [3.14;65.3589]))
  | _ -> false
)

let %test "Cond(Mag(ConstS -3.28), ConstV [56.78; 43.22; 89.76] , ConstV [65.89; 78.34; 26.76] )" =
(
  try 
  let _ = type_of(Cond(Mag(ConstS (-3.28)), ConstV [56.78; 43.22; 89.76] , ConstV [65.89; 78.34; 26.76] ))
  in false
  with
  | Wrong e -> e = (Cond(Mag(ConstS (-3.28)), ConstV [56.78; 43.22; 89.76] , ConstV [65.89; 78.34; 26.76] ))
  | _ -> false
)

let %test "Cond(Inv(Add((Add(F,F)),(Add(T,F)))), ConstV [56.78; 43.22; 89.76; 3.455] , ConstV [65.89; 78.34; 26.76] )" =
(
  try 
  let _ = type_of( Cond((Inv(Add((Add(F,F)),(Add(T,F))))), (ConstV [56.78; 43.22; 89.76; 3.455]), (ConstV [65.89; 78.34; 26.76])) )
  in false
  with
  | Wrong e -> e = Cond((Inv(Add((Add(F,F)),(Add(T,F))))), (ConstV [56.78; 43.22; 89.76; 3.455]),(ConstV [65.89; 78.34; 26.76]))
  | _ -> false 
)

let %test "type_of(Angle((ConstV [56.78; 43.22; 89.76; 3.455]), (ConstV [65.89; 78.34; 26.76])) )" =

(
  try 
  let _ = type_of(Angle((ConstV [56.78; 43.22; 89.76; 3.455]), (ConstV [65.89; 78.34; 26.76])) )
  in false
  with
  | Wrong e -> e = Angle((ConstV [56.78; 43.22; 89.76; 3.455]), (ConstV [65.89; 78.34; 26.76]))
  | _ -> false 
)

let %test "type_of(Angle((Cond((T) ,(ConstV [3.0;4.0 ; 12.0]),(ConstV [-3.0;-4.0 ;-12.0]) )), (ConstV [2.0; 0.0; 0.0; 0.0])))"= 
(
  try 
  let _ = type_of(Angle((Cond((T) ,(ConstV [3.0;4.0 ; 12.0]),(ConstV [-3.0;-4.0 ;-12.0]) )), (ConstV [2.0; 0.0; 0.0; 0.0])))
  in false
  with
  | Wrong e -> e = Angle(Cond((T) ,(ConstV [3.0;4.0 ; 12.0]),(ConstV [-3.0;-4.0 ;-12.0]) ), (ConstV [2.0; 0.0; 0.0; 0.0]))
  | _ -> false
)