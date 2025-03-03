open A2
(* Type of Tests *)
let%test "type_of T" = (type_of T = Bool)
let%test "type_of F" = (type_of F = Bool)

let%test "type_of (ConstS 5.0)" = (type_of (ConstS 5.0) = Scalar)
let%test "type_of (ConstS (-23.0)" = (type_of (ConstS (-23.0)) = Scalar)
let%test "type_of (ConstS 0.0)" = (type_of (ConstS 0.0) = Scalar)

let%test "type_of (ConstV [1.0; -2.213213; 3232424.0])" = (type_of (ConstV [1.0; -2.213213; 3232424.0]) = Vector 3)
let%test "type_of (ConstV [])" =
  (try
    let _ = type_of(ConstV []) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "type_of (Inv T)" = (type_of (Inv T) = Bool)  
let%test "type_of (Inv F)" = (type_of (Inv F) = Bool)  
let%test "type_of (Inv (ConstS (-2.5)))" = (type_of (Inv (ConstS (-2.5))) = Scalar)
let%test "type_of (Inv (ConstS (54.232734284)))" = (type_of (Inv (ConstS (54.232734284))) = Scalar)
let%test "type_of (Inv(ConstV []))" =
  (try
    let _ = type_of(Inv(ConstV [])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Inv (ConstV [2.4323434231; 0.0; -5.234; 0.00002; -0.43234; 3732.0; -4222312.42455]))" = (type_of (Inv (ConstV [2.4323434231; 0.0; -5.234; 0.00002; -0.43234; 3732.0; -4222312.42455])) = Vector 7)

let%test "type_of (Add (T, T))" = (type_of (Add (T, T)) = Bool) 
let%test "type_of (Add (T, F))" = (type_of (Add (T, F)) = Bool) 
let%test "type_of (Add (F, T))" = (type_of (Add (F, T)) = Bool) 
let%test "type_of (Add (F, F))" = (type_of (Add (F, F)) = Bool) 
let%test "type_of (Add (T, ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = type_of (Add (T, ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Add (ConstS 0.0, F))" =
  (try
    let _ = type_of (Add (ConstS 0.0, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Add (ConstS 0.0, ConstV [2.3234]))" =
  (try
    let _ = type_of (Add (ConstS 0.0, ConstV [2.3234])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Add (ConstV [2.3234; 23013.0], F))" =
  (try
    let _ = type_of (Add (ConstV [2.3234; 23013.0], F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Add (ConstS 3.0, ConstS 4323.0))" = (type_of (Add (ConstS 3.0, ConstS 4323.0)) = Scalar) 
let%test "type_of (Add (ConstV [1.0; -2.2324], ConstV [4232.0; -77.2344]))" = (type_of (Add (ConstV [1.0; -2.2324], ConstV [4232.0; -77.2344])) = Vector 2)
let%test "type_of (Add (ConstV [1.0; -2.2324], ConstV []))" =
  (try
    let _ = type_of (Add (ConstV [1.0; -2.2324], ConstV [])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Add (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = type_of (Add (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "type_of (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = (type_of (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = Scalar)
let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0]))" =
  (try
    let _ = type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (DotProd (ConstV [], ConstV [1.0]))" =
  (try
    let _ = type_of (DotProd (ConstV [], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (DotProd (ConstS 23.0, ConstS -122.12313))" =
  (try
    let _ = type_of (DotProd (ConstS 23.0, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (DotProd (ConstS 23.0, ConstV [1.0]))" =
  (try
    let _ = type_of (DotProd (ConstS 23.0, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (DotProd (T, ConstV [1.0]))" =
  (try
    let _ = type_of (DotProd (T, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (DotProd (T, F))" =
  (try
    let _ = type_of (DotProd (T, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "type_of (Mag (ConstV [3.0; 4.0]))" = (type_of (Mag (ConstV [3.0; 4.0])) = Scalar)
let%test "type_of (Mag (ConstV [-2.0; 1.0; 3.0; -3.0]))" = (type_of (Mag (ConstV [-2.0; 1.0; 3.0; -3.0])) = Scalar)
let%test "type_of (Mag (ConstS 3231.212))" = (type_of (Mag (ConstS 3231.212)) = Scalar)
let%test "type_of (Mag (ConstS -31.21112))" = (type_of (Mag (ConstS (-31.21112))) = Scalar)
let%test "type_of (Mag T)" =
  (try
    let _ = type_of (Mag T) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Mag (ConstV []))" =
  (try
    let _ = type_of (Mag((ConstV []))) in false
  with
  | Wrong _ -> true
  | _ -> false)


let%test "type_of (ScalProd (T, T))" = (type_of (ScalProd (T, T)) = Bool) 
let%test "type_of (ScalProd (T, F))" = (type_of (ScalProd (T, F)) = Bool) 
let%test "type_of (ScalProd (F, T))" = (type_of (ScalProd (F, T)) = Bool) 
let%test "type_of (ScalProd (F, F))" = (type_of (ScalProd (F, F)) = Bool) 
let%test "type_of (ScalProd (T, ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = type_of (ScalProd (T, ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (ScalProd (ConstS 0.0, F))" =
  (try
    let _ = type_of (ScalProd (ConstS 0.0, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (ScalProd (ConstS 0.0, ConstV [2.3234]))" = (type_of (ScalProd (ConstS 0.0, ConstV [2.3234])) = Vector 1) 
let%test "type_of (ScalProd (ConstV [2.3234; 23013.0], F))" =
  (try
    let _ = type_of (ScalProd (ConstV [2.3234; 23013.0], F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (ScalProd (ConstS 3.0, ConstS 4323.0))" = (type_of (ScalProd (ConstS 3.0, ConstS 4323.0)) = Scalar) 
let%test "type_of (ScalProd (ConstV [1.0; -2.2324], ConstV []))" =
  (try
    let _ = type_of (ScalProd (ConstV [1.0; -2.2324], ConstV [])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (ScalProd (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = type_of (ScalProd (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (ScalProd (ConstS (-2.5), ConstV [1.0; -2.213213; 3232424.0]))" = (type_of (ScalProd (ConstS (-2.5), ConstV [1.0; -2.213213; 3232424.0])) = Vector 3)
let%test "type_of (ScalProd (ConstV [1.0; -2.213213; 3232424.0], ConstS (-2.5)))" = (type_of (ScalProd (ConstV [1.0; -2.213213; 3232424.0], ConstS (-2.5))) = Vector 3)


let%test "type_of (Angle (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = (type_of (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = Scalar)
let%test "type_of (Angle (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0]))" = (type_of (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])) = Scalar)
let%test "type_of (Angle (ConstV [1.0; 2.0], ConstV [1.0]))" =
  (try
    let _ = type_of (Angle (ConstV [1.0; 2.0], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Angle (ConstV [], ConstV [1.0]))" =
  (try
    let _ = type_of (Angle (ConstV [], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Angle (ConstS 23.0, ConstS -122.12313))" =
  (try
    let _ = type_of (Angle (ConstS 23.0, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Angle (ConstS 23.0, ConstV [1.0]))" =
  (try
    let _ = type_of (Angle (ConstS 23.0, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Angle (T, ConstV [1.0]))" =
  (try
    let _ = type_of (Angle (T, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Angle (T, F))" =
  (try
    let _ = type_of (Angle (T, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "type_of (IsZero (ConstV [0.0; 0.0; 0.0]))" = (type_of (IsZero (ConstV [0.0; 0.0; 0.0])) = Bool)
let%test "type_of (IsZero (ConstV [0.000000001; 0.0; -2.0]))" = (type_of (IsZero (ConstV [0.000000001; 0.0; -2.0])) = Bool)
let%test "type_of (IsZero (T)" = (type_of (IsZero (T)) = Bool)
let%test "type_of (IsZero (F)" = (type_of (IsZero (F)) = Bool)
let%test "type_of (IsZero (ConstS 5.232)" = (type_of (IsZero (ConstS 5.232)) = Bool)
let%test "type_of (IsZero (ConstS 0.0)" = (type_of (IsZero (ConstS 0.0)) = Bool)

let%test "type_of (Cond (T, ConstS 5.0, ConstS 3.0))" = (type_of (Cond (T, ConstS 5.0, ConstS 3.0)) = Scalar)
let%test "type_of (Cond (F, ConstS 5.0, ConstS 3.0))" = (type_of (Cond (F, ConstS 5.0, ConstS 3.0)) = Scalar)
let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)
let%test "type_of (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)
let%test "type_of (Cond (T, T, F))" = (type_of (Cond (T, T, F)) = Bool)
let%test "type_of (Cond (F, T, F))" = (type_of (Cond (F, T, F)) = Bool)
let%test "type_of (Cond (ConstS 2.0, ConstS 2.232, ConstS (-9998.8)))" =
  (try
    let _ = type_of (Cond (ConstS 2.0, ConstS 2.232, ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Cond (ConstV [2.232; -2332.323], ConstS 2.232, ConstS (-9998.8)))" =
  (try
    let _ = type_of (Cond (ConstV [2.232; -2332.323], ConstS 2.232, ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Cond (T, ConstV [], ConstS (-9998.8)))" =
  (try
    let _ = type_of (Cond (T, ConstV [], ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "type_of (Cond (F, ConstV [], ConstS (-9998.8)))" =
  (try
    let _ = type_of (Cond (F, ConstV [], ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)


(* Eval Tests *)
let%test "eval T" = (eval T = B true)
let%test "eval F" = (eval F = B false)

let%test "eval (ConstS 5.0)" = (eval (ConstS 5.0) = S 5.0)
let%test "eval (ConstS -23.0)" = (eval (ConstS (-23.0)) = S (-23.0))
let%test "eval (ConstS 0.0)" = (eval (ConstS 0.0) = S 0.0)

let%test "eval (ConstV [1.0; -2.213213; 3232424.0])" = (eval (ConstV [1.0; -2.213213; 3232424.0]) = V [1.; -2.213213; 3232424.])
let%test "eval (ConstV [])" =
  (try
    let _ = eval(ConstV []) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (Inv T)" = (eval (Inv T) = B false)  
let%test "eval (Inv F)" = (eval (Inv F) = B true)  
let%test "eval (Inv (ConstS (-2.5)))" = (eval (Inv (ConstS (-2.5))) = S 2.5)
let%test "eval (Inv (ConstS (54.232734284)))" = (eval (Inv (ConstS (54.232734284))) = S (-54.232734284))
let%test "eval (Inv(ConstV []))" =
  (try
    let _ = eval(Inv(ConstV [])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Inv (ConstV [2.4323434231; 0.0; -5.234; 0.00002; -0.43234; 3732.0; -4222312.42455]))" = (eval (Inv (ConstV [2.4323434231; 0.0; -5.234; 0.00002; -0.43234; 3732.0; -4222312.42455])) = V [-2.4323434231; -0.; 5.234; -2e-05; 0.43234; -3732.; 4222312.42455])

let%test "eval (Add (T, T))" = (eval (Add (T, T)) = B true) 
let%test "eval (Add (T, F))" = (eval (Add (T, F)) = B true) 
let%test "eval (Add (F, T))" = (eval (Add (F, T)) = B true) 
let%test "eval (Add (F, F))" = (eval (Add (F, F)) = B false) 
let%test "eval (Add (T, ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = eval (Add (T, ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Add (ConstS 0.0, F))" =
  (try
    let _ = eval (Add (ConstS 0.0, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Add (ConstS 0.0, ConstV [2.3234]))" =
  (try
    let _ = eval (Add (ConstS 0.0, ConstV [2.3234])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Add (ConstV [2.3234; 23013.0], F))" =
  (try
    let _ = eval (Add (ConstV [2.3234; 23013.0], F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Add (ConstS 3.0, ConstS 4323.0))" = (eval (Add (ConstS 3.0, ConstS 4323.0)) = S 4326.) 
let%test "eval (Add (ConstV [1.0; -2.0], ConstV [4232.0; -77.0]))" = (eval (Add (ConstV [1.0; -2.0], ConstV [4232.0; -77.0])) = V [4233.; -79.])
let%test "eval (Add (ConstV [1.0; -2.2324], ConstV []))" =
  (try
    let _ = eval (Add (ConstV [1.0; -2.2324], ConstV [])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Add (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = eval (Add (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = (eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = S 32.)
let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [1.0]))" =
  (try
    let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (DotProd (ConstV [], ConstV [1.0]))" =
  (try
    let _ = eval (DotProd (ConstV [], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (DotProd (ConstS 23.0, ConstS (-122.2323)))" =
  (try
    let _ = eval (DotProd (ConstS 23.0, ConstS (-122.2323))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (DotProd (ConstS 23.0, ConstV [1.0]))" =
  (try
    let _ = eval (DotProd (ConstS 23.0, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (DotProd (T, ConstV [1.0]))" =
  (try
    let _ = eval (DotProd (T, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (DotProd (T, F))" =
  (try
    let _ = eval (DotProd (T, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (Mag (ConstV [3.0; 4.0]))" = (eval (Mag (ConstV [3.0; 4.0])) = S 5.)
let%test "eval (Mag (ConstV [-2.0; 1.0; -1.0; 1.0; 3.0; -3.0]))" = (eval (Mag (ConstV [-2.0; 1.0; -1.0; 1.0; 3.0; -3.0])) = S 5.)
let%test "eval (Mag (ConstS 3231.212))" = (eval (Mag (ConstS 3231.212)) = S 3231.212)
let%test "eval (Mag (ConstS -31.21112))" = (eval (Mag (ConstS (-31.21112))) = S 31.21112)
let%test "eval (Mag T)" =
  (try
    let _ = eval (Mag T) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Mag (ConstV []))" =
  (try
    let _ = eval (Mag((ConstV []))) in false
  with
  | Wrong _ -> true
  | _ -> false)


let%test "eval (ScalProd (T, T))" = (eval (ScalProd (T, T)) = B true) 
let%test "eval (ScalProd (T, F))" = (eval (ScalProd (T, F)) = B false) 
let%test "eval (ScalProd (F, T))" = (eval (ScalProd (F, T)) = B false) 
let%test "eval (ScalProd (F, F))" = (eval (ScalProd (F, F)) = B false) 
let%test "eval (ScalProd (T, ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = eval (ScalProd (T, ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (ScalProd (ConstS 0.0, F))" =
  (try
    let _ = eval (ScalProd (ConstS 0.0, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (ScalProd (ConstS 0.0, ConstV [2.3234]))" = (eval (ScalProd (ConstS 0.0, ConstV [2.3234])) = V [0.]) 
let%test "eval (ScalProd (ConstV [2.3234; 23013.0], F))" =
  (try
    let _ = eval (ScalProd (ConstV [2.3234; 23013.0], F)) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (ScalProd (ConstS 3.0, ConstS 4323.0))" = (eval (ScalProd (ConstS 3.0, ConstS 4323.0)) = S 12969.) 
let%test "eval (ScalProd (ConstV [1.0; -2.2324], ConstV []))" =
  (try
    let _ = eval (ScalProd (ConstV [1.0; -2.2324], ConstV [])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (ScalProd (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11]))" =
  (try
    let _ = eval (ScalProd (ConstV [1.0; -2.2324], ConstV [432.23452; 0.0; 12.134; -91373.11])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (ScalProd (ConstS (-2.5), ConstV [1.0; -2.5; 3232424.0]))" = (eval (ScalProd (ConstS (-2.5), ConstV [1.0; -2.5; 3232424.0])) = V [-2.5; 6.25; -8081060.])
let%test "eval (ScalProd (ConstV [1.0; -2.5; 3232424.0], ConstS (-2.5)))" = (eval (ScalProd (ConstV [1.0; -2.5; 3232424.0], ConstS (-2.5))) = V [-2.5; 6.25; -8081060.])

let%test "eval (Angle (ConstV [1.0; 0.0; 0.0], ConstV [1.0; 0.0; 0.0]))" = (eval (Angle (ConstV [1.0; 0.0; 0.0], ConstV [1.0; 0.0; 0.0])) = S 0.)
let%test "eval (Angle (ConstV [1.0; 2.0], ConstV [0.0, 0.0000000001]))" =
  (try
    let _ = eval (Angle (ConstV [1.0; 2.0], ConstV [0.0; 0.0000000001])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Angle (ConstV [], ConstV [1.0]))" =
  (try
    let _ = eval (Angle (ConstV [], ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Angle (ConstS 23.0, ConstS (-122.12313))" =
  (try
    let _ = eval (Angle (ConstS 23.0, ConstS (-122.12313))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Angle (ConstS 23.0, ConstV [1.0]))" =
  (try
    let _ = eval (Angle (ConstS 23.0, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Angle (T, ConstV [1.0]))" =
  (try
    let _ = eval (Angle (T, ConstV [1.0])) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Angle (T, F))" =
  (try
    let _ = eval (Angle (T, F)) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (IsZero (ConstV [0.0; 0.0; 0.0]))" = (eval (IsZero (ConstV [0.0; 0.0; 0.0])) = B true)
let%test "eval (IsZero (ConstV [0.000000001; 0.0; -2.0]))" = (eval (IsZero (ConstV [0.000000001; 0.0; -2.0])) = B false)
let%test "eval (IsZero (T)" = (eval (IsZero (T)) = B false)
let%test "eval (IsZero (F)" = (eval (IsZero (F)) = B true)
let%test "eval (IsZero (ConstS 5.232))" = (eval (IsZero (ConstS 5.232)) = B false)
let%test "eval (IsZero (ConstS 0.000000001))" = (eval (IsZero (ConstS 0.0)) = B true)

let%test "eval (Cond (T, ConstS 5.0, ConstS 3.0))" = (eval (Cond (T, ConstS 5.0, ConstS 3.0)) = S 5.)
let%test "eval (Cond (F, ConstS 5.0, ConstS 3.0))" = (eval (Cond (F, ConstS 5.0, ConstS 3.0)) = S 3.)
let%test "eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [1.; 2.])
let%test "eval (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (eval (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [3.; 4.])
let%test "eval (Cond (T, T, F))" = (eval (Cond (T, T, F)) = B true)
let%test "eval (Cond (IsZero (ConstS 0.), T, F))" = (eval (Cond (IsZero (ConstS 0.), T, F)) = B true)
let%test "eval (Cond (F, T, F))" = (eval (Cond (F, T, F)) = B false)
let%test "eval (Cond (ConstS 2.0, ConstS 2.232, ConstS (-9998.8)))" =
  (try
    let _ = eval (Cond (ConstS 2.0, ConstS 2.232, ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Cond (ConstV [2.232; -2332.323], ConstS 2.232, ConstS (-9998.8)))" =
  (try
    let _ = eval (Cond (ConstV [2.232; -2332.323], ConstS 2.232, ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Cond (T, ConstV [], ConstS (-9998.8)))" =
  (try
    let _ = eval (Cond (T, ConstV [], ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)
let%test "eval (Cond (T, ConstV [], ConstS (-9998.8)))" =
  (try
    let _ = eval (Cond (F, ConstV [], ConstS (-9998.8))) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (ScalProd (Mag (Add (ConstV [3.0; -1.0], Inv (ConstV [2.0; -2.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0])))" = 
  (eval (ScalProd (Mag (Add (ConstV [3.0; -1.0], Inv (ConstV [-1.0; 2.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0]))) = S (-5.))  

let%test "eval (IsZero (Angle (Add (ConstV [2.0; 2.0; 2.0], Inv (ConstV [2.0; 2.0; 2.0])), ConstV [1.0; 1.0; 1.0])))" = 
  (try
    let _ = eval (IsZero (Angle (Add (ConstV [2.0; 2.0; 2.0], Inv (ConstV [2.0; 2.0; 2.0])), ConstV [1.0; 1.0; 1.0]))) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "eval (ScalProd (Mag (Add (ConstV [3.0; -1.0], Inv (ConstV [2.0; -2.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0])))" = 
  (eval (ScalProd (Mag (Add (ConstV [3.0; -1.0], Inv (ConstV [-1.0; 2.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0]))) = S (-5.))

let%test "eval (Cond (IsZero (ConstS 0.0), Add (ConstS 5.0, ConstS 3.0), ScalProd (ConstS 2.0, ConstS 4.0)))" = 
  (eval (Cond (IsZero (ConstS 0.0), Add (ConstS 5.0, ConstS 3.0), ScalProd (ConstS 2.0, ConstS 4.0))) = S (8.0))

let%test "eval (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))" = 
  (eval (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])) = S (1.5707963267948966))

let%test "eval (Mag (Add (ConstV [1.0; 2.0; 3.0], Inv (ConstV [4.0; 5.0; 6.0]))))" = 
  (eval (Mag (Add (ConstV [1.0; 2.0; 3.0], Inv (ConstV [4.0; 5.0; 6.0])))) = S (5.196152422706632))

let%test "eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0]))" = 
  (eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) = S (32.0))

let%test "eval (ScalProd (ConstS 2.0, DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))" = 
  (eval (ScalProd (ConstS 2.0, DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))) = S (0.0))

let%test "eval (Cond (IsZero (ConstV [0.0; 0.0; 0.0]), Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0)))" = 
  (eval (Cond (IsZero (ConstV [0.0; 0.0; 0.0]), Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0))) = S (3.0))

let%test "eval (Inv (ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (eval (Inv (ScalProd (ConstS 2.0, ConstS 3.0))) = S (-6.0))

let%test "eval (Mag (ConstV [3.0; 4.0]))" = 
  (eval (Mag (ConstV [3.0; 4.0])) = S (5.0))

let%test "eval (Cond (T, Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0)))" = 
  (eval (Cond (T, Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0))) = S (3.0))

let%test "eval (Cond (F, Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0)))" = 
  (eval (Cond (F, Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0))) = S (12.0))

let%test "eval (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))))" = 
  (eval (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0)))) = B true)

let%test "eval (IsZero (ConstV [0.0; 0.0; 0.0]))" = 
  (eval (IsZero (ConstV [0.0; 0.0; 0.0])) = B true)

let%test "eval (IsZero (ConstV [0.0; 0.0; 0.1]))" = 
  (eval (IsZero (ConstV [0.0; 0.0; 0.1])) = B false)

let%test "eval (Angle (ConstV [1.0; 1.0], ConstV [1.0; -1.0]))" = 
  (eval (Angle (ConstV [1.0; 1.0], ConstV [1.0; -1.0])) = S (1.5707963267948966))

let%test "eval (ScalProd (Mag (ConstV [3.0; 4.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))" = 
  (eval (ScalProd (Mag (ConstV [3.0; 4.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))) = S (0.0))

let%test "eval (Add (ConstS 1.0, ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (eval (Add (ConstS 1.0, ScalProd (ConstS 2.0, ConstS 3.0))) = S (7.0))

let%test "eval (Inv (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])))" = 
  (eval (Inv (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))) = S (-11.0))

let%test "eval (Cond (IsZero (ConstS 0.0), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (eval (Cond (IsZero (ConstS 0.0), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0))) = S (5.0))

let%test "eval (Cond (IsZero (ConstS 1.0), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (eval (Cond (IsZero (ConstS 1.0), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0))) = S (6.0))

let%test "eval (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0)))" = 
  (eval (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0))) = S (8.0))

let%test "eval (ScalProd (Mag (ConstV [3.0; 4.0]), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0])))" = 
  (eval (ScalProd (Mag (ConstV [3.0; 4.0]), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0]))) = S (-5.0))

let%test "eval (Cond (IsZero (Mag (ConstV [0.0; 0.0; 0.0])), Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0)))" = 
  (eval (Cond (IsZero (Mag (ConstV [0.0; 0.0; 0.0])), Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0))) = S (3.0))

let%test "eval (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))))" = 
  (eval (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))) = S (0.0))

let%test "eval (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (eval (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0))) = S (5.0))

let%test "eval (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0)))" = 
  (eval (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0))) = S (8.0))

let%test "eval (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0)))" = 
  (eval (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0))) = S (8.0))

(* Complex type_of test cases *)
let%test "type_of (Add (ConstS 5.0, ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (type_of (Add (ConstS 5.0, ScalProd (ConstS 2.0, ConstS 3.0))) = Scalar)

let%test "type_of (ScalProd (Mag (ConstV [3.0; 4.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))" = 
  (type_of (ScalProd (Mag (ConstV [3.0; 4.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0]))) = Scalar)

let%test "type_of (Cond (IsZero (ConstS 0.0), Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0)))" = 
  (type_of (Cond (IsZero (ConstS 0.0), Add (ConstS 1.0, ConstS 2.0), ScalProd (ConstS 3.0, ConstS 4.0))) = Scalar)

let%test "type_of (Angle (Add (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Add (ConstV [1.0; 1.0], Inv (ConstV [0.0; 1.0]))))" = 
  (type_of (Angle (Add (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Add (ConstV [1.0; 1.0], Inv (ConstV [0.0; 1.0])))) = Scalar)

let%test "type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0; 6.0]))))" = 
  (type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0; 6.0])))) = Scalar)

let%test "type_of (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0)))" = 
  (type_of (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0))) = Scalar)

let%test "type_of (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))" = 
  (type_of (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))) = Scalar)

let%test "type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0))) = Scalar)

let%test "type_of (ScalProd (Mag (Add (ConstV [1.0; 2.0; 3.0], Inv (ConstV [4.0; 5.0; 6.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0])))" = 
  (type_of (ScalProd (Mag (Add (ConstV [1.0; 2.0; 3.0], Inv (ConstV [4.0; 5.0; 6.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0]))) = Scalar)

let%test "type_of (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0)))" = 
  (type_of (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0))) = Scalar)

let%test "type_of (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))" = 
  (type_of (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))) = Scalar)

let%test "type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0; 6.0]))))" = 
  (type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0; 6.0])))) = Scalar)

let%test "type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0))) = Scalar)

let%test "type_of (Angle (Add (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Add (ConstV [1.0; 1.0], Inv (ConstV [0.0; 1.0]))))" = 
  (type_of (Angle (Add (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Add (ConstV [1.0; 1.0], Inv (ConstV [0.0; 1.0])))) = Scalar)

let%test "type_of (ScalProd (Mag (Add (ConstV [1.0; 2.0; 3.0], Inv (ConstV [4.0; 5.0; 6.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0])))" = 
  (type_of (ScalProd (Mag (Add (ConstV [1.0; 2.0; 3.0], Inv (ConstV [4.0; 5.0; 6.0]))), DotProd (ConstV [1.0; 0.0; -1.0], ConstV [0.0; -1.0; 1.0]))) = Scalar)

let%test "type_of (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0)))" = 
  (type_of (Cond (IsZero (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [0.0; 0.0; 0.0])), Add (ConstS 5.0, ConstS 3.0), ScalProd (Mag (ConstV [1.0; 1.0; 1.0]), ConstS 2.0))) = Scalar)

let%test "type_of (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))" = 
  (type_of (Inv (ScalProd (Mag (ConstV [1.0; 2.0; 2.0]), DotProd (ConstV [1.0; 0.0], ConstV [0.0; 1.0])))) = Scalar)

let%test "type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0; 6.0]))))" = 
  (type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0; 6.0])))) = Scalar)

let%test "type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0)))" = 
  (type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), ScalProd (ConstS 2.0, ConstS 3.0))) = Scalar)

let%test "type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), T))" = 
  (try
    let _ = type_of (Cond (IsZero (Add (ConstS 1.0, Inv (ConstS 1.0))), Mag (ConstV [3.0; 4.0]), T)) in false
  with
  | Wrong _ -> true
  | _ -> false)

let%test "type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0]))))" = 
  (try
    let _ = type_of (Mag (Add (ConstV [1.0; 2.0; 3.0], ScalProd (ConstS 2.0, ConstV [4.0; 5.0])))) in false
  with
  | Wrong _ -> true
  | _ -> false)