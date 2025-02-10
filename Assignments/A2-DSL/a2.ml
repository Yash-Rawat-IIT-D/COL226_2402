(*===================================================================================*)
        (* Name: Yash Rawat || Roll No: 2023CS50334 || Assignment 2 || COL226 *)
(*===================================================================================*)

(*===================================================================================*)
                            (* Float Checking Zero Code*)
(*===================================================================================*)

let my_epsilon = 0.000001;;

let f_eq_z (f : float) : bool = 
  abs_float(f) < my_epsilon
;; 

(*===================================================================================*)
                                (* Boolean ADT Code*)
(*===================================================================================*)

type myBool =
  | T
  | F
;;

let my_not (b : myBool) : myBool = 
  match b with
  | T -> F
  | F -> T
;;
let my_and (b1 : myBool) (b2 : myBool) : myBool = match b1 with
  | T -> b2
  | F -> F
;;
let my_or (b1 : myBool) (b2 : myBool) : myBool = match b1 with
  | T -> T
  | F -> b2
;;
let my_xor (b1 : myBool) (b2 : myBool) : myBool = match b1 with
  | T -> my_not b2
  | F -> b2
;;
let myBool_to_bool (b : myBool) : bool = match b with
  | T -> true
  | F -> false
;;
let bool_to_myBool (b : bool) : myBool = match b with
  | true -> T
  | false -> F
;;

(*===================================================================================*)
                        (* Vector ADT Code from Assignment 1 *)
(*===================================================================================*)

type vector = float list;;

exception DimensionError;;
exception ZeroVectorError;;

let rec create (n: int) (x: float) : vector = 
  if n < 1 then 
    raise DimensionError
  else
    match n with
    | 1 -> x::[]
    | _ -> x::(create (n-1) x)
;;
let rec dim (vec_float : vector) : int = 
  match vec_float with
  | [] -> raise DimensionError  
  | _::[] -> 1
  | _::xs -> 1 + dim xs
;; 
let is_legal_dim (vec_float : vector) : bool  = 
  match vec_float with
  | [] -> false             
  | _::_ -> true
;;
let rec is_zero (v : vector) : bool = 
  if(not (is_legal_dim v)) then
    raise DimensionError
  else
    match v with
    | [] -> raise DimensionError 
    | 0.0 :: [] -> true
    | x::xs -> if x = 0.0 then is_zero xs else false
;; 
let rec is_zero_close (v : vector) : bool = 
  if(not (is_legal_dim v)) then
    raise DimensionError
  else
    match v with
    | [] -> raise DimensionError 
    | x :: [] -> if (f_eq_z x) then true else false
    | x::xs -> if (f_eq_z x) then is_zero_close xs else false
let unit (n : int) (j : int) : vector = 
  if(n < 1) then 
    raise DimensionError
  else if (j > n || j < 1) then 
    raise DimensionError
  else if j = 1 then
    if(n = 1) then
      1.0::[]
    else
       1.0::(create (n-1) 0.0)
  else if j = n then
    if(n = 1) then    
      1.0::[]
    else
      (create (n-1) 0.0) @ (1.0::[])
  else
    (create (j-1) 0.0) @ (1.0::[]) @ (create (n-j) 0.0);;
;;
let rec scale (c : float) (v : vector) : vector =
  if(not (is_legal_dim v) ) then
    raise DimensionError
  else
    match v with
    | [] -> raise DimensionError           
    | x1::[] -> c *. x1 :: []
    | x::xs -> (c *. x) :: (scale c xs)
;;
let rec addv (v1 : vector) (v2 : vector) : vector =
  match v1, v2 with
  | x1::[], x2::[] -> (x1 +. x2)::[]
  | x1::xs1, x2::xs2 -> (x1 +. x2)::(addv xs1 xs2)
  | _ -> raise DimensionError                     
;;                         
let rec dot_prod (v1 : vector) (v2 : vector) : float = 
  match v1, v2 with
  | x1::[], x2::[] -> x1 *. x2
  | x1::xs1, x2::xs2 -> (x1*.x2) +. (dot_prod xs1 xs2)
  | _ -> raise DimensionError                                   
;;                             
let rec inv (v : vector) : vector = 
  if(not (is_legal_dim v)) then
    raise DimensionError
  else  
    match v with
    | [] -> raise DimensionError 
    | x::[] -> (-1.0 *.x)::[]
    | x::xs -> -1.0*.x::(inv xs)
;;
let length (v : vector) : float =
  sqrt(dot_prod v v)
;;
let in_domain_acos (cos_theta:float) : float = 
  if(cos_theta > 1.0) then
    1.0
  else if(cos_theta < -1.0) then
    -1.0
  else
    cos_theta 
;;
let angle (v1 : vector) (v2 : vector) : float = 
  let v1_dot_v2 = dot_prod v1 v2 in
  if(is_zero v1 || is_zero v2) then
    raise ZeroVectorError 
  else
    let len_v1 = length v1 in
    let len_v2 = length v2 in
    let cos_theta = v1_dot_v2 /. ( len_v1 *. len_v2) in 
    let s_cos_theta = in_domain_acos cos_theta in
    acos s_cos_theta
;;


(*===================================================================================*)
    (* Expression Definition | Defitnitional Interpreter | Types and Type Checking*)
(*===================================================================================*)

type expr = 
  | T
  | F
  | ConstS of float                   (* Scalar Constant *)
  | ConstV of vector                  (* Vector Constant *)
  | Add of expr * expr     
  | Sub of expr * expr         
  | Inv of expr
  | ScalProd of expr * expr
  | DotProd of expr * expr
  | Mag of expr
  | Angle of expr * expr
  | IsZero of expr
  | Cond of expr * expr * expr
;;

type types = 
  | Bool
  | Scalar 
  | Vector of int (* n Dimesnional vector of float types *)
;;

exception DimensionMismatch;;
exception TypeMismatch of expr * string;;
exception Wrong of expr ;;

type values = 
| B of bool
| S of float 
| V of vector
;;

(* Placeholder for Error Specifications *)
exception Foo;;

(*===================================================================================*)
                          (* Static Type Checking of Expression e *)
(*===================================================================================*)

let rec type_of (e : expr) : types = 
  match e with
  | T -> Bool
  | F -> Bool
  | ConstS _ -> Scalar
  | ConstV v -> 
    (
      try 
        let n = dim v in
        Vector n
      with _ -> raise (Wrong(e))
    )
  | Add (e1, e2) -> 
    (
      match (type_of e1, type_of e2) with
      | Bool, Bool -> Bool
      | Scalar, Scalar -> Scalar
      | Vector n, Vector m -> if n = m then Vector n else raise(Wrong(e))
      | _ -> raise (Wrong(e))
    )
  | Sub (e1, e2) -> 
    (
      match (type_of e1, type_of e2) with
      (* | Bool, Bool -> Bool *)
      | Scalar, Scalar -> Scalar
      | Vector n, Vector m -> if n = m then Vector n else raise(Wrong(e))
      | _ -> raise(Wrong(e))
    )
  | Inv e1 -> 
    ( 
      match type_of e1 with
      | Bool -> Bool
      | Scalar -> Scalar
      | Vector n -> Vector n
    )
    
  | ScalProd (e1, e2) -> 
    (
      match (type_of e1, type_of e2) with
      | Bool, Bool -> Bool
      | Scalar, Scalar -> Scalar
      | Scalar, Vector n -> Vector n
      | Vector n, Scalar -> Vector n
      | _ -> raise (Wrong(e))
    )

  | DotProd (e1, e2) -> 
    (
      match (type_of e1, type_of e2) with
      | Vector n, Vector m -> if n = m then Scalar else raise(Wrong(e))
      | _ -> raise(Wrong(e))
    )

  | Mag e1 -> 
    (
      match (type_of e1) with
      | Scalar -> Scalar
      | Vector _ -> Scalar
      | _ -> raise (Wrong(e))
    )

  | Angle (e1, e2) -> 
    (
      match (type_of e1, type_of e2) with
      | Vector n, Vector m -> if n = m then Scalar else raise(Wrong(e))
      | _ -> raise (Wrong(e))
    )

  | IsZero e1 -> 
    (
      match type_of e1 with
      | Bool -> Bool
      | Scalar -> Bool
      | Vector _ -> Bool     
    )

  | Cond (e1, e2, e3) -> 
    (
      match (type_of e1) with
      | Bool -> 
        (
          let e2_type = type_of e2 in
          let e3_type = type_of e3 in
            if(e2_type = e3_type) then
              e2_type
            else
              raise (Wrong(e))        

        )
      | _ -> raise (Wrong(e))
    )

;;

(*===================================================================================*)
                          (* Evaluation of Expression e *)
(*===================================================================================*)


let rec eval (e : expr) : values = 

  let _ = type_of e in

  match e with
  | T -> B true
  | F -> B false
  | ConstS s -> S s
  | ConstV v -> V v
  | Add (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | B b1, B b2 -> B (b1 || b2)
      | S s1, S s2 -> S (s1 +. s2)
      | V v1, V v2 -> 
        (
          try
            V (addv v1 v2)
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | Sub (e1, e2) ->
    (
      match (eval e1, eval e2) with
      | S s1, S s2 -> S (s1 -. s2)
      | V v1, V v2 -> 
        (
          try  
            V (addv v1 (inv v2))
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | Inv e1 ->
    (
      match eval e1 with
      | B b -> B (not b)
      | S s -> S (-1.0 *. s)
      | V v -> 
        (
          try
            V (inv v)
          with _ -> raise (Wrong(e))
        )
    )
  | ScalProd (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | B b1, B b2 -> B (b1 && b2)
      | S s1, S s2 -> S (s1 *. s2)
      | S s, V v -> 
        (
          try
            V (scale s v)
          with _ -> raise (Wrong(e))
        )
      | V v, S s -> 
        (
          try
            V (scale s v)
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | DotProd (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | V v1, V v2 -> 
        (
          try
            let dot_prod_val = dot_prod v1 v2 in
            S dot_prod_val
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | Mag e1 -> 
    (
      match eval e1 with
      | S s -> S (abs_float s)
      | V v -> 
        ( try 
            let v_mag = length v in
            S v_mag
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | Angle (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | V v1, V v2 -> 
        (
          try
            let theta_v1_v2 = angle v1 v2 in   
            S theta_v1_v2
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | IsZero e1 -> 
    (
      match eval e1 with
      | S s -> B (f_eq_z s)
      | V v -> 
        (
          try
            let bval = is_zero v in
            B bval
          with _ -> raise (Wrong(e))
        )
      | _ -> raise (Wrong(e))
    )
  | Cond (e1, e2, e3) -> 
    (
      match eval e1 with
      | B b -> if b = true then eval e2 else eval e3
      | _ -> raise (Wrong(e))  
    )
;;


(*===================================================================================*)

(*===================================================================================*)


