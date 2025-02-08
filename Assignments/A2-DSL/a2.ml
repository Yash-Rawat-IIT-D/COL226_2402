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
  | x::[] -> 1
  | x::xs -> 1 + dim xs
;; 
let rec is_legal_dim (vec_float : vector) : bool  = 
  match vec_float with
  | [] -> false             
  | x::xs -> true
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
let rec unit (n : int) (j : int) : vector = 
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
      | Vector n -> Scalar
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
      | Vector n -> Bool     
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

  let my_static_type_check = type_of e in

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
      | V v1, V v2 -> V (addv v1 v2)
      | _ -> raise (Wrong(e))
    )
  | Sub (e1, e2) ->
    (
      match (eval e1, eval e2) with
      | S s1, S s2 -> S (s1 -. s2)
      | V v1, V v2 -> V (addv v1 (inv v2))
      | _ -> raise (Wrong(e))
    )
  | Inv e1 ->
    (
      match eval e1 with
      | B b -> B (not b)
      | S s -> S (-1.0 *. s)
      | V v -> V (inv v)
    )
  | ScalProd (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | B b1, B b2 -> B (b1 && b2)
      | S s1, S s2 -> S (s1 *. s2)
      | S s, V v -> V (scale s v)
      | V v, S s -> V (scale s v)
      | _ -> raise (Wrong(e))
    )
  | DotProd (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | V v1, V v2 -> S (dot_prod v1 v2)
      | _ -> raise (Wrong(e))
    )
  | Mag e1 -> 
    (
      match eval e1 with
      | S s -> S (abs_float s)
      | V v -> S (length v)
      | _ -> raise (Wrong(e))
    )
  | Angle (e1, e2) -> 
    (
      match (eval e1, eval e2) with
      | V v1, V v2 -> S (angle v1 v2)
      | _ -> raise (Wrong(e))
    )
  | IsZero e1 -> 
    (
      match eval e1 with
      | S s -> B (f_eq_z s)
      | V v -> B (is_zero_close v)
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

let print_result (desc : string) (result : bool) : unit =
  Printf.printf "%s: %s\n" desc (if result then "Passed" else "Failed")
;;

(* Test cases for type_of function *)

let test_type_bool () = 
  let tests = [
    (T, Bool, "Type of T");
    (F, Bool, "Type of F");
    (Add (T, F), Bool, "Type of Add (T, F)");
    (Inv (T), Bool, "Type of Inv T");
    (ScalProd(Inv (T),Add(Inv (T),Inv (F))), Bool, "Type of ScalProd(Inv (T),Add(Inv (T),Inv (F)))");
    (ScalProd (T, F), Bool, "Type of ScalProd (T, F)");
    (IsZero (ConstV [0.0; 0.0; 0.0; 0.0]), Bool, "Type of IsZero (ConstV [0.0; 0.0; 0.0; 0.0])");
    ] in
  List.iter (fun (expr, expected_type, desc) ->
    let result = try type_of expr = expected_type with _ -> false in
    print_result desc result
  ) tests
;;

let test_type_scalar() = 
  let tests = [
    (ConstS 3.1415, Scalar, "Type of ConstS 3.14");
    (Add (ConstS 192.01, ConstS 57.44), Scalar, "Type of Add (ConstS 192.01, ConstS 57.44)");
    (Sub (Add (ConstS 192.01, ConstS 57.44), Add (ConstS 46.44, ConstS 341.76)), Scalar, "Type of Sub (Add (ConstS 192.01, ConstS 57.44), Add (ConstS 46.44, ConstS 341.76)) :");
    (Inv ( DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])), Scalar, "Type of Inv ( DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) : ");
    (Mag (ScalProd (ConstS 2.0, ConstV [1.0; 2.0])), Vector 2, "Type of ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
    (DotProd (ScalProd (ConstS 5.43, ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS 1.11)), Scalar, "DotProd (ScalProd (ConstS 5.43, ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS 1.11))) : ");
    (* (Mag (ConstV [3.0; 4.0]), Scalar, "Type of Mag (ConstV [3.0; 4.0])"); *)
    (Angle (ConstV [1.0; 2.0; 3.0], ConstV [-3.0; -4.0; 5.0]), Scalar, "Type ofAngle (ConstV [1.0; 2.0; 3.0], ConstV [-3.0; -4.0; 5.0]) : ");
    (IsZero (ConstS 0.0), Bool, "Type of IsZero (ConstS 0.0)");
    (Cond (T, ConstS 1.0, ConstS 2.0), Scalar, "Type of Cond (T, ConstS 1.0, ConstS 2.0)")
  ] in
  List.iter (fun (expr, expected_type, desc) ->
    let result = try type_of expr = expected_type with _ -> false in
    print_result desc result
  ) tests

  let test_type_vector () = 
    let tests = [
      (ConstV [1.0; 2.0; 3.0], Vector 3, "Type of ConstV [1.0; 2.0; 3.0]");
      (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Vector 2, "Type of Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
      (Sub (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Vector 2, "Type of Sub (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
      (Inv (ConstV [1.0; 2.0]), Vector 2, "Type of Inv (ConstV [1.0; 2.0])");
      (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), Vector 2, "Type of ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
      (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Scalar, "Type of DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
      (Mag (ConstV [3.0; 4.0]), Scalar, "Type of Mag (ConstV [3.0; 4.0])");
      (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Scalar, "Type of Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])");
      (IsZero (ConstV [0.0; 0.0]), Bool, "Type of IsZero (ConstV [0.0; 0.0])");
      (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Vector 2, "Type of Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])")
    ] in
    List.iter (fun (expr, expected_type, desc) ->
      let result = try type_of expr = expected_type with _ -> false in
      print_result desc result
    ) tests
  ;;

let test_type () =

  let tests = [
    (T, Bool, "Type of T");
    (F, Bool, "Type of F");
    (ConstS 3.14, Scalar, "Type of ConstS 3.14");
    (ConstV [1.0; 2.0; 3.0], Vector 3, "Type of ConstV [1.0; 2.0; 3.0]");
    (Add (ConstS 1.0, ConstS 2.0), Scalar, "Type of Add (ConstS 1.0, ConstS 2.0)");
    (Sub (ConstS 1.0, ConstS 2.0), Scalar, "Type of Sub (ConstS 1.0, ConstS 2.0)");
    (Inv (ConstS 1.0), Scalar, "Type of Inv (ConstS 1.0)");
    (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), Vector 2, "Type of ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
    (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Scalar, "Type of DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
    (Mag (ConstV [3.0; 4.0]), Scalar, "Type of Mag (ConstV [3.0; 4.0])");
    (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Scalar, "Type of Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])");
    (IsZero (ConstS 0.0), Bool, "Type of IsZero (ConstS 0.0)");
    (Cond (T, ConstS 1.0, ConstS 2.0), Scalar, "Type of Cond (T, ConstS 1.0, ConstS 2.0)")
  ] in
  List.iter (fun (expr, expected_type, desc) ->
    let result = try type_of expr = expected_type with _ -> false in
    print_result desc result
  ) tests
;;

(* Test cases for eval function *)
let test_eval () =
    let tests = [
    (T, B true, "Eval T");
    (F, B false, "Eval F");
    (ConstS 3.14, S 3.14, "Eval ConstS 3.14");
    (ConstV [1.0; 2.0; 3.0], V [1.0; 2.0; 3.0], "Eval ConstV [1.0; 2.0; 3.0]");
    (Add (ConstS 1.0, ConstS 2.0), S 3.0, "Eval Add (ConstS 1.0, ConstS 2.0)");
    (Sub (ConstS 1.0, ConstS 2.0), S (-1.0), "Eval Sub (ConstS 1.0, ConstS 2.0)");
    (Inv (ConstS 1.0), S (-1.0), "Eval Inv (ConstS 1.0)");
    (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), V [2.0; 4.0], "Eval ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
    (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), S 11.0, "Eval DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
    (Mag (ConstV [3.0; 4.0]), S 5.0, "Eval Mag (ConstV [3.0; 4.0])");
    (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), S (Float.pi /. 2.0), "Eval Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])");
    (IsZero (ConstS 0.0), B true, "Eval IsZero (ConstS 0.0)");
    (Cond (T, ConstS 1.0, ConstS 2.0), S 1.0, "Eval Cond (T, ConstS 1.0, ConstS 2.0)")
  ] in
  List.iter (fun (expr, expected_value, desc) ->
    let result = try eval expr = expected_value with _ -> false in
    print_result desc result
  ) tests
;;

(* Run all tests *)
let () =
  print_endline "===================================================================================";
  print_endline "Running type_bool tests:";
  test_type_bool();
  print_endline "===================================================================================";
  print_endline "Running type_scalar tests:";
  test_type_scalar();
  print_endline "===================================================================================";
  print_endline "Running eval tests:";
  test_type_vector();
  
  print_endline "===================================================================================";
  print_endline "Running eval tests:";
  test_eval ();
;;


(*===================================================================================*)

