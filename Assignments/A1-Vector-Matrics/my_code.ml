type vector = float list;;

exception DimensionError;;

(* let v1 = [2.3;4.5;6.7];; *)

let rec create (n: int) (x: float) : vector = 
  if n < 1 then 
    raise DimensionError
  else
    match n with
    | 1 -> [x]
    | _ -> x::(create (n-1) x);;

let rec dim (vec_float : vector) : int = 
  match vec_float with
  | [] -> 0
  | x::xs -> 1 + dim xs;; 

(* PS : Is dimension checking to be done here ?  *)
let rec is_zero (v : vector) : bool = 
  match v with
  | [] -> true
  | x::xs -> if x = 0.0 then is_zero xs else false;; 

let rec unit (n : int) (j : int) : vector = 
  if j > n || j < 1 then 
    raise DimensionError
  else if j = 1 then
    1.0::(create (n-1) 0.0)
  else if j = n then
    (create (n-1) 0.0) @ [1.0]
  else
    (create (j-1) 0.0) @ [1.0] @ (create (n-j) 0.0);;
  ;;

let rec scale (c : float) (v : vector) : vector =
  match v with
  | [] -> []
  | x::xs -> (c*.x)::(scale c xs);;

let rec addv (v1 : vector) (v2 : vector) : vector =
  (* 
  
  This becomes redundant since mathcing will 
  take care of this case.
  
  if(dim v1 <> dim v2) then
    raise DimensionError
  else 

  *)
  match v1, v2 with
  | [], [] -> []
  | x1::xs1, x2::xs2 -> (x1 +. x2)::(addv xs1 xs2)
  | _ -> raise DimensionError;; 

  
let rec dot_prod (v1 : vector) (v2 : vector) : float = 
  match v1, v2 with
  | [], [] -> 0.0
  | x1::xs1, x2::xs2 -> (x1*.x2) +. (dot_prod xs1 xs2)
  | _ -> raise DimensionError;;

(* PS : Is dimension checking to be done here ?  *)
let rec inv (v : vector) : vector = 
  match v with
  | [] -> []
  | x::xs -> -1.0*.x::(inv xs);;

(* PS : Is dimension checking to be done here ?  *)
(* We would need to prove dot_prod v v >= 0.0 
   for correctness sake *)

let length (v : vector) : float =
  sqrt(dot_prod v v);;

let angle (v1 : vector) (v2 : vector) : float = 
  let v1_dot_v2 = dot_prod v1 v2 in
  let len_v1 = length v1 in
  let len_v2 = length v2 in
  let cos_theta = v1_dot_v2 /. (len_v1 *. len_v2) in
  (* Implementation of acos guarantees  that the angle returned 
  is the smaller one (theta <= pi radians) *)
  acos cos_theta;;


  
(* ========================================================================== *)
  
(* Extensive Testing of all the operations of vector module *)


(* ========================================================================== *)

(* Mathematical Proofs of the following properties of operations *)

(* 

(Commutativity)  u + v = v + u
(Associativity) u + (v + w) = (u + v) + w
(Identity of addition)  v + O = v
(Identity scalar)  1.v = v
(Annihilator scalar)  0.v = O
(Additive Inverse)  v + (- v) = O
(Scalar product combination)  b.(c.v) = (b.c).v
(Scalar sum-product distribution)  (b + c).v = b.v + c.v
(Scalar Distribution over vector sums)  b.(u + v) = b.u + b.v

State at least three other properties (especially with respect to the length, dot product, angle etc.) 

*)

(* ========================================================================== *)
