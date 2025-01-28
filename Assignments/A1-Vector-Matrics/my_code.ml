type vector = float list;;

exception DimensionError;;
exception ZeroVectorError;;

let rec create (n: int) (x: float) : vector = 
  if n < 1 then 
    raise DimensionError
  else
    match n with
    | 1 -> x::[]
    | _ -> x::(create (n-1) x);;


let rec create_tail_rec (n: int) (x:float) : vector = 
  let rec create_tail_rec_helper (n: int) (x: float) (acc: vector) : vector = 
    if n < 1 then 
      raise DimensionError
    else
      match n with
      | 1 -> x::acc
      | _ -> create_tail_rec_helper (n-1) x (x::acc)
  in
  create_tail_rec_helper n x [];;

let rec dim (vec_float : vector) : int = 
  match vec_float with
  | [] -> raise DimensionError  (* Not the base case if we assume that a valid vector of dimension >= 1 is being passed to dim *)
  | x::[] -> 1
  | x::xs -> 1 + dim xs;; 

let rec dim_tail_rec (vec_float : vector) : int = 
  let rec dim_tail_rec_helper (vec_float : vector) (acc : int) : int = 
    match vec_float with
    | [] -> acc
    | x::[] -> acc + 1
    | x::xs -> dim_tail_rec_helper xs (acc + 1)
  in
  dim_tail_rec_helper vec_float 0;;

let rec is_legal_dim (vec_float : vector) : bool  = 
  match vec_float with
  | [] -> false             (* Redundant if we assume that a valid vector of dimension >= 1 is being passed to dim *)
  | x::xs -> true

;;
(* PS : Is dimension checking to be done here ?  *)
let rec is_zero (v : vector) : bool = 
  if(not (is_legal_dim v)) then
    raise DimensionError
  else
    match v with
    | [] -> raise DimensionError (*This condition is added for completness of matching and would we be triggered before *)
    | 0.0 :: [] -> true
    | x::xs -> if x = 0.0 then is_zero xs else false;; 


let rec is_zero_tail_rec (v : vector) : bool = 
  if(not (is_legal_dim v)) then
    raise DimensionError
  else
    let rec is_zero_tail_rec_helper (v : vector) : bool = 
      match v with
      | [] -> raise DimensionError
      | 0.0 :: [] -> true
      | x::xs -> if x = 0.0 then is_zero_tail_rec_helper xs else false
    in
    is_zero_tail_rec_helper v;;

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
    if(n = 1) then    (* Redundant since if n = 1 then j = 1, earlier if condition swill trigger *)
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
    | [] -> raise DimensionError (*This condition is added for completness of matching and would we be triggered before *)          
    | x1::[] -> c *. x1 :: []
    | x::xs -> (c *. x) :: (scale c xs)
;;

let rec addv (v1 : vector) (v2 : vector) : vector =
  (* 
  
  This becomes redundant since mathcing will 
  take care of this case.
  
  if(dim v1 <> dim v2) then
    raise DimensionError
  else 

  *)
  match v1, v2 with
  | x1::[], x2::[] -> (x1 +. x2)::[]
  | x1::xs1, x2::xs2 -> (x1 +. x2)::(addv xs1 xs2)
  | _ -> raise DimensionError;; 
  
let rec dot_prod (v1 : vector) (v2 : vector) : float = 
  match v1, v2 with
  | x1::[], x2::[] -> x1 *. x2
  | x1::xs1, x2::xs2 -> (x1*.x2) +. (dot_prod xs1 xs2)
  | _ -> raise DimensionError;;

(* PS : Is dimension checking to be done here ?  *)
let rec inv (v : vector) : vector = 
  if(not (is_legal_dim v)) then
    raise DimensionError
  else  
    match v with
    | [] -> raise DimensionError (*This condition is added for completness of matching and would we be triggered before *)
    | x::[] -> (-1.0 *.x)::[]
    | x::xs -> -1.0*.x::(inv xs);;


(* PS : Is dimension checking to be done here ?  *)
(* We would need to prove dot_prod v v >= 0.0 
   for correctness sake *)

let length (v : vector) : float =
  sqrt(dot_prod v v);;

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
  let len_v1 = length v1 in
  let len_v2 = length v2 in
  if(len_v1 == 0.0 || len_v2 == 0.0) then
    raise ZeroVectorError 
  else
    let cos_theta = v1_dot_v2 /. (len_v1 *. len_v2) in
    (* Implementation of acos guarantees  that the angle returned 
    is the smaller one (theta <= pi radians) *)
    
    
    let s_cos_theta = in_domain_acos cos_theta in
    acos s_cos_theta
;;
  
(* ========================================================================== *)
  
          (* Extensive Testing of all the operations of vector module *)

(* ========================================================================== *)

(* Test Cases for Create *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of create : \n";;
Printf.printf "==========================================================================\n";;

let n = 5;;
let create_tc_0 = create n 3.14;;

(* Expected Output TC_1 : [3.14, 3.14, 3.14, 3.14, 3.14] *)
List.iter (Printf.printf "%0.2f ") create_tc_0;;
Printf.printf "\n";;

let n = 1;;

let create_tc_1 = create n 2.71;;
(* Expected Output TC_1 : [2.71] *)

List.iter (Printf.printf "%0.2f ") create_tc_1;;
Printf.printf "\n";;

let n = 3;;
let create_tc_2 = create n 1.0;;
(* Expected Output TC_2 : [1.0, 1.0, 1.0] *)

List.iter (Printf.printf "%0.2f ") create_tc_2;;
Printf.printf "\n";;

let n = 4;;
let create_tc_3 = create n (-2.5);;
(* Expected Output TC_3 : [-2.5, -2.5, -2.5, -2.5] *)

List.iter (Printf.printf "%0.2f ") create_tc_3;;
Printf.printf "\n";;

try
  let n = 0 in
  let _ = create n 2.71 in
  Printf.printf "Test Case 4 Failed - Error Not Detected \n"
with
| DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n"
;;



(* ========================================================================== *)

(* Test Cases for dimension *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of Dimension : \n";;
Printf.printf "==========================================================================\n";;

let test_dim () =
  let vec1 = [1.782; 2.441; 3.761] in
  let vec2 = [9.11; 9.89; 8.91] in
  let vec3 = [9.11; 9.59; 9.28] in
  let vec4 = [5.0; 6.0; 7.0; 8.0] in

  (* Expected Output: 3 *)
  let dim_tc_1 = dim vec1 in
  Printf.printf "dim_tc_1: %d\n" dim_tc_1;

  (* Expected Output: 1 *)
  let dim_tc_2 = dim vec2 in
  Printf.printf "dim_tc_2: %d\n" dim_tc_2;

  (* Expected Output: 0 *)
  let dim_tc_3 = dim vec3 in
  Printf.printf "dim_tc_3: %d\n" dim_tc_3;

  (* Expected Output: 4 *)
  let dim_tc_4 = dim vec4 in
  Printf.printf "dim_tc_4: %d\n" dim_tc_4;


  (* Testing for DimensionError *)
  try
    let _ = dim [] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n"
;;

test_dim ();;



(* ========================================================================== *)

(* Test Cases for dimension *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of is_Zero : \n";;
Printf.printf "==========================================================================\n";;

let test_is_zero () =
  let vec1 = [0.0; 0.0; 0.0] in
  let vec2 = [0.0; 0.0; 0.0002] in
  let vec3 = [1.412; 3.31; 0.332] in
  let vec4 = [0.0; 0.0; 0.0; 0.0] in

  (* Expected Output: true *)
  let is_zero_tc_1 = is_zero vec1 in
  Printf.printf "is_zero_tc_1: %b\n" is_zero_tc_1;

  (* Expected Output: false *)
  let is_zero_tc_2 = is_zero vec2 in
  Printf.printf "is_zero_tc_2: %b\n" is_zero_tc_2;

  (* Expected Output: false *)
  let is_zero_tc_3 = is_zero vec3 in
  Printf.printf "is_zero_tc_3: %b\n" is_zero_tc_3;

  (* Expected Output: true *)
  let is_zero_tc_4 = is_zero vec4 in
  Printf.printf "is_zero_tc_4: %b\n" is_zero_tc_4;

  (* Testing for DimensionError *)
  try
    let _ = is_zero [] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n"
;;

test_is_zero ();;

(* ========================================================================== *)

(* Test Cases for unit *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of unit : \n";;
Printf.printf "==========================================================================\n";;

let test_unit () =
  (* Normal cases *)
  let unit_tc_1 = unit 5 3 in
  (* Expected Output: [0.0; 0.0; 1.0; 0.0; 0.0] *)
  List.iter (Printf.printf "%0.2f ") unit_tc_1;
  Printf.printf "\n";

  let unit_tc_2 = unit 4 1 in
  (* Expected Output: [1.0; 0.0; 0.0; 0.0] *)
  List.iter (Printf.printf "%0.2f ") unit_tc_2;
  Printf.printf "\n";

  let unit_tc_3 = unit 6 6 in
  (* Expected Output: [0.0; 0.0; 0.0; 0.0; 0.0; 1.0] *)
  List.iter (Printf.printf "%0.2f ") unit_tc_3;
  Printf.printf "\n";

  let unit_tc_4 = unit 1 1 in
  (* Expected Output: [1.0] *)
  List.iter (Printf.printf "%0.2f ") unit_tc_4;
  Printf.printf "\n";

  (* Edge cases for Dimension Error *)
  try
    let _ = unit 0 1 in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = unit 5 0 in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = unit 5 6 in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";
;;

test_unit ();;


(* ========================================================================== *)

(* Test Cases for scale *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of scale : \n";;
Printf.printf "==========================================================================\n";;

let test_scale () =
  (* Normal cases *)
  let scale_tc_1 = scale 2.0 [1.0; 2.0; 3.0] in
  (* Expected Output: [2.0000; 4.0000; 6.0000] *)
  List.iter (Printf.printf "%0.4f ") scale_tc_1;
  Printf.printf "\n";

  let scale_tc_2 = scale (-1.78) [2.21; -3.14; 4.32] in
  (* Expected Output: [-3.9338; 5.5892; -7.6896] *)
  List.iter (Printf.printf "%0.4f ") scale_tc_2;
  Printf.printf "\n";

  let scale_tc_3 = scale 0.0 [1.0; 2.0; 3.0] in
  (* Expected Output: [0.0000; 0.0000; 0.000] *)
  List.iter (Printf.printf "%0.4f ") scale_tc_3;
  Printf.printf "\n";

  let scale_tc_4 = scale 9.57 [1.22; 2.45; 3.33] in
  (* Expected Output: [11.6754; 23.4465; 31.8681] *)
  List.iter (Printf.printf "%0.4f ") scale_tc_4;
  Printf.printf "\n";

  (* Edge cases for Dimension Error *)
  try
    let _ = scale 2.44 [] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";
;;

test_scale ();;


(* ========================================================================== *)

(* Test Cases for addv *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of addv : \n";;
Printf.printf "==========================================================================\n";;

let test_addv () =
  (* Normal cases *)
  let addv_tc_1 = addv [1.1234; 2.2345; 3.3456; 4.4567; 5.5678; 6.6789] [7.7890; 8.8901; 9.9012; 10.0123; 11.1234; 12.2345] in
  (* Expected Output: [8.9124; 11.1246; 13.2468; 14.4690; 16.6912; 18.9134] *)
  List.iter (Printf.printf "%0.4f ") addv_tc_1;
  Printf.printf "\n";

  let addv_tc_2 = addv [0.0; 0.0; 0.0; 0.0; 0.0; 0.0] [1.1111; 2.2222; 3.3333; 4.4444; 5.5555; 6.6666] in
  (* Expected Output: [1.1111; 2.2222; 3.3333; 4.4444; 5.5555; 6.6666] *)
  List.iter (Printf.printf "%0.4f ") addv_tc_2;
  Printf.printf "\n";

  let addv_tc_3 = addv [1.2345; 2.3456; 3.4567; 4.5678; 5.6789; 6.7890] [-1.2345; -2.3456; -3.4567; -4.5678; -5.6789; -6.7890] in
  (* Expected Output: [0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000] *)
  List.iter (Printf.printf "%0.4f ") addv_tc_3;
  Printf.printf "\n";

  let addv_tc_4 = addv [9.8765; 8.7654; 7.6543; 6.5432; 5.4321; 4.3210] [0.1234; 0.2345; 0.3456; 0.4567; 0.5678; 0.6789] in
  (* Expected Output: [9.9999; 8.9999; 7.9999; 6.9999; 5.9999; 4.9999] *)
  List.iter (Printf.printf "%0.4f ") addv_tc_4;
  Printf.printf "\n";

  (* Edge cases for Dimension Error *)
  try
    let _ = addv [1.0; 2.1; 3.2; 4.3; 5.4; 6.5] [1.0; 2.0; 3.0] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = addv [0.0] [3.1415; 2.7182; 1.618] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = addv [1.0; 2.0; 3.0] [] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = addv [] [1.0; 2.0; 3.0] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";
;;

test_addv ();;

(* Test Cases for addv *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of addv : \n";;
Printf.printf "==========================================================================\n";;




(* ========================================================================== *)

(* ========================================================================== *)

(* Test Cases for dot_prod *)
Printf.printf "==========================================================================\n";;
Printf.printf "Testing implementation of dot_prod : \n";;
Printf.printf "==========================================================================\n";;

let test_dot () =
  (* General cases *)
  let dot_tc_1 = dot_prod [1.1234; 2.2345; 3.3456; 4.4567] [4.5678; 3.4567; 2.3456; 1.2345] in
  (* Expected Output: 26.2047 *)
  Printf.printf "dot_tc_1: %0.4f\n" dot_tc_1;

  let dot_tc_2 = dot_prod [1.5678; 2.6789; 3.7890; 4.8901; 5.9012] [5.0123; 4.1234; 3.2345; 2.3456; 1.4567] in
  (* Expected Output: 51.2265 *)
  Printf.printf "dot_tc_2: %0.4f\n" dot_tc_2;

  let dot_tc_3 = dot_prod [0.0000; 0.0000; 0.0000; 0.0000; 0.0000; 0.0000] [1.1234; 2.2345; 3.3456; 4.4567; 5.5678; 6.6789] in
  (* Expected Output: 0.0000 *)
  Printf.printf "dot_tc_3: %0.4f\n" dot_tc_3;

  let dot_tc_4 = dot_prod [1.1111; 2.2222; 3.3333; 4.4444; 5.5555; 6.6666] [6.6666; 5.5555; 4.4444; 3.3333; 2.2222; 1.1111] in
  (* Expected Output: 69.1344 *)
  Printf.printf "dot_tc_4: %0.4f\n" dot_tc_4;

  (* Edge cases for Dimension Error *)
  try
    let _ = dot_prod [1.0; 2.0; 3.0] [1.0; 2.0] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = dot_prod [1.0; 2.0; 3.0] [] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = dot_prod [] [1.0; 2.0; 3.0] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";

  try
    let _ = dot_prod [] [] in
    Printf.printf "Test Case Failed - Error Not Detected \n"
  with
  | DimensionError -> Printf.printf "Test Case Passed - DimensionError Detected \n";
;;

test_dot ();;




(* 

======================================================================================================

                  Mathematical Proofs of the following properties of operations 

======================================================================================================



======================================================================================================

In our implementation, vectors are essentially an alias for lists of floats
Since our module uses recursive functions, we will try to prove the properties of our 
module using structural induction on the vectors 

An intersting property of our module is that the vectors have dimensions = n >= 1
Thus the base case of our induc


======================================================================================================

Some common abbreviations used in the proofs :

  1) IH : Inductive Hypothesis
  2) O  : The zero vector of appropriate dimension n >=1 i.e. O = [0.0, 0.0, 0.0, ..., 0.0] (n times)

!!! Note that addition of two vectors u and v is defined as per the module's implentation only and
!!! proof's have been given for the same (Using + to denote the same to avoid crumbersome poofs).

!!! Similary for scalar multiplication, we re-use * to denote scalar multiplication of vectors and is defined 
!!! as per the module's implentation only and proof's have been given for the same.

!!! Since OCaml does not support operator overloading , *. and +. are used to denote float multiplication and addition
!!! in the syntax of OCaml but for the sake of clarity and brevity, we will use * and + to denote the same in the proofs


!!! Appropriate brackets have been used to preserve the order of operations in the proofs

Thus in proofs : u + v = addv u v , u + (v + w) =  addv u (addv v w), (u+v)+w = addv (addv u v) w , etc. 
                
                Similarly for scalar multiplication (here b and c are scalar and v is a vector of floats)         
               : c * v = scale c v,  b * (c * v) = scale b (scale c v), (b*c) * v = scale b*c v etc.

!!! For most base cases : x::[] = [x] has been used directly (implied by the cons operator in OCaml)

======================================================================================================

Important Note : Dimension are of vectors are preserved upon addition and scalar multiplication:

Claim : For any vectors u,v such that dim V = dim U = n , dim(u + v) = n

Proof : Proof By Structural Induction induction on the structure of the vectors u and v

        Let Inductive Hypothesis be : For any vectors u,v such that dim V = dim U = n >= 1, dim(u + v) = n
        
        Base Case : u = [u0], v = [v0] 
          
          Then we have : dim(u + v) = dim([u0 + v0]) = 1 = n
          Hence, the claim holds for the base case  ------ (1)

        Inductive Step : Suppose IH holds for some vecors xu, xv such that dim xu = dim xv = n > 1
          
          Then consider : u = u0 :: xu, v = v0 :: xv, where v0 is a float and head of vector v and similarly for u0
          
          Then we have : dim(u + v) = dim([u0 + v0] :: (xu + xv)) = 1 + dim(xu + xv) = 1 + n = n
          
          Hence, the IH holds for vectors u and v of form u0 :: xu and v0 :: xv if
          if it holds for xu and xv, where dim xu = dim xv = n > 1
          (Note that dim u = 1 + dim xu = 1 + n = 1 + dim xv = dim v , by the definition of dim in our module)
          
          ---- (2)

        By (1),(2) and Structural Induction, we have proved that the IH holds i.e. 
        for any vectors u,v such that dim V = dim U = n , dim(u + v) = n for the addv operation
        of our module

        Thereby proving the claim that dimensions are preserved upon addition of vectors

Claim : For any vector v and scalar c, such that dim v = n, dim(scale (c v)) = n

Proof : Proof By Structural Induction induction on the structure of the vector v

        Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, dim(scale (c v)) = n
        
        Base Case : v = [v0]
          
          Then we have : dim(scale (c v)) = dim([c v0]) = 1 = n
          Hence, the claim holds for the base case  ------ (1)

        Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
          
          Then consider : v = v0 :: xv, where v0 is a float and head of vector v
          
          Then we have : dim(scale (c v)) = dim([c v0] :: scale (c xv)) = 1 + dim(scale (c xv)) = 1 + n = n
          
          Hence, the IH holds for vector v of form v0 :: xv if
          if it holds for xv, where dim xv = n > 1 - (Note that dim v = 1 + dim xv = 1 + n 
          = dim v , by the definition of dim in our module) ----- (2)

        By (1),(2) and Structural Induction, we have proved that the IH holds i.e. 
        for any vector v and scalar c, such that dim v = n, dim(scale (c v)) = n for the scale operation
        of our module

        Thereby proving the claim that dimensions are preserved upon scalar multiplication of vectors


====================================================================================================================

                                  Proof of Commutativity of Vector Addition : 

====================================================================================================================

Claim : For any vectors u,v such that dim V = dim U = n , u + v = v + u
        (As mentioned u + v is explicitly defined as : add u v , in our module
         and similarly for v + u : add v u, in our module)

Proof : Proof By Structural Induction induction on the structure of the vectors u and v
        
        Let Inductive Hypothesis be : For any vectors u,v such that dim V = dim U = n >= 1, u + v = v + u
        
        Base Case : u = [u0], v = [v0]
          
          Then we have : u + v = [u0 + v0] = [v0 + u0] = v + u (Since addition is commutative on floats)
          Hence, the claim holds for the base case  ------ (1)

        Inductive Step : Suppose IH holds for some vecors xu, xv such that dim xu = dim xv = n > 1
          
          Then consider : u = u0 :: xu, v = v0 :: xv, where v0 is a float and head of vector v and similarly for u0
          
          Then we have : u + v = [u0 + v0] :: (xu + xv)
                               = [v0 + u0] :: (xv + xu) (By IH and commutatvity of addition on floats)
                               = v + u
          
          Hence, the IH holds for vectors u and v of form u0 :: xu and v0 :: xv if
          if it holds for xu and xv, where dim xu = dim xv = n > 1
          (Note that dim u = 1 + dim xu = 1 + n = 1 + dim xv = dim v , 
          by the definition of dim in our module) ------- (2)

        By (1),(2) and Structural Induction, we have proved that the IH holds i.e. 
        for any vectors u,v such that dim V = dim U = n , u + v = v + u  ( for the addv operation of our module)
        Thereby proving the claim of commutativity of vector addition
        

==================================================================================================================== 

                                Proof of Associativity of Vector Addition :

====================================================================================================================

Claim : For any vectors u,v,w such that dim V = dim U = dim W = n >= 1, u + (v + w) = (u + v) + w
        (As mentioned u + (v + w) is explicitly defined as : add (u) (add v w) , in our module
         and similarly for (u + v) + w is explicitly : add (add u v) (w), in our module)

Proof : Proof By Structural Induction on the structure of the vectors u, v, and w

  Let Inductive Hypothesis be : For any vectors u, v, w such that 
                                dim u = dim v = dim w = n >= 1, u + (v + w) = (u + v) + w
  
  Base Case : u = [u0], v = [v0], w = [w0]
    
    Then we have : u + (v + w) = [u0] + ([v0] + [w0])
            = [u0] + [v0 + w0] (By definition of vector addition)
            = [u0 + (v0 + w0)] (By definition of vector addition)
            = [(u0 + v0) + w0] (By associativity of addition on floats)
            = [u0 + v0] + [w0] (By definition of vector addition in reverse)
            = ([u0] + [v0]) + [w0] (By definition of vector addition in reverse)
            = (u + v) + w
    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vectors xu, xv, xw such that dim xu = dim xv = dim xw = n > 1
    
    Then consider : u = u0 :: xu, v = v0 :: xv, w = w0 :: xw, where v0 is a float and head of vector v and similarly for u0 and w0
    
    Then we have : u + (v + w) = [u0 :: xu] + ([v0 :: xv] + [w0 :: xw])
            = [u0 :: xu] + ([v0 + w0] :: (xv + xw)) (By definition of vector addition)
            = [u0 + (v0 + w0)] :: (xu + (xv + xw)) (By definition of vector addition)
            = [(u0 + v0) + w0] :: ((xu + xv) + xw) (By IH and associativity of addition on floats)
            = ([u0 + v0] :: (xu + xv)) + [w0 :: xw] (By definition of vector addition in reverse)
            = ([u0 :: xu] + [v0 :: xv]) + [w0 :: xw] (By definition of vector addition in reverse)
            = (u + v) + w
    
    Hence, the IH holds for vectors u, v, and w of form u0 :: xu, v0 :: xv, and w0 :: xw if
    it holds for xu, xv, and xw, where dim xu = dim xv = dim xw = n > 1 (Note that dim u = 1 + dim xu = 1 + n 
    = 1 + dim xv = dim v = 1 + dim(xw) = dim w, by the definition of dim in our module) ----- (2)

  By (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vectors u, v, w such that dim u = dim v = dim w = n, u + (v + w) = (u + v) + w 

  Thereby proving the claim of associativity of vector addition


==================================================================================================================== 

                                        Proof of Identity of Addition : 

====================================================================================================================

Claim : For any vector v such that dim V = n >= 1, v + O = v , where O is the zero vector of dimension n

Proof : Proof By Structural Induction on the structure of the vector v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, v + O_{n} = v
  (Note - We will use O_{n} to denote the zero vector of dimension n for the sake of clarity)
  
  Base Case : v = [v0]
    
    Then we have : v + O = [v0] + [0.0] = [v0 + 0.0] = [v0] = v (Since 0.0 is the additive identity for floats)
    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : v + O_{n+1} = [v0 :: xv] + [0.0 :: O_{n}] 
                               = [v0 + 0.0] :: (xv + O_{n})  (by definition of vector addition in our module)
                               = [v0] :: xv = v (By IH and the fact that 0.0 is the additive identity for floats)
    
    ---- (By IH and definition of vector addition, as well as definition of dim in our module)


    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) ----- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v such that dim V = n >= 1, v + O = v 

  Thereby proving the claim of identity of addition


==================================================================================================================== 

                                Proof of Identity of Scalar Multiplication :

====================================================================================================================

Claim : For any vector v such that dim V = n >= 1, 1.0 * v = v
        (As mentioned 1.0 * v is explicitly defined as : scale 1.0 v , in our module)

Proof : Proof By Structural Induction on the structure of the vector v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, 1.0 * v = v
  
  Base Case : v = [v0]
    
    Then we have : 1.0 * v = 1.0 * [v0] = [1.0 * v0] = [v0] = v (Since 1.0 is the multiplicative identity for floats)
    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : 1.0 * v = 1.0 * [v0 :: xv] 
                           = [1.0 * v0] :: (1.0 * xv)   (By definition of scalar multiplication on vectors in our module)
                           = [v0] :: xv                 (By IH and definition of vector addition, and the fact that 1.0 
                                                         is the multiplicative identity for floats)
                           = v  
    
    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) --------- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v such that dim V = n >= 1, 1.0 * v = v 

  Thereby proving the claim of identity of scalar multiplication

====================================================================================================================

                                Proof of Annihilator of Scalar Multiplication :

====================================================================================================================

Claim : For any vector v such that dim V = n >= 1, 0.0 * v = O, where O is the zero vector of dimension n
        (As mentioned 0.0 * v is explicitly defined as : scale 0.0 v , in our module)

Proof : Proof By Structural Induction on the structure of the vector v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, 0.0 * v = O (denoted by O_{n})
  
  Base Case : v = [v0]
    
    Then we have : 0.0 * v = 0.0 * [v0] = [0.0 * v0] = [0.0] = O_{1} (Since 0.0 is the annihilator 
    for float multiplication). Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : 0.0 * v = 0.0 * [v0 :: xv] 
                           = [0.0 * v0] :: (0.0 * xv) (By definition of scalar multiplication on vectors in our module, 
                                                       as well as 0.0 is the annihilator for float multiplication)

                          = [0.0] :: O_{n} = O_{n+1}  (By IH and definition of vector addition, as 
                                                       well as definition of dim in our module)
    
    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) ----- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v such that dim V = n >= 1, 0.0 * v = O 

  Thereby proving the claim of annihilator of scalar multiplication


====================================================================================================================
                                    Proof of Additive Inverse of Vectors :
====================================================================================================================

Claim : For any vector v such that dim V = n >= 1, (v) + (inv v) = O, where O is the zero vector of dimension n
        (As mentioned (v) + (inv v) is explicitly defined as : add v (inv v) , in our module)

Proof : Proof by Structural Induction on Structure of v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, (v) + (inv v) = O (denoted by O_{n})
  
  Base Case : v = [v0]
    
    Then we have : (v) + (inv v) = [v0] + [-1.0 * v0]  (By definition of additive inverse in our module)
                                 = [v0 + -1.0 * v0 ]      (By definition of vector addition in our module)
                                 = [0.0] = O_{1}       (By the fact that -1.0*x is the additive inverse of x for floats)
  
    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : (v) + (inv v) = [v0 :: xv] + (inv [v0 :: xv]) 
                                 = [v0 :: xv] + [-1.0 * v0 :: (inv xv)]           (By definition of additive inverse in our module)
                                 = [v0 + (-1.0 * v0)] :: ((xv) + (in xv))         (By definition of vector addition in our module)
                                 = [0.0] :: O_{n} = O_{n+1}                       (By IH and definition of vector addition,
                                                                                   and the definition of additive inverse of floats) 
                                                                                   ,as well as definition of dim in our module)

    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) -------- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v such that dim V = n >= 1, (v) + (inv v) = O 

  Thereby proving the claim of additive inverse of vectors

====================================================================================================================
                                (Left) Combination of Scalar Products on Vectors :
====================================================================================================================


Claim : For any vector v and scalar b,c such that dim v = n >= 1, b * (c * v) = (b * c) * v
        (As mentioned b * (c * v) is explicitly defined as : scale b (scale c v) , in our module
         and similarly for (b * c) * v : scale (b * c) v, in our module)

Proof : Proof by Structural Induction on structure v

  Let Inductive Hypothesis be : For any vector v and scalars b,c such that dim v = n >= 1, b * (c * v) = (b * c) * v

  Base Case : v = [v0]
    
    Then we have : b * (c * v) = b * (c * [v0]) 
                               = b * [c * v0]   (By definition of scalar multiplication on vectors in our module)
                               = [b * (c * v0)] (By definition of scalar multiplication on vectors in our module) 
                               = [(b * c) * v0] (By associativity of multiplication on floats)
                               = (b * c) * [v0] (By definition of scalar multiplication on vectors in our module in reverse)

    Hence, the claim holds for the base case  ------ (1)
  
  Inductive Step :  Suppose IH holds for some vector xv such that dim xv = n > 1

    Then consider : v = v0 :: xv , where v0 is a float and head of vector v 

    Then we have : b * (c * v) = b * (c * [v0 :: xv]) 
                               = b * ([c * v0] :: (c * xv))           (By definition of scalar multiplication on vectors in our module)
                               = [b * (c * v0)] :: (b * (c * xv))     (By definition of scalar multiplication on vectors in our module) 
                               = [(b * c) * v0] :: ((b * c) * xv)     (By IH and associativity of multiplication on floats)
                               = (b * c) * [v0 :: xv] = (b * c) * v   (By definition of scalar multiplication on vectors in our module
                                                                       in reverse)

    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) -------- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v and scalars b, c such that dim v = n >= 1, b * (c * v) = (b * c) * v

  Thereby proving the claim of combination of scalar products on vectors


====================================================================================================================
                                       Scalar Sum Product Distribution on Vectors :
====================================================================================================================

Claim : For any vector v and scalars b,c such that dim v = n >= 1, (b + c) * v = (b * v) + (c * v)
        (As mentioned (b + c) * v is explicitly defined as : scale (b + c) v , in our module
         and similarly for (b * v) + (c * v) : addv (scale b v) (scale c v), in our module 
         where b and c are scalars and v is a vector of floats)

Proof : Proof by Structural Induction on structure of v

Let Inductive Hypothesis be : For any vector v and scalars b, c such that dim v = n >= 1, (b + c) * v = (b * v) + (c * v)

Base Case : v = [v0]
  
  Then we have : (b + c) * v = (b + c) * [v0] 
                             = [(b + c) * v0]          (By definition of scalar multiplication on vectors in our module) 
                             = [b * v0 + c * v0]       (By distributivity of multiplication over addition on floats)
                             = [b * v0] + [c * v0]     (By definition of addition on vectors in our module in reverse)
                             = (b * [v0]) + (c * [v0]) (By definition of scalar multiplication on vectors in our module in reverse)
                             = (b * v) + (c * v)       (By definition of scalar multiplication on vectors in our module in reverse)

  (By definition of scalar multiplication and addition on vectors and distributivity of multiplication over addition on floats)

  Hence, the claim holds for the base case  ------ (1)

Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1

  Then consider : v = v0 :: xv , where v0 is a float and head of vector v 

  Then we have : (b + c) * v = (b + c) * [v0 :: xv]                                    
                             = [(b + c) * v0] :: ((b + c) * xv)                        (By definition of scalar multiplication on vectors in our module)
                             = [b * v0 + c * v0] :: ((b * xv) + (c * xv))              (By IH and distributivity of multiplication over addition on floats)
                             = ([b * v0] :: (b * xv)) + ([c * v0] :: (c * xv))         (By definition of addition on vectors in our module in reverse)
                             = (b * [v0 :: xv]) + (c * [v0 :: xv])                     (By definition of scalar multiplication on vectors in our module in reverse)
                             = (b * v) + (c * v)                                       (By definition of scalar multiplication on vectors in our module in reverse)

  Hence, the IH holds for vector v of form v0 :: xv if
  it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
  by the definition of dim in our module) -------- (2)

Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
for any vector v and scalars b, c such that dim v = n >= 1, (b + c) * v = (b * v) + (c * v)

Thereby proving the claim of scalar sum product distribution on vectors

====================================================================================================================
                                       Scalar Distribution over Vector Sums :
====================================================================================================================

Claim : For any vectors u and v and scalar b such that dim u = dim v = n >= 1, b * (u + v) = (b * u) + (b * v)
        (As mentioned b * (u + v) is explicitly defined as : scale b (addv u v) , in our module
         and similarly for (b * u) + (b * v) : addv (scale b u) (scale b v), in our module 
         where b is a scalar and u and v are vectors of floats)

Proof  : Proof by Structural Induction on structure of u and v
        
Let Inductive Hypothesis be : For any vectors u and v and scalar b such that dim u = dim v = n >= 1, b * (u + v) = (b * u) + (b * v)

Base Case : u = [u0], v = [v0]
  
  Then we have : b * (u + v) = b * ([u0] + [v0]) 
                             = b * [u0 + v0]           (By definition of vector addition in our module)
                             = [b * (u0 + v0)]         (By definition of scalar multiplication on vectors in our module) 
                             = [b * u0 + b * v0]       (By distributivity of multiplication over addition on floats)
                             = [b * u0] + [b * v0]     (By definition of addition on vectors in our module in reverse)
                             = (b * [u0]) + (b * [v0]) (By definition of scalar multiplication on vectors in our module in reverse)
                             = (b * u) + (b * v)       (By definition of scalar multiplication on vectors in our module in reverse)


  Hence, the claim holds for the base case  ------ (1)

Inductive Step : Suppose inductive hypothesis holds for some vectors xu and xv such that dim xu = dim xv = n > 1

  Then consider : u = u0 :: xu, v = v0 :: xv , where u0 and v0 are floats and heads of vectors u and v

  Then we have : b * (u + v) = b * ([u0 :: xu] + [v0 :: xv])                                    
                             = b * ([u0 + v0] :: (xu + xv))                        (By definition of vector addition in our module)
                             = [b * (u0 + v0)] :: (b * (xu + xv))                  (By definition of scalar multiplication on vectors in our module)
                             = [b * u0 + b * v0] :: (b * xu + b * xv)              (By IH and distributivity of multiplication over addition on floats)
                             = ([b * u0] :: (b * xu)) + ([b * v0] :: (b * xv))     (By definition of addition on vectors in our module in reverse)
                             = (b * [u0 :: xu]) + (b * [v0 :: xv])                 (By definition of scalar multiplication on vectors in our module in reverse)
                             = (b * u) + (b * v)                                   (By definition of scalar multiplication on vectors in our module in reverse)

  Hence, the IH holds for vectors u and v of form u0 :: xu and v0 :: xv if
  it holds for xu and xv, where dim xu = dim xv = n > 1 (Note that dim u = 1 + dim xu = 1 + n = 1 + dim xv = dim v, 
  by the definition of dim in our module) -------- (2)  

Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e.
for any vectors u and v and scalar b such that dim u = dim v = n >= 1, b * (u + v) = (b * u) + (b * v)

*)

