type vector = float list;;

exception DimensionError;;
exception ZeroVectorError;;

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


let rec is_legal_dim (vec_float : vector) : bool  = 
  match vec_float with
  | [] -> false
  | x::xs -> true

;;
(* PS : Is dimension checking to be done here ?  *)
let rec is_zero (v : vector) : bool = 
  if(is_legal_dim v = false) then
    raise DimensionError
  else
    match v with
    | [] -> true
    | [0.0] -> true
    | x::xs -> if x = 0.0 then is_zero xs else false;; 

let rec unit (n : int) (j : int) : vector = 
  if(n < 1) then 
    raise DimensionError
  else if (j > n || j < 1) then 
    raise DimensionError
  else if j = 1 then
    if(n = 1) then
      [1.0]
    else
       1.0::(create (n-1) 0.0)
  else if j = n then
    if(n = 1) then    (* Redundant since if n = 1 then j = 1, earlier if will trigger *)
      [1.0]
    else
      (create (n-1) 0.0) @ [1.0]
  else
    (create (j-1) 0.0) @ [1.0] @ (create (n-j) 0.0);;
;;

let rec scale (c : float) (v : vector) : vector =
  if(dim v < 1) then
    raise DimensionError
  else
    match v with
    | [] -> []
    | [x1] -> [c *. x1]
    | x::xs -> (c *. x) :: (scale c xs)
let rec addv (v1 : vector) (v2 : vector) : vector =
  (* 
  
  This becomes redundant since mathcing will 
  take care of this case.
  
  if(dim v1 <> dim v2) then
    raise DimensionError
  else 

  *)
  match v1, v2 with
  | [x1], [x2] -> [x1 +. x2]
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
  if(len_v1 = 0.0 || len_v2 = 0.0) then
    raise ZeroVectorError 
  else
  let cos_theta = v1_dot_v2 /. (len_v1 *. len_v2) in
  (* Implementation of acos guarantees  that the angle returned 
  is the smaller one (theta <= pi radians) *)
  acos cos_theta;;
  
(* ========================================================================== *)
  
(* Extensive Testing of all the operations of vector module *)

let v1 = create 20000 4.0;;
let v2 = create 20000 5.0;;

let v3 = addv v1 v2;;

let () = 
  List.iter (fun x -> Printf.printf "%f " x) v1;
  print_newline ();;
(* ========================================================================== *)

(* Mathematical Proofs of the following properties of operations *)

(* ========================================================================== *)

(*


======================================================================================================

In our implementation, vectors are essentially an alias for lists of floats
Since our module uses tail recursive functions, we will try to prove the properties of our 
module using structural induction on the vectors 

An intersting property of our module is that the vectors have dimensions = n >= 1
Thus the base case of our induc


======================================================================================================

Some common abbreviations used in the proofs :

  1) IH : Inductive Hypothesis
  2) O  : The zero vector of appropriate dimension n >=1 i.e. O = [0.0, 0.0, 0.0, ..., 0.0] (n times)

Note that addition of two vectors u and v is defined as per the module's implentation only and
proof's have been given for the same (Uses + to denote crumbersome poofs).

Similary for scalar multiplication, we use * to denote scalar multiplication of vectors and is defined 
as per the module's implentation only 

Thus in proofs : u + v = add u v , u + (v + w) add u (add v w) etc. 

               : c * v = scale c v, b.(c.v) = scale b (scale c v) etc.

======================================================================================================

Important Note : Dimension are of vecors are preserved upon addition and scalar multiplication:

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

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, v + O = v
  
  Base Case : v = [v0]
    
    Then we have : v + O = [v0] + [0.0] = [v0 + 0.0] = [v0] = v (Since 0.0 is the additive identity for floats)
    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : v + O = [v0 :: xv] + [0.0 :: O] = [v0 + 0.0] :: (xv + O) = [v0] :: xv = v
    
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

Proof : Proof By Structural Induction on the structure of the vector v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, 1.0 * v = v
  
  Base Case : v = [v0]
    
    Then we have : 1.0 * v = 1.0 * [v0] = [1.0 * v0] = [v0] = v (Since 1.0 is the multiplicative identity for floats)
    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : 1.0 * v = 1.0 * [v0 :: xv] = [1.0 * v0] :: (1.0 * xv) = [v0] :: xv = v
    
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

Proof : Proof By Structural Induction on the structure of the vector v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, 0.0 * v = O (denoted by O_{n})
  
  Base Case : v = [v0]
    
    Then we have : 0.0 * v = 0.0 * [v0] = [0.0 * v0] = [0.0] = O_{1} (Since 0.0 is the annihilator 
    for float multiplication). Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : 0.0 * v = 0.0 * [v0 :: xv] = [0.0 * v0] :: (0.0 * xv) = [0.0] :: O_{n} = O_{n+1}
    
    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) ----- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v such that dim V = n >= 1, 0.0 * v = O 

  Thereby proving the claim of annihilator of scalar multiplication


====================================================================================================================
                                  Proof of Additive Inverse of Vectors :
====================================================================================================================

Claim : For any vector v such that dim V = n >= 1, v + (-1.0 * v) = O, where O is the zero vector of dimension n

Proof : Proof by Structural Induction on Structure of v

  Let Inductive Hypothesis be : For any vector v such that dim V = n >= 1, v + (-1.0 * v) = O (denoted by O_{n})
  
  Base Case : v = [v0]
    
    Then we have : v + (-1.0 * v) = [v0] + (-1.0 * [v0]) = [v0] + [-1.0 * v0] = [v0 - v0] = [0.0] = O_{1}
    (By definition of addition and scalar multiplication on vector in our module , as well as
    the definition of additive inverse of floats).

    Hence, the claim holds for the base case  ------ (1)

  Inductive Step : Suppose IH holds for some vector xv such that dim xv = n > 1
    
    Then consider : v = v0 :: xv, where v0 is a float and head of vector v
    
    Then we have : v + (-1.0 * v) = [v0 :: xv] + (-1.0 * [v0 :: xv]) = [v0 :: xv] + [-1.0 * v0 :: (-1.0 * xv)]
            = [v0 + (-1.0 * v0)] :: (xv + (-1.0 * xv)) = [0.0] :: O_{n} = O_{n+1}
    
    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) -------- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v such that dim V = n >= 1, v + (-1.0 * v) = O 

  Thereby proving the claim of additive inverse of vectors

====================================================================================================================
                                      (Left) Combination of Scalar Products on Vectors :
====================================================================================================================


Claim : For any vector v and scalar b,c such that dim v = n >= 1, b * (c * u) = (b * u) * u

Proof : Proof by Structural Induction on structure v

  Let Inductive Hypothesis be : For any vector v and scalars b,c such that dim v = n >= 1, b * (c * u) = (b * u) * u

  Base Case : v = [v0]
    
    Then we have : b * (c * v) = b * (c * [v0]) = b * [c * v0] = [b * (c * v0)] = [(b * c) * v0] = (b * c) * [v0]
    (By definition of scalar multiplication of vector and associativity of multiplication on floats)

    Hence, the claim holds for the base case  ------ (1)
  
  Inductive Step :  Suppose IH holds for some vector xv such that dim xv = n > 1

    Then consider : v = v0 :: xv , where v0 is a float and head of vector v 

    Then we have : b * (c * v) = b * (c * [v0 :: xv]) = b * ([c * v0] :: (c * xv))
            = [b * (c * v0)] :: (b * (c * xv)) = [(b * c) * v0] :: ((b * c) * xv)
            = (b * c) * [v0 :: xv] = (b * c) * v

    Hence, the IH holds for vector v of form v0 :: xv if
    it holds for xv, where dim xv = n > 1 (Note that dim v = 1 + dim xv = 1 + n = dim v, 
    by the definition of dim in our module) -------- (2)

  Thus by (1), (2), and Structural Induction, we have proved that the IH holds i.e. 
  for any vector v and scalars b, c such that dim v = n >= 1, b * (c * v) = (b * c) * v

  Thereby proving the claim of combination of scalar products on vectors

*)




(* ========================================================================== *)
