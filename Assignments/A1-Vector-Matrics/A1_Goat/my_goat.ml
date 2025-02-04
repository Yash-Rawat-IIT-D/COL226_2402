exception DimensionError of string;;

type vector = float list;;


let create n x : vector =
    if n < 1 then
        raise (DimensionError "Dimension must be greater than 0")
    else
        let rec create1 n acc : vector =
            if n = 0 then acc
            else create1 (n - 1) (x :: acc)
        in
    create1 n []
;;


let dim (v : vector) : int = 
    let rec dim1 v acc : int =
        match v with 
        | [] -> raise (DimensionError "Dimension must be greater than 0")
        | x::[] -> acc+1
        | _::xs -> dim1 xs (acc + 1)
    in
    dim1 v 0
;;


let rec is_zero (v : vector) : bool = 
    match v with 
    | [] -> raise (DimensionError "Dimension must be greater than 0")
    | [0.0] -> true 
    | x::xs -> if x=0. then is_zero xs else false;;

let unit n j : vector = 
    if (n < 1 || j < 1 || n < j) then
        raise (DimensionError "(1 <= j <= n) is violated")   
    else
        let rec unit1 n j acc : vector =
            if (n = 0) 
                then acc
            else if (n = j) 
                then unit1 (n - 1) j (1.0 :: acc)
            else 
                unit1 (n - 1) j (0.0 :: acc)
        in
        unit1 n j []
;;


let rec scale c v : vector = 
    if (v=[]) then
        raise (DimensionError "Dimension must be greater than 0")
    else
    match v with
    | x::[] -> [c *. x]
    | x::xs -> (c *. x) :: scale c xs
;;

let addv v1 v2 : vector =
    if (dim v1 != dim v2) then
        raise (DimensionError "Dimensions do not match")
    else
        let rec addv1 v1 v2 : vector = 
            match v1, v2 with
            | [], [] -> []
            | x::xs, y::ys -> (x +. y) :: addv1 xs ys
        in
        addv1 v1 v2
;;


let dot_prod v1 v2 : float =
    if (dim v1 != dim v2) then
        raise (DimensionError "Dimensions do not match")
    else
        let rec dot_prod1 v1 v2 acc : float =
            match v1 with
            | [] -> acc
            | x::xs -> match v2 with
                | y::ys -> dot_prod1 xs ys (acc +. (x *. y))
        in
        dot_prod1 v1 v2 0.0
;;


let inv v : vector = 
    scale (-1.0) v
;;


let length v : float = sqrt (dot_prod v v);;

let angle v1 v2 : float = 
    let dot = dot_prod v1 v2 in
    let len1 = length v1 in
    let len2 = length v2 in
    if (len1 = 0.0 || len2 = 0.0) then
        raise (Invalid_argument "Length of vector cannot be zero")

    else
        let cos1 = dot /. (len1 *. len2) in
        let cos1_clamped = 
            if cos1 > 1.0 then 1.0
            else if cos1 < -1.0 then -1.0
            else cos1
        in
        acos cos1_clamped
;;

(*

Note: Length in the following proofs referse to the length of the list representing the vector.
That is, it refers to the size of the vector.
1. Commutativity of addv
-------------------------
To Prove:
    For all u: vector, for all v: vector, addv u v = addv v u.

Proof:
    By induction on the length of u: vector.

    Base Case (u = [x]):
        (Let v = [y])
        -> addv u v = addv [x] v
            = addv1 [x] v (where addv1 is the helper function)
            = addv1 [x] [y] (v = [y])
            = [x + y] :: addv1 [] [] (definition of addv1)
            = [y + x] :: addv1 [] [] (commutativity of addition)
            = addv1 [y] [x] (definition of addv1)
            = addv1 v u (as u = [x], v = [y])
            = addv v u (definition of addv)
        -> Therefore, addv u v = addv v u when u = [x].

        Induction Hypothesis:
        Assume for any vector u of length k we have,
            for all v: vector,
                addv u v = addv v u.

    Induction Step:
        Let u = x :: xs (vector of length k + 1, xs is a vector of length k).
        (Let v = y :: ys)
        -> addv u v = addv (x :: xs) (y :: ys)
            = addv1 (x :: xs) (y :: ys) (where addv1 is the helper function)
            = [x+y] :: addv1 xs ys (definition of addv1)
            = [y+x] :: addv1 xs ys (commutativity of addition)
            = [y+x] :: addv xs ys (definition of addv)
            = [y+x] :: addv ys xs (by the induction hypothesis on xs, ys as length=k)
            = [y+x] :: addv1 ys xs (definition of addv)
            = addv1 (y :: ys) (x :: xs) (definition of addv1)
            = addv (y :: ys) (x :: xs) (definition of addv)
            = addv v u (as u = x :: xs, v = y :: ys)
        -> Therefore, addv u v = addv v u when u = x :: xs, v = y :: ys.        

Therefore, by structural induction on the length of u, for all vectors u and v, addv u v = addv v u.


--------------------------------------------------------------------------------------------------------------------
2. Associativity of addv
-------------------------
To Prove:
    For all u: vector, for all v: vector, and for all w: vector,
    addv u (addv v w) = addv (addv u v) w.

Proof:
    By induction on the length of u: vector.

    Base Case (u = [x]):
        (Let v = [y] and w = [z])
        -> addv u (addv v w) = addv [x] (addv1 [y] [z]) (helper function addv1)
            = addv [x] ([y+z]::[]) (definition of addv1)
            = addv1 [x] [y+z] (definition of addv)
            = [x+(y+z)]::[] (definition of addv1)
            = [(x+y)+z] (associativity of addition)                 ..(1)
        
        -> addv (addv u v) w = addv (addv1 [x] [y]) [z] (helper function addv1)
            = addv [x+y]::[] [z] (definition of addv1)
            = addv1 [x+y] [z] (definition of addv)
            = [(x+y)+z]::[] (definition of addv1)
            = [(x+y)+z]                                             ..(2)
        
        -> Therefore, addv u (addv v w) = addv (addv u v) w when u = [x] by (1) and (2).

    Induction Hypothesis:
        Assume for any vector u of length k, 
            for all v: vector and w: vector,
            addv u (addv v w) = addv (addv u v) w.

    Induction Step:
        Let u = x :: xs (vector of length k + 1, xs is a vector of length k).
        (Let v = y :: ys and w = z :: zs)
        -> addv u (addv v w) = addv1 (x :: xs) (addv1 (y :: ys) (z :: zs)) (helper function addv1)
            = addv1 (x :: xs) ((y+z) :: (addv1 ys zs)) (definition of addv1)
            = [(x+(y+z))] :: (addv1 xs (addv1 ys zs)) (definition of addv1).
            = [(x+y)+z] :: (addv xs (addv ys zs)) (associativity of addition, helper function addv1)  .
            = [(x+y)+z] :: (addv (addv xs ys) zs) (by the induction hypothesis)  ..(3)
        -> addv (addv u v) w = addv1 (addv1 (x :: xs) (y :: ys)) (z :: zs) (helper function addv1)
            = addv1 ((x+y) :: (addv1 xs ys)) (z :: zs) (definition of addv1)
            = [(x+y)+z] :: (addv1 (addv1 xs ys) zs) (definition of addv1).      
            = [(x+y)+z] :: (addv (addv xs ys) zs) (definition of addv).      ..(4)
        -> Therefore, addv u (addv v w) = addv (addv u v) w by (3) and (4).

Conclusion:
    By structural induction on the length of u,
    for all vectors u, v, and w, addv u (addv v w) = addv (addv u v) w.

--------------------------------------------------------------------------------------------------------------------
3. Identity of addv
---------------------
To Prove:
  For all v: vector, addv v [0.0] = v.

Proof:
    By induction on the length of v: vector.

    Base Case (v = [x]):
        -> addv v [0.0] = addv [x] [0.0]
            = addv1 [x] [0.0] (definition of addv)
            = [(x + 0.0)] :: addv1 [] [] (definition of addv1)
            = [x] (since x + 0.0 = x and addv1 [] [] = []).
        -> Therefore, addv v [0.0] = v when v = [x].

    Induction Hypothesis:
        Assume for any vector v of length k, addv v O = v.

    Induction Step:
        Let v = x :: xs (vector of length k + 1, xs is a vector of length k).
        Let O = [0.0] :: os (where os is a zero vector of length k).
        -> addv v O = addv (x :: xs) ([0.0] :: os)
            = addv1 (x :: xs) ([0.0] :: os)(definition of addv)
            = (x + 0.0) :: addv1 xs os (definition of addv1)
            = x :: addv1 xs os (simplifying x + 0.0 to x)
            = x :: xs (by the induction hypothesis on xs which is a vector of length k, addv1 xs os = xs).
            = v (as v = x :: xs).
        -> Therefore, addv v O = v when v = x :: xs.

Conclusion:
    By structural induction on the length of v,
    addv v O = v for all vectors v.

--------------------------------------------------------------------------------------------------------------------
4. Identity Scalar: 1.v = v
---------------------------
To Prove:
    For all v: vector, scale 1.0 v = v.

Proof:
    By induction on the length of v: vector.

    Base Case (v = [x]):
        -> scale 1.0 v = scale 1.0 [x]
            = [1.0 *. x](definition of scale)
            = [x] (since 1.0 *. x = x).
        -> Therefore, scale 1.0 v = v when v = [x].

    Induction Hypothesis:
        Assume for any vector v of length k, scale 1.0 v = v.

    Induction Step:
        Let v = x :: xs (vector of length k + 1, xs is a vector of length k).
        -> scale 1.0 v = scale 1.0 (x :: xs)
            = (1.0 *. x) :: scale 1.0 xs (definition of scale)
            = x :: xs (by the induction hypothesis on xs, scale 1.0 xs = xs).
            = v (as v = x :: xs).
        -> Therefore, scale 1.0 v = v when v = x :: xs.

Conclusion:
    By structural induction on the length of v,
    scale 1.0 v = v for all vectors v.

--------------------------------------------------------------------------------------------------------------------
5. Annihilator Scalar: 0.v = O
------------------------------
To Prove:
    For all v: vector, scale 0.0 v = O, where O is a zero vector of the same length as v.

Proof:
    By induction on the length of v: vector.

    Base Case (v = [x]):
        -> scale 0.0 v = scale 0.0 [x]
            = [0.0 *. x] (definition of scale)
            = [0.0] (since 0.0 *. x = 0.0).
        -> Therefore, scale 0.0 v = O when v = [x].

    Induction Hypothesis:
        Assume for a vector v of length k, scale 0.0 v = O.

    Induction Step:
        Let v = x :: xs (vector of length k + 1, xs is a vector of length k).
        Let O be zero vector of length k+1, o be zero vector of length k. (O = 0.0 :: o)
        -> scale 0.0 v = scale 0.0 (x :: xs) 
            = (0.0 *. x) :: scale 0.0 xs (definition of scale)
            = 0.0 :: o (by the induction hypothesis on xs, scale 0.0 xs = o).
            = O (as O = 0.0 :: o for vectors of length k + 1).
        -> Therefore, scale 0.0 v = O when v = x :: xs.

Conclusion:
    By structural induction on the length of v,
    scale 0.0 v = O for all vectors v.

--------------------------------------------------------------------------------------------------------------------
6. Additive Inverse: v + (-v) = O
---------------------------------
To Prove:
    For all v: vector, addv v (inv v) = O, where O is a zero vector of the same length as v.

Proof:
    By induction on the length of v: vector.
    inv v = scale (-1.0) v.
    Therefore, it is sufficient to prove addv v (scale (-1.0) v) = O.

    Base Case (v = [x]):
        -> addv v (scale (-1.0) v) = addv [x] (scale (-1.0) [x])
            = addv [x] [(-1.0 *. x)] (definition of scale)
            = addv1 [x] [(-1.0 *. x)] (definition of addv)
            = [(x + (-1.0 *. x))] :: addv1 [] [] (definition of addv1)
            = [0.0] (since x + (-1.0 *. x) = 0.0 and addv1 [] [] = []).
        -> Therefore, addv v (scale (-1.0) v) = O when v = [x].

    Induction Hypothesis:
        Assume for any vector v of length k,
            addv v (scale (-1.0) v) = O.

    Induction Step:
        Let v = x :: xs (vector of length k + 1, xs is a vector of length k).
        Let O be zero vector of length k+1, o be zero vector of length k. (O = 0.0 :: o)
        -> addv v (scale (-1.0) v) = addv (x :: xs) (scale (-1.0) (x :: xs))
            = addv1 (x :: xs) ((-1.0 *. x) :: scale (-1.0) xs) (definition of scale)
            = [(x + (-1.0 *. x))] :: addv1 xs (scale (-1.0) xs) (definition of addv1)
            = [0.0] :: o (since x + (-1.0 *. x) = 0.0 and by induction hypothesis, addv1 xs (scale (-1.0) xs) = o).
            = O (as O = 0.0 :: o for vectors of length k + 1).
        -> Therefore, addv v (scale (-1.0) v) = O when v = x :: xs.

Conclusion:
    By structural induction on the length of v,
    addv v (scale (-1.0) v) = O for all vectors v.
    Therefore, add v (inv v) = O for all vectors v.

--------------------------------------------------------------------------------------------------------------------
7. Scalar Product Combination: b.(c.v) = (b.c).v
------------------------------------------------
To Prove:
    For all v: vector, scale b (scale c v) = scale (b *. c) v.

Proof:
    By induction on the length of v: vector.

    Base Case (v = [x]):
        -> scale b (scale c v) = scale b (scale c [x])
            = scale b [(c *. x)] (definition of scale)
            = [(b *. (c *. x))] (definition of scale)
            = [(b *. c) *. x] (associativity of scalar multiplication)
            = scale (b *. c) [x] (definition of scale).
        -> Therefore, scale b (scale c v) = scale (b *. c) v when v = [x].

    Induction Hypothesis:
        Assume for any vector v of length k,
            scale b (scale c v) = scale (b *. c) v.

    Induction Step:
        Let v = x :: xs (vector of length k + 1, xs is a vector of length k).
        -> scale b (scale c v) = scale b (scale c (x :: xs))
            = scale b ((c *. x) :: scale c xs) (definition of scale)
            = (b *. (c *. x)) :: scale b (scale c xs) (definition of scale)
            = ((b *. c) *. x) :: scale (b *. c) xs (by induction hypothesis on xs)
            = scale (b *. c) (x :: xs) (definition of scale).
            = scale (b *. c) v (as v = x :: xs).
        -> Therefore, scale b (scale c v) = scale (b *. c) v when v = x :: xs.

Conclusion:
    By structural induction on the length of v,
    scale b (scale c v) = scale (b *. c) v for all vectors v.

--------------------------------------------------------------------------------------------------------------------
8. Scalar Sum-Product Distribution: (b + c).v = b.v + c.v
----------------------------------------------------------
To Prove:
    For all v: vector, scale (b + c) v = addv (scale b v) (scale c v).

Proof:
    By induction on the length of v: vector.

    Base Case (v = [x]):
        -> scale (b + c) v = scale (b + c) [x]
            = [(b + c) *. x] (definition of scale)
            = [b *. x + c *. x] (distributive property of multiplication over addition)
            = addv [b *. x] [c *. x] (definition of addv)
            = addv (scale b [x]) (scale c [x]) (definition of scale).
        -> Therefore, scale (b + c) v = addv (scale b v) (scale c v) when v = [x].

    Induction Hypothesis:
        Assume for any vector v of length k,
            scale (b + c) v = addv (scale b v) (scale c v).

    Induction Step:
        Let v = x :: xs (vector of length k + 1, xs is a vector of length k).
        -> scale (b + c) v = scale (b + c) (x :: xs)
            = ((b + c) *. x) :: scale (b + c) xs (definition of scale)
            = (b *. x + c *. x) :: addv (scale b xs) (scale c xs) (by induction hypothesis on xs)
            = (b *. x + c *. x) :: addv1 (scale b xs) (scale c xs) (definition of addv)
            = addv1 ((b *. x) :: scale b xs) ((c *. x) :: scale c xs) (definition of addv1)
            = addv (scale b (x :: xs)) (scale c (x :: xs)) (definition of scale and addv).
            = addv (scale b v) (scale c v) (as v = x :: xs).
        -> Therefore, scale (b + c) v = addv (scale b v) (scale c v) when v = x :: xs.

Conclusion:
    By structural induction on the length of v,
    scale (b + c) v = addv (scale b v) (scale c v) for all vectors v.

--------------------------------------------------------------------------------------------------------------------
9. Scalar Distribution over Vector Sums: b.(u + v) = b.u + b.v
--------------------------------------------------------------
To Prove:
    For all u, v: vectors, scale b (addv u v) = addv (scale b u) (scale b v).

Proof:
    By induction on the length of u and v: vectors (both must have the same length).

    Base Case (u = [x], v = [y]):
        -> scale b (addv u v) = scale b (addv [x] [y])
            = scale b [(x + y)] (definition of addv)
            = [b *. (x + y)] (definition of scale)
            = [b *. x + b *. y] (distributive property of multiplication over addition)
            = addv [b *. x] [b *. y] (definition of addv)
            = addv (scale b [x]) (scale b [y]) (definition of scale).
        -> Therefore, scale b (addv u v) = addv (scale b u) (scale b v) when u = [x], v = [y].

    Induction Hypothesis:
        Assume for any vectors u, v of length k,
            scale b (addv u v) = addv (scale b u) (scale b v).

    Induction Step:
        Let u = x :: xs and v = y :: ys (vectors of length k + 1, xs and ys are vectors of length k).
        -> scale b (addv u v) = scale b (addv (x :: xs) (y :: ys))
            = scale b ((x + y) :: addv xs ys) (definition of addv)
            = (b *. (x + y)) :: scale b (addv xs ys) (definition of scale)
            = (b *. x + b *. y) :: addv (scale b xs) (scale b ys) (by induction hypothesis on xs and ys)
            = (b *. x + b *. y) :: addv1 (scale b xs) (scale b ys) (definition of addv)
            = addv1 ((b *. x) :: scale b xs) ((b *. y) :: scale b ys) (definition of addv1)
            = addv (scale b (x :: xs)) (scale b (y :: ys)) (definition of scale and addv).
            = addv (scale b u) (scale b v) (as u = x :: xs, v = y :: ys).
        -> Therefore, scale b (addv u v) = addv (scale b u) (scale b v) when u = x :: xs, v = y :: ys.

Conclusion:
    By structural induction on the lengths of u and v,
    scale b (addv u v) = addv (scale b u) (scale b v) for all vectors u and v.

--------------------------------------------------------------------------------------------------------------------
*)



(*
Additional properties:

--------------------------------------------------------------------------------------------------------------------
10. Angle Between v and v = 0
----------------------------------------
To Prove:
    For all v: vector, angle v v = 0.

Proof:

    -> angle v v = angle v v
        = acos (dot_prod v v / (length v * length v))
        = acos (dot_prod v v / (sqrt (dot_prod v v) * sqrt (dot_prod v v)))
        = acos (dot_prod v v / (dot_prod v v))
        = acos 1.0 
        = 0.0 (since cos 0 = 1).
    -> Therefore, angle v v = 0 when v = v.

--------------------------------------------------------------------------------------------------------------------
11. Scalar Multiplication and Dot Product: dot (scale c u) v = c * dot u v
------------------------------------------------------------------------
To Prove:
    For all vectors u, v, and scalars c, dot (scale c u) v = c * dot u v.

Proof:
    By induction on the length of u: vector.

    Base Case (u = [x], v = [y]):
        -> dot (scale c u) v = dot (scale c [x]) [y]
            = dot ([c *. x]) [y] (definition of scale)
            = (c *. x) * y (definition of dot)
            = c *. (x *. y) (associativity of multiplication)
            = c *. dot [x] [y] (definition of dot)
            = c *. dot u v (as u = [x], v = [y]).
        -> Therefore, dot (scale c u) v = c * dot u v when u = [x].
    
    Induction Hypothesis:
        Assume for any vector u of length k, for all vectors v and scalars c, dot (scale c u) v = c * dot u v.
    
    Induction Step:
        Let u = x :: xs, v = y :: ys (vectors of length k + 1, xs and ys are vectors of length k).
        -> dot (scale c u) v = dot (scale c (x :: xs)) (y :: ys)
            = dot ((c *. x) :: scale c xs) (y :: ys) (definition of scale)
            = (c *. x) * y + dot (scale c xs) ys (definition of dot)
            = c *. (x * y) + c *. dot xs ys (by the induction hypothesis on xs and ys)
            = c *. (dot [x] y + dot xs ys) (distributive property of multiplication over addition)
            = c *. (dot (x :: xs) (y :: ys)) (definition of dot)
            = c *. dot u v (as u = x :: xs, v = y :: ys).
        -> Therefore, dot (scale c u) v = c * dot u v when u = x :: xs.


12. Distributivity of Dot Product Over Vector Addition: dot (addv u v) w = dot u w + dot v w
--------------------------------------------------------------------------------------
To Prove:
    For all vectors u, v, w, dot (addv u v) w = dot u w + dot v w.

Proof:
    By induction on the length of u: vector.

    Base Case (u = [x], v = [y], w = [z]):
        -> dot (addv u v) w = dot (addv [x] [y]) [z]
            = dot [x + y] [z] (definition of addv)
            = (x + y) * z (definition of dot)
            = x * z + y * z (distributive property of multiplication over addition)
            = dot [x] [z] + dot [y] [z] (definition of dot)
            = dot u w + dot v w (as u = [x], v = [y], w = [z]).


    Induction Hypothesis:
    Assume for any vectors u, v, and w of length k, we have,
        dot (addv u v) w = dot u w + dot v w.

    Induction Step:
        Let u = x :: xs, v = y :: ys, w = z :: zs (vectors of length k + 1).
        -> dot (addv u v) w = dot (addv (x :: xs) (y :: ys)) (z :: zs)
            = dot ((x + y) :: addv xs ys) (z :: zs) (definition of addv)
            = (x + y) * z + dot (addv xs ys) zs (definition of dot)
            = x * z + y * z + dot xs zs + dot ys zs (by the induction hypothesis on xs and zs)
            = dot [x] [z] + dot [y] [z] + dot xs zs + dot ys zs (definition of dot)
            = dot [x] [z] + dot xs zs + dot [y] [z] + dot ys zs (commutativity of addition)
            = dot [x]::xs [z]::zs + dot [y]::ys [z]::zs (definition of dot)
            = dot u w + dot v w (as u = x :: xs, v = y :: ys, w = z :: zs).
    
    Therefore, by structural induction on the length of u, for all vectors u, v, and w, dot (addv u v) w = dot u w + dot v w.
*)

(* Test Cases for create *)
Printf.printf "Test Cases for create\n";;

(* Normal case *)
let n = 5;;
let result = create n 3.0;;
Printf.printf "create with n=5: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - n=1 *)
let n = 1;;
let result = create n 0.0;;
Printf.printf "create with n=1: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - n=0 (invalid case) *)
let n = 0;;
try 
    let result = create n 2.0 in
    (* If no exception occurs, print the result (just for completeness) *)
    List.iter (Printf.printf "%0.2f ") result
with 
    DimensionError msg -> 
        Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - n=10000 *)
let n = 10000;;
let result = create n 1.0;;
Printf.printf "create with n=10000: ";;
Printf.printf "Size: %d\n" (dim result);;

(* Test Cases for dim *)
Printf.printf "Test Cases for dim\n";;

(* Normal case *)
let v = [1.0; 2.0; 3.0];;
let result = dim v;;
Printf.printf "dim of [1.0; 2.0; 3.0]: %d\n" result;;

(* Edge case - empty vector *)
let v = [];;
try
  let result = dim v in
  Printf.printf "dim of []: %d\n" result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Edge case - single element vector *)
let v = [5.0];;
let result = dim v;;
Printf.printf "dim of [5.0]: %d\n" result;;

(* Large case - vector with 10000 elements *)
let v = create 10000 1.;;
let result = dim v;;
Printf.printf "dim of vector with 10000 elements: %d\n" result;;

(* Test Cases for is_zero *)
Printf.printf "Test Cases for is_zero\n";;

(* Normal case - zero vector *)
let v = [0.0; 0.0; 0.0];;
let result = is_zero v;;
Printf.printf "is_zero of [0.0; 0.0; 0.0]: %b\n" result;;

(* Normal case - non-zero vector *)
let v = [1.0; 0.0; 0.0];;
let result = is_zero v;;
Printf.printf "is_zero of [1.0; 0.0; 0.0]: %b\n" result;;

(* Edge case - empty vector *)
let v = [];;
try
  let result = is_zero v in
    Printf.printf "is_zero of []: %b\n" result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Edge case - single zero element *)
let v = [0.0];;
let result = is_zero v;;
Printf.printf "is_zero of [0.0]: %b\n" result;;

(* Test Cases for unit *)
Printf.printf "Test Cases for unit\n";;

(* Normal case *)
let result = unit 5 3;;
Printf.printf "unit vector of size 5 with 3rd element as 1: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - n=j *)
let result = unit 5 5;;
Printf.printf "unit vector of size 5 with 5th element as 1: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - n<j (invalid case) *)
let n = 3;;
let j = 4;;
try
  let result = unit n j in
    (* If no exception occurs, print the result (just for completeness) *)
    List.iter (Printf.printf "%0.2f ") result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - n=10000, j=5000 *)
let result = unit 10000 5000;;
Printf.printf "unit vector of size 10000 with 5000th element as 1: ";;
Printf.printf "Size: %d\n" (dim result);;

(* Test Cases for scale *)
Printf.printf "Test Cases for scale\n";;

(* Normal case *)
let v = [1.0; 2.0; 3.0];;
let c = 2.0;;
let result = scale c v;;
Printf.printf "scale of [1.0; 2.0; 3.0] by 2.0: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - empty vector *)
let v = [];;
let c = 2.0;;
try
  let result = scale c v in
    (* If no exception occurs, print the result (just for completeness) *)
    List.iter (Printf.printf "%0.2f ") result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - scaling a vector of size 10000 by 2 *)
let v = create 10000 1.0;;
let c = 2.0;;
let result = scale c v;;
Printf.printf "scale of vector with 10000 elements by 2: ";;
Printf.printf "Size: %d\n" (dim result);;

(* Test Cases for addv *)
Printf.printf "Test Cases for addv\n";;

(* Normal case *)
let v1 = [1.0; 2.0; 3.0];;
let v2 = [4.0; 5.0; 6.0];;
let result = addv v1 v2;;
Printf.printf "addv of [1.0; 2.0; 3.0] and [4.0; 5.0; 6.0]: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - vectors of different dimensions *)
let v1 = [1.0; 2.0];;
let v2 = [1.0; 2.0; 3.0];;
try
  let result = addv v1 v2 in
    (* If no exception occurs, print the result (just for completeness) *)
    List.iter (Printf.printf "%0.2f ") result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - adding two vectors of size 10000 *)
let v1 = create 10000 1.0;;
let v2 = create 10000 2.0;;
let result = addv v1 v2;;
Printf.printf "addv of two vectors with 10000 elements: ";;
Printf.printf "Size: %d\n" (dim result);;

(* Test Cases for dot_prod *)
Printf.printf "Test Cases for dot_prod\n";;

(* Normal case *)
let v1 = [1.0; 2.0; 3.0];;
let v2 = [4.0; 5.0; 6.0];;
let result = dot_prod v1 v2;;
Printf.printf "dot_prod of [1.0; 2.0; 3.0] and [4.0; 5.0; 6.0]: %0.2f\n" result;;

(* Edge case - empty vectors *)
let v1 = [];;
let v2 = [];;
try
  let result = dot_prod v1 v2 in
    (* If no exception occurs, print the result (just for completeness) *)
    Printf.printf "dot_prod of two empty vectors: %0.2f\n" result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - dot product of two vectors with 10000 elements *)
let v1 = create 10000 1.0;;
let v2 = create 10000 2.0;;
let result = dot_prod v1 v2;;
Printf.printf "dot_prod of two vectors with 10000 elements: %0.2f\n" result;;

(* Test Cases for inv *)
Printf.printf "Test Cases for inv\n";;

(* Normal case *)
let v = [1.0; -2.0; 3.0];;
let result = inv v;;
Printf.printf "inv of [1.0; -2.0; 3.0]: ";;
List.iter (Printf.printf "%0.2f ") result;;
print_newline ();;

(* Edge case - empty vector *)
let v = [];;
try
  let result = inv v in
    (* If no exception occurs, print the result (just for completeness) *)
    List.iter (Printf.printf "%0.2f ") result
with DimensionError msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - inverse of a vector with 10000 elements *)
let v = create 10000 2.0;;
let result = inv v;;
Printf.printf "inv of vector with 10000 elements: ";;
Printf.printf "Size: %d\n" (dim result);;

(* Test Cases for length *)
Printf.printf "Test Cases for length\n";;

(* Normal case *)
let v = [1.0; 2.0; 3.0];;
let result = length v;;
Printf.printf "length of [1.0; 2.0; 3.0]: %0.2f\n" result;;

(* Edge case - zero vector *)
let v = [0.0; 0.0; 0.0];;
let result = length v;;
Printf.printf "length of [0.0; 0.0; 0.0]: %0.2f\n" result;;

(* Large case - length of a vector with 10000 elements *)
let v = create 10000 1.0;;
let result = length v;;
Printf.printf "length of vector with 10000 elements: %0.2f\n" result;;

(* Test Cases for angle *)
Printf.printf "Test Cases for angle\n";;

(* Normal case *)
let v1 = [1.0; 0.0];;
let v2 = [0.0; 1.0];;
let result = angle v1 v2;;
Printf.printf "angle between [1.0; 0.0] and [0.0; 1.0]: %0.2f\n" result;;

(* Edge case - zero vector *)
let v1 = [0.0; 0.0];;
let v2 = [1.0; 0.0];;
try
  let result = angle v1 v2 in
    (* If no exception occurs, print the result (just for completeness) *)
    Printf.printf "angle between [0.0; 0.0] and [1.0; 0.0]: %0.2f\n" result
with Invalid_argument msg -> Printf.printf "Error: %s\n" msg;;
print_newline ();;

(* Large case - angle between two large vectors *)
let v1 = create 10000 1.0;;
let v2 = create 10000 2.0;;
let result = angle v1 v2;;
Printf.printf "angle between two large vectors: %0.2f\n" result;;
