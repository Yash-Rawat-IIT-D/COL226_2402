(* User Defined Types *)

(*
User defined types are defined using the type keyword.
The syntax is:
type <type_name> = <type_definition>
where <type_name> is the name of the new type and <type_definition> is the definition of the new type.
*)

(* Example 1: Aliasing an already existing type *)

(* type age = int;;   *)

let v = 20;;
let q = 30;;

let sum = v + q;;

(* Example 2: Defining a new type using pre-existing types *)

type str_int_tup = string * int;;

let person = ("John", 25);;
let yolo = ('a',1);;

(* Example 3: Defining a new type *)

type myBool = T | F;; (* myBool is a new type with two values T and F *)

(*
To be more specific, myBool has two constructors T and F.
The constructors are used to create values of the type myBool but
they cannot take additional arguments, and represent themselves.
*)

let myBool_T = T;;
let myBool_F = F;;

let myBool_and x y = match x with
  | T -> y
  | F -> F
;;


let myBool_or x y = match x with
  | T -> T
  | F -> y
;;

let myBool_not x = match x with
  | T -> F
  | F -> T
;;

let myBool_xor x y = match x with
  | T -> myBool_not y
  | F -> y
;;

let myBool_to_Bool x = match x with
  | T -> true
  | F -> false
;;


(* Now We shall test mybool's correctness of the myBool functions *)

let test_and_T_T = ( myBool_to_Bool (myBool_and myBool_T myBool_T) = 
                   (&&) (myBool_to_Bool myBool_T) (myBool_to_Bool myBool_F) );;

let test_and_T_F = ( myBool_to_Bool (myBool_and myBool_T myBool_F) = 
                   (&&) (myBool_to_Bool myBool_T) (myBool_to_Bool myBool_F) );;

let test_and_F_T = ( myBool_to_Bool (myBool_and myBool_F myBool_T) = 
                   (&&) (myBool_to_Bool myBool_F) (myBool_to_Bool myBool_T) );;
let test_and_F_F = ( myBool_to_Bool (myBool_and myBool_F myBool_F) = 
                   (&&) (myBool_to_Bool myBool_F) (myBool_to_Bool myBool_F) );;

(* Example 4: Defining a new type using pre-existing types *)
