(* 
Now we will Create natural numbers using 
Neumann Construction of Natural Numbers
*)

type nat = Z | S of nat;;

(*
nat has a base case of Z (0 of the Naturals)
and otherwise is made by a constructor S that takes a nat 
and returns a nat

The Canonical values of nat are : Z , (S Z), S (S Z), S (S (S Z)) and so on

nat thus represents the inductively defined , set closed under
the S operation
*)

let nat_zero = Z;;
let nat_one = S Z;;
let nat_two = S (S Z);;
let nat_three = S (S (S Z));;

(*
This (Structural) Inductive definition of nat will allow us to
define functions on nat by structural recursion as we will see
*)

(*
Look how case based pattern matching on constructors of nat 
is used to define not_zero
*)

let is_not_zero n = match n with
  | Z -> false
  | S _ -> true;;

let myb = is_not_zero nat_zero;;
let myb2 = is_not_zero nat_one;;

(* Recursive function to find the conversion *)

let rec nat_2_int n = match n with
  | Z -> 0
  | S m -> 1 + (nat_2_int m);;

let myint = nat_2_int nat_three;;

let rec nat_add x y = match x with
  | Z -> y
  | S m -> S (nat_add m y)
;; 

let nat_six = nat_add nat_three nat_three;;
let my6 = nat_2_int nat_six;;