type exp = Num of int 
         | Plus of exp * exp
         | Times of exp * exp 
;; 

let t1 = Num 1;;
let t2 = Num 3;;
let t3 = Num 5;;
let t4 = Num 6;;
let t5 = Plus (t1, t2);;
let t6 = Times (t3, t4);;
let t7 = Times (t1, t2);;
let t8 = Plus (t3, t4);;
let t9 = Plus (t5, t6);;
let t10 = Times (t7, t8);;
let t11 = Plus (t9, t10);;
let rec size e = match e with
  | Num _ -> 1
  | Plus (e1, e2) -> 1 + size e1 + size e2
  | Times (e1, e2) -> 1 + size e1 + size e2
;;

let rec ht e = match e with 
  | Num _ -> 0
  | Plus (e1, e2) -> 1 + (max (ht e1) (ht e2))
  | Times (e1, e2) -> 1 + (max (ht e1) (ht e2))

let rec post_trav e = match e with
  | Num n -> Int.to_string n
  | Plus (e1, e2) ->  (post_trav e1) ^ " " ^ (post_trav e2) ^ " +" 
  | Times (e1, e2) -> (post_trav e1) ^ " " ^ (post_trav e2) ^ " *"
;;

let rec in_trav e = match e with
  | Num n -> Int.to_string n
  | Plus (e1, e2) ->  "(" ^ (in_trav e1) ^ "+" ^ (in_trav e2) ^ ")"
  | Times (e1, e2) -> "(" ^ (in_trav e1) ^  "*" ^ (in_trav e2) ^ ")"


let rec eval e = match e with
  | Num n -> n
  | Plus (e1, e2) -> (eval e1) + (eval e2)
  | Times (e1, e2) -> (eval e1) * (eval e2)
;;


(* 
Gives us a canonical representation of the expression tree.
*)

let plus e1 e2 = let v1 = eval e1 in let v2 = eval e2 in Num (v1 + v2);;
let times e1 e2 = let v1 = eval e1 in let v2 = eval e2 in Num (v1 * v2);;
let rec calc e = match e with
  | Num n -> Num n
  | Plus (e1, e2) -> plus e1 e2
  | Times (e1, e2) -> times e1 e2

  



let s = in_trav t11;;
let s1 = in_trav t9;;

(* 

let sbar = post_trav t11;;
let sbar1 = post_trav t9;; 
Printf.printf "%s\n" sbar1;;
Printf.printf "%s\n" sbar;; 

*)
(* 

Post Order Traversal gives us the postfix notation of the expression tree.
Which is useful for evaluating the expression tree using a stack machine.

*)

Printf.printf "%s\n" s1;;
Printf.printf "%s\n" s;;

let v = eval t11;;
let v1 = eval t9;;

Printf.printf "%d\n" v1;;
Printf.printf "%d\n" v;;




