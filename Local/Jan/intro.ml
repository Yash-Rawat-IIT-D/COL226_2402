(* intro.ml *)

(* Unit Type *)
(* Corresponding to --> 1 Set *)
let myunit = ();;

(* Boolean Type *)
(* Corresponding to --> 2 Set *)

let mybool = true;;
let mybool2 = false;;

let and_check = (mybool && mybool2);;
let or_check = (mybool || mybool2);;
let not_check = not mybool;;

let union f g = fun x -> (f x) || (g x);;

(* Integers ---> Z *)

let a = 5;;
let b = 10;;



(* String operations *)

let str1 = "Hello";;
let str2 = "World";;

let concatenated_str = str1 ^ " " ^ str2;;
let str_length = String.length concatenated_str;;
let uppercase_str = String.uppercase_ascii concatenated_str;;
let lowercase_str = String.lowercase_ascii concatenated_str;;
