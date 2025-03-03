open List;;

type symbol = string * int;;

type signature = symbol list;;

let my_sig = [("0",0);("1",0);("+",2);("*",2)];;

type tree = V of string | C of {node : symbol; children:tree list};;

let rec is_valid_tree t = match t with 
  | V s -> true
  | C {node = (s,n); children = l} -> if ((length l) = n && (mem (s,n) my_sig )) then 
                                      fold_left (fun acc x -> acc && is_valid_tree x) true l
                                      else false;;

let rec vars_tree t = match t with 
  | V s -> [s]
  | C {node = (s,n); children = l} -> fold_left (fun acc x -> acc @ (vars_tree x)) [] l;;

let rec ht t = match t with
| V s -> 0
| C {node = (s,n); children = l } -> 1 + fold_left(fun acc x -> max acc (ht x)) 0 l


                                    

let x = V "x";;
let y = V "y";;
let z = V "z";;

let zero =  C {node = ("0",0); children = []};;
let one =  C {node = ("1",0); children = []};;
let plus_zero_one = C {node = ("+",2); children = [zero;one]};;
let plus_x_y = C {node = ("+",2); children = [x;y]};;

let plus_x_y_z = C {node = ("+",2); children = [x;y;z]};;

let check1 = is_valid_tree plus_x_y;;

print_string ("check1 = " ^ string_of_bool check1 ^ "\n");;