type symbol = string * int
type signature = symbol list
val my_sig : (string * int) List.t
type tree = V of string | C of { node : symbol; children : tree list; }
val is_valid_tree : tree -> bool
val x : tree
val y : tree
val z : tree
val zero : tree
val one : tree
val plus_zero_one : tree
val plus_x_y : tree
val plus_x_y_z : tree
val check1 : bool
