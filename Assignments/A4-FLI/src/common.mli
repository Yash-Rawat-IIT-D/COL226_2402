type exp =
    Num of int
  | Plus of exp * exp
  | Times of exp * exp
  | Bool of bool
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
  | Eq of exp * exp
  | Gt of exp * exp
  | Var of string
  | Lambda of string * exp
  | App of exp * exp
type values = N of int | B of bool
type typ = INT_T | BOOL_T
type table_node = {
  mutable table : (string, values) Hashtbl.t;
  mutable parent : table_node option ref;
  mutable children : table_node list ref;
}
type closure = { cl_expr : exp; cl_table : table_node ref; }
