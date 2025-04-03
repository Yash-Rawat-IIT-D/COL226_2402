(*===================================================================================*)
                (* Assignment 4 - Functional Language Interpreters *)
                (*            2023CS50334 - Yash Rawat             *)

(* File Name  : secd_machine.ml
   
   File Use   : Implement the API for secd Stacks, Environments, Closures and 
                Dumps that will be required for implementation and execution of 
                SECD  Stack machine
  
              : Module Like interface provided for S,E,C,D etc.
                Which helps in code modularity as well as testing of implementation *)

(*===================================================================================*)


(** SECD Machine Core Components *)

(** Abstract syntax for supported expressions *)
type exp = 
  | Num of int 
  | Bool of bool 
  | Var of string  
  | Lambda of string * exp 
  | App of exp * exp
  | Plus of exp * exp 
  | Times of exp * exp
  | Not of exp 
  | And of exp * exp 
  | Or of exp * exp 
  | Eq of exp * exp 
  | Gt of exp * exp

(** SECD Machine Operational Components *)
type opcode = 
  | LDN of int          (* Load numeric constant *)
  | LDB of bool         (* Load boolean constant *)
  | LOOKUP of string    (* Variable lookup *)
  | MKCLOS of string * secd_code  (* Create closure *)
  | APP                 (* Function application *)
  | RET                 (* Return from function *)
  | ADD | MUL | NOT | AND | OR | EQ | GT  (* Primitive operations *)

and secd_code = opcode list

(** Value closures = code + environment *)
and closure = {
  param : string;
  code : secd_code;
  env : secd_env ref  (* Reference for mutable sharing *)
}

(** Value representation *)
and value = 
  | Prim of prim_val
  | Clos of closure
and prim_val = N of int | B of bool

(** Environment structure *)
and secd_env = {
  mutable bindings : (string, value) Hashtbl.t;
  parent : secd_env option ref;  (* Lexical parent *)
}

(** Machine state components *)
type secd_state = {
  stack : value list;       (* S - Value stack *)
  env : secd_env ref;       (* E - Current environment *)
  code : secd_code;         (* C - Remaining code *)
  dump : secd_state list;   (* D - Continuation stack *)
}




