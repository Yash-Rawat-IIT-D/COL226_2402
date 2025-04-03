(*===================================================================================*)
                (* Assignment 4 - Functional Language Interpreters *)
                (*            2023CS50334 - Yash Rawat             *)

(* File Name  : common.ml
   File Use   : Define the common interface for the other files such as defining
                recursive types to be used throughout the program
              : Module Like interface , will use Open keyword to import *)
(*===================================================================================*)


(* exp : Denotes the Abstract Syntactic Category of the expression supported by the toy language
         Largely Similar to what has been discussed throughout the first half of semester
         
         Intereseting addition of Lambda expressions (Provide us a way to implement abstractions on exp 
         as first class members of exp) and their Applications *)

type exp = Num of int | Plus of exp * exp | Times of exp * exp
          | Bool of bool | Not of exp | And of exp * exp | Or of exp * exp
          | Eq of exp * exp | Gt of exp * exp 
          | Var of string  
          | Lambda of string * exp 
          | App of exp * exp

(* values : The semantic of value of an expression that is arrived upon by application of 
            denotional semantics on expressions or their canonical represenation ansewers *)

type values = N of int | B of bool

(* typ : Syntactic Category of types that can be associated with expresisons using a 
         type checking method*)

type typ = INT_T | BOOL_T


(* Modelling theoretical Closure's Using a cl_expr : expr in our toy language and cl_table 
   : table_node representing the table environment at the time of packing of closure

   The Kirvine Stack Machine operates on closures and closure stacks so 
   this key detail is important *)

type krv_closure = {
  cl_expr : exp;
  cl_table : krv_table_node ref; 
}

(* Creating A Linked List Like Data Structure that will handle the table's in closure as well
   Preserve the essence of variable lookup's , local bindings and so by implementing simple API
   in table.ml  
   
   An implementation detail is that ref and mutable syntax is used to model an efficient table implementation
   in this assignment while still taking advantage of OCaml's builtin's to do the heavy lifting for us

   *)

and krv_table_node = {
  mutable table : (string, krv_closure) Hashtbl.t;       (* Table -> The actual mapping of variables and values *)
  mutable parent : krv_table_node option ref;           (* Linked List like using option (Like Option Enum of Rust) *)
  mutable children : krv_table_node list ref;           (* Stack of scopes inside current scope *)
}

(* Notice the use of and - Used for defition of mutually defined syntactic categories 
   Which for our case is closure and tables (atleast for kirivine machine)*)

