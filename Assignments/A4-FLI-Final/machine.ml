(*===================================================================================*)
                  (* Assignment 4 - Functional Language Interpreters *)
                  (*            2023CS50334 - Yash Rawat             *)

(* File Name  : machine.ml
   
   File Use   : Implement the API for secd_machine as well as kirvine_machine - Define
                custom syntactic types as well as 
              : Env Module to encapsulate all the design implementations except types *)
              
(*===================================================================================*)

(* Required type definitions For this assignment *)

(* Variable defined to be alias of string *)
type variable = string

type machine = KRV_M | SECD_M

type lamexp =
  (* Lambda Calculi expressions *)
  | V of variable
  | App of lamexp * lamexp
  | Lam of variable * lamexp
  (* Support for other expression categories *)
  | Num of int
  | Bool of bool 
  | Plus of lamexp * lamexp
  | Sub of lamexp * lamexp
  | Times of lamexp * lamexp
  | Not of lamexp
  | And of lamexp * lamexp
  | Or of lamexp * lamexp
  | Eq of lamexp * lamexp
  | Gt of lamexp * lamexp
  | Lt of lamexp * lamexp
  | GEq of lamexp * lamexp
  | LEq of lamexp * lamexp
  | IFTE of lamexp * lamexp * lamexp


type prim_ans = N of int | B of bool

type opcode =
  | LOOKUP of variable
  | MkCLOS of variable * opcode list
  | APP
  | RET
  | LDN of int 
  | LDB of bool 
  | ADD | MUL | NOT | AND | OR 
  | EQ | GT | LT | GEQ | LEQ 
  | COND of opcode list * opcode list

type secd_code = opcode list

type answers = 
  | Prim of prim_ans
  | Clos of closure ref  
and closure = 
  | KRV_Clos of krv_clos 
  | SECD_Clos of secd_clos
and krv_clos = {
  expr : lamexp;
  table : gamma ref;
}
and secd_clos = {
  param : variable;
  code : secd_code ref;
  table : gamma ref;
}
and gamma = {
  mutable bindings : (variable,answers) Hashtbl.t;
  mutable parent : gamma ref option
}

exception Invalid_Gamma of machine * string 
exception Invalid_Closure of machine * string
exception Invalid_Answer of machine * string 
exception Invalid_Closure_Bound of string
exception Stack_Underflow of string
exception Var_Not_Found of string
exception BType_Error of lamexp * lamexp * string
exception UType_Error of lamexp * lamexp * string

exception Invalid_Exp of lamexp * string


(*===================================================================================*)
(*===================================================================================*)

let verify_closure (machine_type : machine) (clr : closure ref) : (closure ref) =
  match machine_type, !clr with 
    | KRV_M, KRV_Clos _ -> clr
    | KRV_M, _ -> raise(Invalid_Closure(KRV_M,"Invalid Closure type encountered during KRV_Machine Execution"))
    | SECD_M, SECD_Clos _ -> clr 
    | SECD_M, _ -> raise(Invalid_Closure(SECD_M, "Invalid Closure type encountered during SECD_Machine Execution"))

let verify_answer (machine_type : machine) (ans : answers ref) : (answers ref) = 
  match machine_type, !ans with 
    | _ , Clos(clr) -> let _  = verify_closure machine_type clr in ans
    | _ , Prim (_) -> ans
let verify_binding (machine_type : machine) (binding_pair : (variable * answers) ref): (variable * answers) ref =
  let _,ans = !binding_pair in
  let _ = verify_answer machine_type (ref ans) in 
  binding_pair 

(*===================================================================================*)
(*===================================================================================*)

let hash_table_size_init = 4

let create_gamma (parent_ref : gamma ref option) : gamma = {
  bindings = Hashtbl.create hash_table_size_init;
  parent = parent_ref;
}

let create_new_gamma () : gamma =
  create_gamma None

let add_binding (table : gamma ref) (var : variable) (ans : answers) =
  Hashtbl.replace !table.bindings var ans
  

let create_krv_clos (exp : lamexp) (table_ref : gamma ref) : closure =
  let krv_clr = {expr = exp; table = table_ref} in 
  KRV_Clos krv_clr

let rec lookup (table : gamma ref) (var : variable) : answers =
  match Hashtbl.find_opt !table.bindings var with
  | Some(ans) -> ans
  | None -> (
      match !table.parent with
      | Some(parent) -> lookup parent var
      | None -> raise (Var_Not_Found ("Variable not found: " ^ var))
    )
  
let add_child (parent : gamma ref) (child : gamma ref) =
  !(child).parent <- Some(parent) (* C Style Mutation of references *)

(*===================================================================================*)
(*===================================================================================*)

let plus_helper (cl1 : closure) (cl2 : closure) : closure = 
  match cl1, cl2 with 
    | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
        | Num(n1), Num(n2) -> create_krv_clos (Num (n1+n2)) (ref (create_new_gamma ())) 
        | _,_ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for addition")) 
    )
    | _ -> raise(Invalid_Closure(KRV_M,"Implemented Plus_helper for Kirvine Machine as of now"))

let sub_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Num (n1 - n2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for subtraction"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Sub_helper for Kirvine Machine as of now"))

let times_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Num (n1 * n2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for multiplication"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Times_helper for Kirvine Machine as of now"))

let and_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Bool(b1), Bool(b2) -> create_krv_clos (Bool(b1 && b2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for logical AND"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented And_helper for Kirvine Machine as of now"))

let or_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Bool(b1), Bool(b2) -> create_krv_clos (Bool(b1 || b2)) (ref (create_new_gamma()))
      | _, _ -> raise(BType_Error(krv1.expr, krv2.expr,"Invalid Expression for logical OR"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Or_helper for Kirvine Machine as of now"))

let eq_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 = n2)) (ref (create_new_gamma ()))
      | Bool(b1), Bool(b2) -> create_krv_clos (Bool(b1 = b2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for equality comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Eq_helper for Kirvine Machine as of now"))

let gt_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 > n2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for greater than comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Gt_helper for Kirvine Machine as of now"))

let lt_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 < n2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for less than comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Lt_helper for Kirvine Machine as of now"))
  
let geq_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 >= n2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for greater than or equal comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented GEq_helper for Kirvine Machine as of now"))
  
let leq_helper (cl1 : closure) (cl2 : closure) : closure =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 <= n2)) (ref (create_new_gamma ()))
      | _, _ -> raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for less than or equal comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented LEq_helper for Kirvine Machine as of now"))  

(*===================================================================================*)
(*===================================================================================*)

let rec krv_machine (cl : closure) (stack : closure list) : closure = 
  let rec tail_helper_krv (cl : closure) (stack : closure list) : closure =
    let _ = verify_closure KRV_M (ref cl) in
    match cl with 
      | KRV_Clos krv_cl -> (
        match krv_cl.expr with
          | Num(_) | Bool(_) -> (
              cl
            )

          | V (var) -> (
              match lookup krv_cl.table var with 
              | Clos(clr) ->  (
                tail_helper_krv !clr stack
              )
              | _ -> raise(Invalid_Closure_Bound(var ^ " : This variable is bound to a non-closure type answer in Kirvine Machine Exeution"))
            )

          | App(e1, e2) ->(
              let arg_closure = create_krv_clos e2 krv_cl.table in
              let operator_closure = create_krv_clos e1 krv_cl.table in
              tail_helper_krv operator_closure (arg_closure :: stack)
            )

          | Lam(x, e') -> (
              match stack with
              | [] ->
                  (* No arguments left on the stack; return the current closure *)
                  cl
              | arg_cl :: rest_stack ->
                  (* Extend the environment with the argument binding *)
                  let new_gamma = create_gamma (Some krv_cl.table) in
                  add_binding (ref new_gamma) x (Clos(ref arg_cl));
                  
                  (* Create a new closure for the body of the lambda abstraction *)
                  let new_closure = create_krv_clos e' (ref new_gamma) in
                  
                  (* Continue evaluating with the new closure and updated stack *)
                  tail_helper_krv new_closure rest_stack
            )
          
          | Plus(e1,e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in 
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in 
              tail_helper_krv (plus_helper res_e1 res_e2) stack
          )

          | Sub(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (sub_helper res_e1 res_e2) stack
          )
          
          | Times(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (times_helper res_e1 res_e2) stack
            )
          
          | And(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (and_helper res_e1 res_e2) stack
            )
          
          | Or(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (or_helper res_e1 res_e2) stack
            )

          | Eq(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (eq_helper res_e1 res_e2) stack
            )

          | Gt(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (gt_helper res_e1 res_e2) stack
            )

          | Lt(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (lt_helper res_e1 res_e2) stack
            )

          | GEq(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (geq_helper res_e1 res_e2) stack
            )

          | LEq(e1, e2) -> (
              let res_e1 = krv_machine (create_krv_clos e1 krv_cl.table) [] in
              let res_e2 = krv_machine (create_krv_clos e2 krv_cl.table) [] in
              tail_helper_krv (leq_helper res_e1 res_e2) stack
            )   

          | IFTE(cond_exp, true_exp, false_exp) -> (
              let cond_res = krv_machine (create_krv_clos cond_exp krv_cl.table) [] in
              let take_true = ( match cond_res with 
                                | KRV_Clos (krv_cond) -> (
                                    match krv_cond.expr with 
                                      | Bool(b) -> b 
                                      | _ -> raise(Invalid_Closure(KRV_M,"Closure of conditional branch must evaluate to a boolean value"))
                                  )
                                | _ -> raise(Invalid_Closure(KRV_M,"Invalid Closure found in Execution of kirvine machine"))
                              ) in 
              let branch_exp = if take_true then true_exp else false_exp in
              tail_helper_krv (create_krv_clos branch_exp krv_cl.table) stack
          )
          
          | e -> raise(Invalid_Exp(e,"Expression Not Supported yet in KRV Machine"))
      )
      | _ -> raise(Invalid_Closure(KRV_M,"Invalid closure encountered in Kirvine Machine"))
  in
  tail_helper_krv cl stack 