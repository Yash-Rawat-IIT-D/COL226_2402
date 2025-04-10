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
  | ADD |SUB | MUL 
  | NOT | AND | OR 
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

(* Exclusively Used for SECD Machine exectuion *)
type secd_stack = answers list
type secd_triple = {
  st : secd_stack ref;
  env : gamma ref;
  cd : secd_code ref;
}
type secd_dump = secd_triple list
type secd_state = SECD_State of secd_stack * gamma * secd_code * secd_dump

exception Invalid_Gamma of machine * string 
exception Invalid_Closure of machine * string
exception Invalid_Answer of machine * string 
exception Invalid_Closure_Bound of string
exception Stack_Underflow of string
exception Empty_Stack of string
exception Stack_Overflow of string
exception Var_Not_Found of string
exception BType_Error of lamexp * lamexp * string
exception UType_Error of lamexp * lamexp * string
exception Invalid_Exp of lamexp * string
exception Stuck of machine * string


(*===================================================================================*)
              (* Helper Methods to verify that correct sub-types are
                          being used in respective machines *)
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

let rec string_of_lamexp (e : lamexp) : string = 
  match e with
  | V(var) -> "V(" ^ var ^ ")"
  | App(e1, e2) -> "App(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Lam(var, e') -> "Lam(" ^ "V(" ^var ^  "), " ^ string_of_lamexp e' ^ ")"
  | Num(n) -> "Num(" ^ string_of_int n ^ ")"
  | Bool(b) -> "Bool(" ^ string_of_bool b ^ ")"
  | Plus(e1, e2) -> "Plus(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Sub(e1, e2) -> "Sub(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Times(e1, e2) -> "Times(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Not(e') -> "Not(" ^ string_of_lamexp e' ^ ")"
  | And(e1, e2) -> "And(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Or(e1, e2) -> "Or(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Eq(e1, e2) -> "Eq(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Gt(e1, e2) -> "Gt(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | Lt(e1, e2) -> "Lt(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | GEq(e1, e2) -> "GEq(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | LEq(e1, e2) -> "LEq(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ")"
  | IFTE(e1, e2, e3) -> "IFTE(" ^ string_of_lamexp e1 ^ ", " ^ string_of_lamexp e2 ^ ", " ^ string_of_lamexp e3 ^ ")"

(*===================================================================================*)
(*===================================================================================*)

(* Helper Functions for Gamma - Handling Variable binding to answers 
  In a statically scoped manner - Use of Linked List of Hash-Tables *)

let hash_table_size_init = 4

let create_gamma (parent_ref : gamma ref option) : gamma = {
  bindings = Hashtbl.create hash_table_size_init;
  parent = parent_ref;
}

let create_new_gamma () : gamma =
  create_gamma None

let add_binding (table : gamma ref) (var : variable) (ans : answers) =
  Hashtbl.replace !table.bindings var ans

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

(* Method to create and a closure for krivine machine *)
let create_krv_clos (exp : lamexp) (table_ref : gamma ref) : krv_clos =
  let krv_clr = {expr = exp; table = table_ref} in 
  krv_clr


(*===================================================================================*)
(*===================================================================================*)
                              (* Krivine Machine *)
(*===================================================================================*)
(*===================================================================================*)

(* Helper Methods for Kirvine Machine *)
let plus_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos = 
  match cl1, cl2 with 
    | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
        | Num(n1), Num(n2) -> create_krv_clos (Num (n1+n2)) (gamma_ref) 
        | _,_ -> if over_ride then create_krv_clos (Plus(krv1.expr,krv2.expr)) (gamma_ref)
                 else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for addition")) 
    )
    | _ -> raise(Invalid_Closure(KRV_M,"Implemented Plus_helper for Kirvine Machine as of now"))

 let sub_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Num (n1 - n2)) (gamma_ref)
      | _, _ ->if over_ride then create_krv_clos (Sub(krv1.expr,krv2.expr)) (gamma_ref) 
               else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for subtraction"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Sub_helper for Kirvine Machine as of now"))

let times_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Num (n1 * n2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (Times(krv1.expr,krv2.expr)) (gamma_ref)
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for multiplication"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Times_helper for Kirvine Machine as of now"))

let and_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Bool(b1), Bool(b2) -> create_krv_clos (Bool(b1 && b2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (And(krv1.expr,krv2.expr)) (gamma_ref)
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for logical AND"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented And_helper for Kirvine Machine as of now"))

let or_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Bool(b1), Bool(b2) -> create_krv_clos (Bool(b1 || b2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (Or(krv1.expr,krv2.expr)) (gamma_ref)
                else raise(BType_Error(krv1.expr, krv2.expr,"Invalid Expression for logical OR"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Or_helper for Kirvine Machine as of now"))

let not_helper (cl : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl with
  | KRV_Clos krv -> (
      match krv.expr with
      | Bool(b) -> create_krv_clos (Bool(not b)) (gamma_ref)
      | _ -> if over_ride then create_krv_clos (Not(krv.expr)) (gamma_ref)
              else raise(BType_Error(krv.expr, krv.expr, "Invalid Expression for logical NOT"))
    )
  | _ -> raise(Invalid_Closure(KRV_M, "Implemented Not_helper for Kirvine Machine as of now"))

let eq_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 = n2)) (gamma_ref)
      | Bool(b1), Bool(b2) -> create_krv_clos (Bool(b1 = b2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (Eq(krv1.expr,krv2.expr)) (gamma_ref) 
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for equality comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Eq_helper for Kirvine Machine as of now"))

let gt_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 > n2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (Gt(krv1.expr,krv2.expr)) (gamma_ref)
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for greater than comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Gt_helper for Kirvine Machine as of now"))

let lt_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 < n2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (Lt(krv1.expr,krv2.expr)) (gamma_ref)
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for less than comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented Lt_helper for Kirvine Machine as of now"))
  
let geq_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 >= n2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (GEq(krv1.expr,krv2.expr)) (gamma_ref)
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for greater than or equal comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented GEq_helper for Kirvine Machine as of now"))
  
let leq_helper (cl1 : closure) (cl2 : closure) (gamma_ref : gamma ref) (over_ride : bool) : krv_clos =
  match cl1, cl2 with
  | KRV_Clos krv1, KRV_Clos krv2 -> (
      match krv1.expr, krv2.expr with
      | Num(n1), Num(n2) -> create_krv_clos (Bool(n1 <= n2)) (gamma_ref)
      | _, _ -> if over_ride then create_krv_clos (LEq(krv1.expr,krv2.expr)) (gamma_ref) 
                else raise (BType_Error(krv1.expr, krv2.expr, "Invalid Expression for less than or equal comparison"))
    )
  | _ -> raise(Invalid_Closure(KRV_M,"Implemented LEq_helper for Kirvine Machine as of now"))  

(* let rec unload (cl : closure) : lamexp =
  match cl with
  | KRV_Clos krv_cl -> (
      match krv_cl.expr with
      | Lam(x, e) -> 
          Lam(x, unload_expr e krv_cl.table)
      | _ -> unload_expr krv_cl.expr krv_cl.table
    )
  | _ -> failwith "Unloading Implemented for KRV_Closure Only !"

and unload_expr (expr : lamexp) (env : gamma ref) : lamexp =
  match expr with
  | V(var) -> (
      try
        match lookup env var with
        | Clos(cl) -> V(var)     (* Actually substitute the variable *)
        | _ -> V(var)
      with Var_Not_Found _ -> V(var)
    )
  | App(e1, e2) -> 
      App(unload_expr e1 env, unload_expr e2 env)
  | Lam(x, e) ->
      (* Handle variable capture by creating a fresh environment *)
      let new_env = ref (create_gamma (Some env)) in
      Lam(x, unload_expr e new_env)
  | e -> e  Other expressions remain unchanged *)

let rec subs_var (expr : lamexp) (env : gamma ref) : lamexp =
  match expr with
  | V(var) -> (
    try
      match lookup env var with
      | Clos(cl) -> (
        match !cl with 
        | KRV_Clos krv_inner -> (
          (* Check if this is a self-reference to avoid infinite recursion *)
          match krv_inner.expr with 
          | V(x) -> V(x)
          | _ -> subs_var krv_inner.expr env
        )
        | _ -> raise (Invalid_Closure(KRV_M, "foobarbaz"))
      )
      | _ -> V(var)
    with Var_Not_Found _ -> V(var)
  )
  | App(e1, e2) -> 
      App(subs_var e1 env, subs_var e2 env)
  | Lam(x, e) ->
      (* For lambda expressions, create a fresh environment to avoid variable capture *)
      let new_env = ref (create_gamma (Some env)) in
      
      (* Explicitly bind x to itself to shadow any bindings in parent environments *)
      add_binding new_env x (Clos(ref (KRV_Clos {expr = V(x); table = ref (create_new_gamma())})));
      
      (* Now substitute in the body with the updated environment *)
      Lam(x, subs_var e new_env)
  | Plus(e1, e2) ->
      Plus(subs_var e1 env, subs_var e2 env)
  | Times(e1, e2) ->
      Times(subs_var e1 env, subs_var e2 env)
  | And(e1, e2) ->
      And(subs_var e1 env, subs_var e2 env)
  | Or(e1, e2) ->
      Or(subs_var e1 env, subs_var e2 env)
  | Not(e) ->
      Not(subs_var e env)
  | Eq(e1, e2) ->
      Eq(subs_var e1 env, subs_var e2 env)
  | Gt(e1, e2) ->
      Gt(subs_var e1 env, subs_var e2 env)
  | e -> e  (* Other expressions remain unchanged - Bool and Num Cases *)
  
  (* Simplified unload_routine that makes a top-level call to subs_var *)
and unload_routine (cl : closure) (table : gamma ref) : krv_clos = 
    match cl with
    | KRV_Clos krv_cl -> 
        (* Apply subs_var directly to the expression *)
        (* let _ = print_endline ("Enter Unload :" ^ string_of_lamexp krv_cl.expr) in *)
        let substituted_expr = subs_var krv_cl.expr krv_cl.table in
        create_krv_clos substituted_expr table
    | _ -> failwith "Unloading Implemented for KRV_Closure Only!"
  
(*===================================================================================*)
(*===================================================================================*)

(* Kirivine Machine Execution Semantics implemented as OCaml Method *)

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
        try
          match lookup krv_cl.table var with 
          | Clos(clr) -> tail_helper_krv !clr stack
          | _ -> raise(Invalid_Closure_Bound(var ^ " : This variable is bound to a non-closure type answer in Kirvine Machine Execution"))
        with Var_Not_Found _ -> 
          (* Free variable - can only be a valid result if stack is empty *)
          match stack with
          | [] -> cl  (* Return as is when stack is empty *)
          | _ -> raise(Invalid_Exp(V(var), "Cannot apply a free variable to arguments"))
        )

        | App(e1, e2) -> (
            match e1 with
            | Lam(_, _) | App(_, _) ->
                (* e1 is an application that might evaluate to a lambda, proceed with standard evaluation *)
                let arg_closure = create_krv_clos e2 krv_cl.table in
                tail_helper_krv (KRV_Clos(create_krv_clos e1 krv_cl.table)) (KRV_Clos(arg_closure) :: stack)
            | V(var) ->
                (* e1 is a variable, check if it's bound to a lambda or application *)
                (try
                  match lookup krv_cl.table var with
                  | Clos(clr) -> (
                      match !clr with
                      | KRV_Clos krv_inner -> (
                          match krv_inner.expr with
                          | Lam(_, _) | App(_, _) ->
                              (* Variable is bound to a lambda or application, proceed with standard evaluation *)
                              let arg_closure = create_krv_clos e2 krv_cl.table in
                              let op_closure = create_krv_clos e1 krv_cl.table in
                              tail_helper_krv (KRV_Clos op_closure) (KRV_Clos(arg_closure) :: stack)
                          | _ ->
                              (* Return partially evaluated application *)
                              if stack = [] then
                                  let _ = print_endline ("HO :" ^ string_of_lamexp krv_cl.expr) in
                                  KRV_Clos(unload_routine cl krv_cl.table)
                              else
                                  tail_helper_krv (KRV_Clos(create_krv_clos e1 krv_cl.table)) (KRV_Clos(create_krv_clos e2 krv_cl.table) :: stack)
                      )
                      | _ -> 
                          if stack = [] then
                              KRV_Clos(unload_routine cl krv_cl.table)
                          else
                              tail_helper_krv (KRV_Clos(create_krv_clos e1 krv_cl.table)) (KRV_Clos(create_krv_clos e2 krv_cl.table) :: stack)
                  )
                  | _ -> raise (Invalid_Answer(KRV_M,"foobarbaz error message"))
                with Var_Not_Found _ ->
                    if stack = [] then
                        KRV_Clos(unload_routine cl krv_cl.table)
                    else
                        tail_helper_krv (KRV_Clos(create_krv_clos e1 krv_cl.table)) (KRV_Clos(create_krv_clos e2 krv_cl.table) :: stack)
                )
            | _ ->
                (* Neither e1 nor e2 is a lambda or an application or a variable bound to a lambda/application *)
                if stack = [] then
                    KRV_Clos(unload_routine cl krv_cl.table)
                else
                    tail_helper_krv (KRV_Clos(create_krv_clos e1 krv_cl.table)) (KRV_Clos(create_krv_clos e2 krv_cl.table) :: stack)
          )
      

      | Lam(x, e') -> (
          match stack with
          | [] ->(KRV_Clos(unload_routine cl krv_cl.table))
          | arg_cl :: rest_stack ->
              (* Extend the environment with the argument binding *)
              let new_gamma = create_gamma (Some krv_cl.table) in
              add_binding (ref new_gamma) (x) (Clos(ref arg_cl));
              
              (* Create a new closure for the body of the lambda abstraction *)
              let new_closure = create_krv_clos e' (ref new_gamma) in
              
              (* Continue evaluating with the new closure and updated stack *)
              tail_helper_krv (KRV_Clos(new_closure)) rest_stack
        )
      
      | Plus(e1,e2) -> (
          let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
          let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
          let res_cl = plus_helper res_e1 res_e2 krv_cl.table true in
          match res_cl.expr = krv_cl.expr with 
          | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
          | true  -> KRV_Clos(unload_routine cl krv_cl.table)
        )

      | Sub(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = sub_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )
      
      | Times(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = times_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )
      
      | And(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = and_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )

      | Not(e) -> (
        let res_e = krv_machine (KRV_Clos(create_krv_clos e krv_cl.table)) [] in 
        let res_cl = not_helper res_e krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )
      
      | Or(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = or_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )

      | Eq(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = eq_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )

      | Gt(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = gt_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )

      | Lt(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = lt_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )

      | GEq(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = geq_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )

      | LEq(e1, e2) -> (
        let res_e1 = krv_machine (KRV_Clos(create_krv_clos e1 krv_cl.table)) [] in 
        let res_e2 = krv_machine (KRV_Clos(create_krv_clos e2 krv_cl.table)) [] in 
        let res_cl = leq_helper res_e1 res_e2 krv_cl.table true in
        match res_cl.expr = krv_cl.expr with 
        | false -> tail_helper_krv (KRV_Clos(res_cl)) stack
        | true  -> KRV_Clos(unload_routine cl krv_cl.table)
      )   

      | IFTE(cond_exp, true_exp, false_exp) -> (
          let cond_res = krv_machine (KRV_Clos(create_krv_clos cond_exp krv_cl.table)) [] in
          let take_true = ( match cond_res with 
                            | KRV_Clos (krv_cond) -> (
                              match krv_cond.expr with 
                              | Bool(b) -> b 
                              | _ -> raise(Invalid_Closure(KRV_M,"Closure of conditional branch must evaluate to a boolean value"))
                              )
                            | _ -> raise(Invalid_Closure(KRV_M,"Invalid Closure found in Execution of kirvine machine"))) in 
          let branch_exp = if take_true then true_exp else false_exp in
          tail_helper_krv (KRV_Clos(create_krv_clos branch_exp krv_cl.table)) stack
        )
      
      | e -> raise(Invalid_Exp(e,"Expression Not Supported yet in KRV Machine"))
    )
    | _ -> raise(Invalid_Closure(KRV_M,"Invalid closure encountered in Kirvine Machine"))
  in
  tail_helper_krv cl stack 


(*===================================================================================*)
(*===================================================================================*)
                    (* SECD Machine - Call by Value Semantics *)
(*===================================================================================*)
(*===================================================================================*)


(* Helper Method for SECD_Stack *)

let create_secd_stack() : secd_stack = [] 

let push_secd_st (stack : secd_stack) (value : answers) : secd_stack =
  let _ = verify_answer SECD_M (ref value) in
  value :: stack 

let pop_secd_st (stack : secd_stack) : answers * secd_stack = 
  match stack with 
  | [] -> raise (Stack_Underflow("Popping from an Empty SECD Stack"))
  | top :: rest -> let _ = verify_answer SECD_M (ref top) in (top ,rest)

let peek_secd_st (stack : secd_stack) : answers = 
  match stack with 
    | [] -> raise (Empty_Stack("Peeking in Empty SECD_Stack"))
    | top :: _ -> top

let is_empty_st (stack : secd_stack) : bool = 
  match stack with
    | [] -> true
    | _ -> false

(* Helper method to create a closure for SECD Machine *)

let create_secd_clos (var : variable) (code_ref : secd_code ref) (table_ref : gamma ref) : closure = 
  let sec_clos = {  param = var; code = code_ref; table = table_ref;} in
  SECD_Clos sec_clos

(* Helper Method for SECD_Code *)

let rec compile (expr : lamexp) : secd_code = 
  match expr with
    (* Dealing With Compilation of Lambda Calculi Expressions *)
    | V (var)           ->  [LOOKUP(var)]
    | App (e1,e2)       -> compile e1 @ compile e2 @ [APP]
    | Lam (param,body)  -> [MkCLOS(param, compile body @ [RET])]
    (* Dealing with Op-Codes for older terms !*)
    | Num n             -> [LDN n]
    | Bool b            -> [LDB b]
    | Plus(e1,e2)       -> compile e1 @ compile e2 @ [ADD]
    | Sub(e1,e2)        ->  compile e1 @ compile e2 @ [SUB]
    | Times(e1,e2)      -> compile e1 @ compile e2 @ [MUL]
    | Not(e)            -> compile e @ [NOT]
    | And(e1, e2)       -> compile e1 @ compile e2 @ [AND]  
    | Or(e1, e2)        -> compile e1 @ compile e2 @ [OR]  
    | Eq(e1, e2)        -> compile e1 @ compile e2 @ [EQ]  
    | Gt(e1, e2)        -> compile e1 @ compile e2 @ [GT]  
    | Lt(e1,e2)         -> compile e1 @ compile e2 @ [LT]
    | LEq(e1,e2)        -> compile e1 @ compile e2 @ [LEQ]
    | GEq(e1,e2)        -> compile e1 @ compile e2 @ [GEQ]
    | IFTE(e1,e2,e3)    -> compile e1 @ [COND(compile e2, compile e3)]



(* Helper Methods for SECD_Dump *)

let create_triple (stack_ref : secd_stack ref) (env_ref : gamma ref) (code_ref : secd_code ref) : secd_triple = 
  { st = stack_ref; env = env_ref; cd = code_ref }

let push_secd_dmp (dump : secd_dump) (triple : secd_triple) : secd_dump =
  triple :: dump

let pop_secd_dmp (dump : secd_dump) : secd_triple * secd_triple list = 
  match dump with
    | [] -> raise (Empty_Stack("Popping from an Empty Dump in SECD Machine"))
    | top :: rest -> (top, rest)

let peek_secd_dmp (dump : secd_dump) : secd_triple = 
  match dump with
  | [] -> raise (Empty_Stack("Peeking into an Empty Dump in SECD Machine"))
  | top :: _ -> top

let is_empty_dmp (dump : secd_dump) : bool = 
  match dump with
    | [] -> true
    | _ -> false

let secd_machine (s : secd_stack) (e : gamma) (c : secd_code) (d : secd_dump) : answers = 
  let init_state = SECD_State(s,e,c,d) in
  let rec tail_secd_helper (state : secd_state) : secd_state =
    let SECD_State(s,e,c,d) = state in
    match c with  
      | [] -> state      (* Exhausted the opcodes *)
      | op :: rest_code -> (
          match op with 
            | LDN n -> (  
                let ans = Prim(N(n)) in 
                let _ = verify_answer SECD_M (ref ans) in 
                tail_secd_helper (SECD_State((push_secd_st s ans),e,rest_code,d))  
              )

            | LDB b -> (
                let ans = Prim(B(b)) in 
                let _ = verify_answer SECD_M (ref ans) in
                tail_secd_helper (SECD_State((push_secd_st s ans),e,rest_code,d))
              )

            | LOOKUP var -> (
                try
                  let ans = lookup (ref e) var in
                  let _ = verify_answer SECD_M (ref ans) in 
                  tail_secd_helper (SECD_State((push_secd_st s ans),e,rest_code,d))
                with Var_Not_Found _ ->
                  raise (Var_Not_Found ("Unbound variable in call-by-value context of SECD Machine : " ^ var))  
              )

            | MkCLOS(param,body_code) -> (
                let clos_ans = create_secd_clos param (ref body_code) (ref e) in
                let ans = Clos(ref clos_ans) in
                tail_secd_helper (SECD_State((push_secd_st s ans),e,rest_code,d))
              )

            | APP -> (
                match s with 
                | arg :: Clos(clos) :: rest_stack -> (
                    let _ = verify_closure SECD_M clos in
                    match !clos with 
                      | SECD_Clos sec_clos -> ( 
                          let aug_table = create_gamma (Some(sec_clos.table)) in
                          add_binding (ref aug_table) (sec_clos.param) arg;
                          let old_context = create_triple (ref rest_stack) (ref e) (ref rest_code) in
                          tail_secd_helper(SECD_State((create_secd_stack()),aug_table,!(sec_clos.code),push_secd_dmp d old_context))

                        )
                      | _ -> raise(Invalid_Closure(SECD_M, "Invalid Closure type encountered during SECD_Machine Execution"))
                  )
                | _ -> raise(Stuck(SECD_M,"Invalid stack for APP opcode : stack should have form : a::⟨⟨⟨x,c′,γ'⟩⟩⟩::S,
                                \n Where a represents argument bound to parameter x"))
              )

            | RET -> (
                match s, d with
                | [result], ret_context :: rest_dump -> (
                  let new_s = result :: !(ret_context.st) in
                  let new_e = ret_context.env in
                  let new_c = !(ret_context.cd) in
                  tail_secd_helper(SECD_State(new_s,!new_e,new_c,rest_dump))
                  )
                | _, [] -> raise (Stuck (SECD_M,"RET with empty dump : No Context found to return to after abstraction call"))
                | _, _ -> raise (Stuck (SECD_M,"Invalid stack for RET opcode : Function Call's should end with only one answer
                                        on current context's stack"))
              )

            | ADD -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N (n2))) :: rest_stack -> (
                      let ans = Prim((N (n1+n2))) in
                      tail_secd_helper(SECD_State(push_secd_st rest_stack ans,e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for ADD opcode : Expected atleast two arguments on stack to be of primitive 
                  \n value type with integer associated values"))
              )

            | SUB -> (
              match s with 
                | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                    let ans = Prim((N(n2-n1))) in (* Cheeky - Left is evaluated first so it would be pushed first*)
                    tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                  )
                | _ -> raise (Stuck (SECD_M,"Invalid stack for SUB opcode: Expected at least two integers on stack"))
              )
                
            | MUL -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                      let ans = Prim((N(n1*n2))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for MUL opcode: Expected at least two integers on stack"))
              )
                
            | NOT -> (
                match s with 
                  | Prim(B(b)) :: rest_stack -> (
                      let ans = Prim((B(not b))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for NOT opcode: Expected a boolean on stack"))
              )
            
            | AND -> (
                match s with 
                  | Prim(B(b1)) :: Prim((B(b2))) :: rest_stack -> (
                      let ans = Prim((B(b1 && b2))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for AND opcode: Expected at least two booleans on stack"))
              )
            
            | OR -> (
                match s with 
                  | Prim(B(b1)) :: Prim((B(b2))) :: rest_stack -> (
                      let ans = Prim((B(b1 || b2))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for OR opcode: Expected at least two booleans on stack"))
              )
            
            | EQ -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                      let ans = Prim((B(n1 = n2))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | Prim(B(b1)) :: Prim((B(b2))) :: rest_stack -> (
                      let ans = Prim((B(b1 = b2))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for EQ opcode: Expected two comparable values on stack"))
              )
            
            | GT -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                      let ans = Prim((B(n2 > n1))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for GT opcode: Expected two integers on stack"))
              )
            
            | LT -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                      let ans = Prim((B(n2 < n1))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for LT opcode: Expected two integers on stack"))
              )
            
            | GEQ -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                      let ans = Prim((B(n2 >= n1))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for GEQ opcode: Expected two integers on stack"))
              )
            
            | LEQ -> (
                match s with 
                  | Prim(N(n1)) :: Prim((N(n2))) :: rest_stack -> (
                      let ans = Prim((B(n2 <= n1))) in
                      tail_secd_helper(SECD_State((push_secd_st rest_stack ans),e,rest_code,d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for LEQ opcode: Expected two integers on stack"))
              )
            
            | COND(then_code, else_code) -> (
                match s with
                  | Prim(B(b)) :: rest_stack -> (
                      let branch_code = if b then then_code else else_code in
                      tail_secd_helper(SECD_State(rest_stack, e, branch_code @ rest_code, d))
                    )
                  | _ -> raise (Stuck (SECD_M,"Invalid stack for COND opcode: Expected a boolean on stack"))
              )
        )
  in  
  let final_state  = tail_secd_helper init_state in
  let SECD_State(final_ans_stack,_,_,_) = final_state in
  match final_ans_stack with 
    | [] -> raise(Empty_Stack("Final Answer Stack in SECD Machine is empty !"))
    | final_ans::[] -> final_ans
    | foo :: others -> raise(Stack_Overflow("Final Answer Stack in SECD Machine contains more than 1 elements !"))