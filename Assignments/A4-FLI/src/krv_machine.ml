(*===================================================================================*)
                (* Assignment 4 - Functional Language Interpreters *)
                (*            2023CS50334 - Yash Rawat             *)

(* File Name  : krv_machine.ml
   
   File Use   : Implement the API for krv_tables and krv_closures that will 
                be required for implementation and execution of Kirvine 
                Stack machine
  
              : Module Like interface provided for Env, Closures, Machines etc.
                Which helps in code modularity as well as testing of implementation *)


(*===================================================================================*)

(* krv_exp : Denotes the Abstract Syntactic Category of the expression supported by the toy language
            Largely Similar to what has been discussed throughout the first half of semester
         
           : Since Krivine Machine is used for evaluation of only pure lambda 
             calculus, I created two separates exp : krv_exp and secd_exp to handle
             the implementations *) 


type krv_exp = KVar of string | KLambda of string * krv_exp | KApp of krv_exp * krv_exp   

(* Modelling theoretical Closure's Using a cl_expr : expr in our toy language and cl_table 
  : table_node representing the table environment at the time of packing of closure

  The Kirvine Stack Machine operates on closures and closure stacks so 
  this key detail is important *)

type krv_closure = {
  cl_expr : krv_exp;
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
  mutable parent : krv_table_node option ref;            (* Linked List like using option (Like Option Enum of Rust) *)
  mutable children : krv_table_node list ref;            (* Stack of scopes inside current scope *)
}



exception Invalid_Exp of string


(* Notice the use of and - Used for defition of mutually defined syntactic categories 
  Which for our case is closure and tables (atleast for kirivine machine)*)

(*===================================================================================*)
(*===================================================================================*) 

(** 
  Krv_Env - Environment Management for the Krivine Stack Machine

  This module defines and manages the environment structure used in the Krivine Stack Machine.
  The environment is modeled as a linked list of tables (lexical scopes), where each table maps 
  variable names to closures. The environment persists throughout the machine's transitions, 
  enabling proper lexical scoping and variable lookups.

  Key Features:
  - **Environment Creation**: Supports dynamic creation of new tables when entering new lexical scopes.
  - **Parent-Child Relationships**: Maintains parent-child links between environments to enable variable 
    lookups across nested scopes.
  - **Variable Bindings**: Provides functionality to bind variables to closures within the current scope.
  - **Environment Traversal**: Allows recursive traversal of parent environments for variable resolution.
  - **Debugging Utilities**: Includes functions to print the structure and contents of an environment.

  This abstraction ensures efficient and accurate modeling of lexical scoping, a core requirement for 
  implementing call-by-name semantics in the Krivine Stack Machine. *)


module Krv_Env = struct

  exception Var_Not_Found of string
  let hash_table_size_init = 8

  (** Creates new environment node for lexical scoping 
    - Initializes empty hashtable for variable bindings
    - Sets parent link for environment chain traversal *)

  let create ?(parent : krv_table_node option = None) () : krv_table_node = {
    table = Hashtbl.create hash_table_size_init;
    parent = ref parent;
    children = ref [];
  } 

  (** Establishes parent-child relationship between environments
    - Updates parent's children list with new child reference
    - Sets child's parent pointer for lexical scope chaining *)

  let add_child (parent : krv_table_node) (child : krv_table_node) = 
    parent.children := child :: !(parent.children);
    child.parent := Some parent
  
  (** Binds variable to closure in current environment
    - Maps variable name to closure in environment hashtable
    - Implements variable shadowing of parent bindings *)

  let add_binding (env_table : krv_table_node) (var : string) (clos : krv_closure) = 
    Hashtbl.replace env_table.table var clos
  
  
  (** Looks up a variable in the environment chain
    - Starts from the current environment and recursively checks parent environments
    - Returns the closure bound to the variable if found
    - Raises [Var_Not_Found] exception if the variable is not bound in any environment *)

  let rec lookup (env_table : krv_table_node) (var : string) : krv_closure =  
    match Hashtbl.find_opt env_table.table var with
    | Some closure -> closure  (* Variable found in current environment *)
    | None -> 
        match !(env_table.parent) with
        | Some parent -> lookup parent var  (* Recursively check parent environment *)
        | None -> raise (Var_Not_Found ("Variable not found in the table :" ^ var))  (* Variable not found in any environment *)

  (** Debug utility to print environment chain from a reference
    - Dereferences environment pointer before traversal
    - Recursively displays all variables in parent chain *)
  let print_env (env : krv_table_node ref) =
    let rec aux current_env =
      Printf.printf "Environment:\n";
      Hashtbl.iter (fun k _ -> Printf.printf "  %s\n" k) current_env.table;
      match !(current_env.parent) with
      | Some p -> aux p
      | None -> ()
    in
    aux !env

end

(** 
  Krv_Closure - Closure Management for the Krivine Stack Machine

  This module defines and manages closures, which are the core computational units 
  of the Krivine Stack Machine. A closure is a combination of an expression and its 
  captured environment, enabling proper lexical scoping during lazy evaluation.

  Key Features:
  - **Closure Creation**: Captures an expression and its environment into a closure.
  - **Closure Application**: Implements β-reduction by binding arguments to formal 
    parameters in a new environment and focusing on the body of the lambda abstraction.
  - **Debugging Utilities**: Provides functions to inspect closures and their environments.

  Role in Krivine Machine:
  - The Krivine machine operates on closures and closure stacks to evaluate pure 
    λ-calculus expressions lazily (call-by-name). Closures ensure that variables are 
    resolved in the correct environment, even when deferred evaluation occurs.
  
  Example Transition Rule (KrApp):
    ⟨⟨λx.e', γ⟩, cl :: s⟩ ==> ⟨⟨e', γ[x ↦ cl]⟩, s⟩
    - Pop the argument closure `cl` from the stack.
    - Bind it to the formal parameter `x` in a new environment derived from `γ`.
    - Focus on the body `e'` of the lambda abstraction with the augmented table. *)

module Krv_Closure = struct 

  (** Create closure capturing current environment state 
      - expr: Lambda expression being closed over
      - env_table: Reference to active environment node
      - Returns closure with frozen environment reference 
        which will be later used for its unpacking if required
        such as in abstraction application *)

  let create (expr : krv_exp) (env_table : krv_table_node ref) : krv_closure = 
    {cl_expr = expr; cl_table = env_table}

  (* Apply closure to argument (β-reduction step)
      - closure: Closure containing lambda abstraction
      - arg : Unevaluated argument closure
      - Returns new closure for body expression *)
    
  let apply (closure : krv_closure) (arg : krv_closure) : krv_closure =
  match closure.cl_expr with
  | KLambda(param, body) -> (
    
      let new_env = Krv_Env.create ~parent:(Some !(closure.cl_table)) () in
      
      Krv_Env.add_binding new_env param arg;
      
      (* Create closure for body in extended environment *)
      { cl_expr = body; cl_table = ref new_env }
    )     
  | _ -> raise (Invalid_Exp "Cannot apply non-lambda closure")
  
  (* Debugging Method - to_string
     Recursively unpack closure to string representation 
      - depth: Current nesting level for indentation
      - Returns string showing expression structure 
      - Added Advantage is that 2D visualisation makes it clear about who is
        in the body of what *)

  let rec to_string ?(depth = 0) (krv_cl : krv_closure) : string =
    let indent = String.make (depth * 2) ' ' in
    match krv_cl.cl_expr with
    | KVar x -> 
        Printf.sprintf "%s%s" indent x
    | KLambda(param, body) ->
        let body_cl = { krv_cl with cl_expr = body } in
        Printf.sprintf "%sλ%s.\n%s" indent param (to_string ~depth:(depth+1) body_cl)
    | KApp(e1, e2) ->
        let e1_cl = { krv_cl with cl_expr = e1 } in
        let e2_cl = { krv_cl with cl_expr = e2 } in
        Printf.sprintf "%s(\n%s\n%s\n%s)" 
          indent 
          (to_string ~depth:(depth+1) e1_cl)
          (to_string ~depth:(depth+1) e2_cl)
          indent

  (* Debug utility to print the environemnt table
     That is associated in a Krv_Closure *)
  let print_env (cl : krv_closure) = Krv_Env.print_env cl.cl_table

end

(** 
  Krv_Machine - Implementation of the Krivine Abstract Machine
  
  This module implements the Krivine Abstract Machine (KAM) for call-by-name 
  evaluation of the pure λ-calculus. The machine operates on closures using a 
  state transition system that follows the operational semantics:
  
  (KrApp) ⟨⟨(e₁ e₂), γ⟩⟩, s ==> ⟨⟨e₁, γ⟩⟩, ⟨⟨e₂, γ⟩⟩::s
  (KrVar) ⟨⟨x, γ⟩⟩, s ==> γ(x), s
  (KrAbs) ⟨⟨λx.e′, γ⟩⟩, cl::s ==> ⟨⟨e′, γ[x ↦ cl]⟩⟩, s *)

module Krv_Machine = struct

  (** Exception raised when the machine reaches a non-terminal stuck state *)
  exception Stuck of string * krv_exp * krv_closure list
  
  (** Exception raised when a variable lookup fails in the environment *)
  exception Var_Not_Found of string

  (** Machine state consisting of a focus closure and a closure stack
      - param : current : The closure currently being evaluated
      - param : stack : Unevaluated argument closures *)

  type state = {
    current : krv_closure;
    stack : krv_closure list;
  }

  (** Creates a new machine state
      - param : current_clos : The closure in focus
      - param : stack_closures : Stack of argument closures
      - return : A new machine state *)

  let create (current_clos : krv_closure) (stack_closures : krv_closure list) : state = 
    {current = current_clos; stack = stack_closures}

  (** Initializes the machine with a pure lambda expression
      - param : expr : Initial lambda expression
      - return : Initial machine state with empty environment and stack *)

  let init (expr : krv_exp) : state = 
    let empty_env = Krv_Env.create () in
    { current = Krv_Closure.create expr (ref empty_env);
      stack = [] }

  (** Performs one transition step according to KAM rules
      - param : st : Current machine state
      - return : Next machine state after applying transition rule
    
      The transition rules implemented are:
      - (KrApp): Push argument closure onto stack, focus on function
      - (KrVar): Replace variable with its bound closure from environment
      - (KrAbs): Apply lambda by binding argument to parameter, focus on body *)

  let step (st : state) : state = 
    match st.current.cl_expr , st.stack with 
    | KApp(e1, e2), sstack -> (
      let new_current = Krv_Closure.create e1 st.current.cl_table in
      let new_stack_top = Krv_Closure.create e2 st.current.cl_table in
      let new_stack = new_stack_top :: sstack in
      let new_state = create new_current new_stack in
      new_state
    )
    | KVar(x), sstack -> (
      let env = !(st.current.cl_table) in
      try 
        let new_current = Krv_Env.lookup env x in
          create new_current sstack 
        with
          Krv_Env.Var_Not_Found _ -> raise (Var_Not_Found("Unbound Variable: "^x))
    )
    | KLambda(param,body), (arg :: rest) -> (
      let new_env = Krv_Env.create ~parent:(Some !(st.current.cl_table)) () in
      Krv_Env.add_binding new_env param arg;
      let new_current = Krv_Closure.create body (ref new_env) in
      create new_current rest
    )
    | _,[] -> st

  (** Evaluates an expression to normal form using the Krivine machine
      - param : initial_state : Starting machine state
      - return : Final closure representing the evaluation result
    
      This function applies the step function repeatedly until reaching a
      fixed point (no further reduction possible), implementing a tail-recursive
      evaluation loop for the call-by-name strategy. *)

  let execute (initial_state : state) : krv_closure =
    let rec exec_loop current_state =
      let next_state = step current_state in
      if next_state = current_state then 
        current_state.current  (* Fixed point reached *)
      else 
        exec_loop next_state
    in
    exec_loop initial_state

end
