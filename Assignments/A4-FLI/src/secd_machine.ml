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
  env : secd_env ref  (* Reference for environment sharing *)
}

(** Value representation *)
and values = 
  | Prim of prim_val
  | Clos of closure
and prim_val = N of int | B of bool

(** Environment structure *)
and secd_env = {
  mutable bindings : (string, values) Hashtbl.t;
  parent : secd_env option ref;  (* Lexical parent *)
}

(** Stack type for values storage *)
type secd_stack = values list

(** SEC triple for efficient state saving *)
type secd_triple = {
  st : secd_stack ref;   (* Reference to stack *)
  env : secd_env ref;    (* Reference to environment *)
  cd : secd_code ref;    (* Reference to code *)
}

(** SECD state with efficient triple-based dump *)
type secd_state = {
  stack : secd_stack;       (* S - Current stack (for direct access) *)
  env : secd_env ref;       (* E - Current environment reference *)
  code : secd_code;         (* C - Current code segment (for direct access) *)
  dump : secd_triple list;  (* D - Saved states as triples with references *)
}


(*===================================================================================*)
                  (* Modular Implementaion of SECD Stack Machine *)
(*===================================================================================*)

(** SECD_Env - Environment Management for the SECD Machine

    This module defines and manages the environment structure used in the SECD Machine.
    The environment is represented as a hierarchical mapping from variables to values,
    implementing lexical scoping through parent references. Each environment preserves 
    its creation-time bindings while enabling efficient variable lookups across nested scopes.
    
    Key Features:
    - **Environment Creation**: Dynamically creates new environments with parent links.
    - **Variable Binding**: Maps variables to fully evaluated values or closures.
    - **Lexical Scoping**: Resolves variables by traversing parent references up the chain.
    - **Debugging Utilities**: Provides functions to inspect and print environments. *)
module SECD_Env = struct

  (** Exception raised when a variable is not found in any environment *)
  exception Var_Not_Found of string

  (** Initial size of hash tables for environment bindings *)
  let hash_table_size_init = 4

  (** 
    Creates a new environment node for lexical scoping 
    @param parent Optional parent environment (default: None)
    @return A new environment node with empty bindings *)
  
  let create ?(parent=None) () : secd_env = {
    bindings = Hashtbl.create hash_table_size_init;
    parent = ref parent;
  }

  (** 
    Binds a variable to a value in the current environment
    @param env_table The current environment
    @param var The variable name
    @param val_obj The value to bind (can be primitive or closure)
    - Implements variable shadowing if the variable is already bound in this scope *)

  let add_binding (env_table : secd_env) (var : string) (val_obj : values) = 
    Hashtbl.replace env_table.bindings var val_obj

  (** 
    Looks up a variable in the environment chain
    @param env_table The starting environment
    @param var The variable name to look up
    @return The value bound to the variable if found
    @raise Var_Not_Found if the variable is not found in any scope
    - Traverses parent environments recursively for lexical scoping *)

  let rec lookup (env_table : secd_env) (var : string) : values =  
    match Hashtbl.find_opt env_table.bindings var with
    | Some value -> value (* Variable found in current environment *)
    | None -> 
        match !(env_table.parent) with
        | Some parent -> lookup parent var (* Recursively check parent environment *)
        | None -> raise (Var_Not_Found ("Variable not found: " ^ var))

 (** Debug utility to print environment chain from a reference
    - Dereferences environment pointer before traversal
    - Recursively displays all variables and their bindings along the chain *)

  let print_env (env : secd_env ref) =
    let rec aux current_env =
      Printf.printf "SECD Environment:\n";
      Hashtbl.iter (fun k v -> 
        match v with
        | Prim(prim_val) -> (
            match prim_val with
            | N(n) -> Printf.printf "  Variable: %s => Integer: %d\n" k n
            | B(b) -> Printf.printf "  Variable: %s => Boolean: %b\n" k b
          )
        | Clos(clos) -> 
            Printf.printf "  Variable: %s => <Closure>\n" k
      ) current_env.bindings;
      match !(current_env.parent) with
      | Some p -> 
          Printf.printf "Parent Environment:\n";
          aux p
      | None -> 
          Printf.printf "No parent environment\n"
    in
    aux !env


  end

(** SECD_Stack - Stack Management for the SECD Abstract Machine
  
    This module manages the Stack (S) component of the SECD machine, which stores
    intermediate values during evaluation. The stack contains fully evaluated values
    (primitives or closures) that represent the results of subexpressions that have
    already been computed according to call-by-value semantics.
  
    Key operations include pushing values onto the stack, popping values for use in
    operations, and inspecting the stack state during execution. *)
module SECD_Stack = struct

  (** Exception raised when attempting to pop from an empty stack *)
  exception Empty_Stack

  (** Creates a new empty stack *)
  let create () : secd_stack = []

  (** Pushes a value onto the stack *)
  let push (stack : secd_stack) (value : values) : secd_stack =
    value :: stack

  (** Pops a value from the top of the stack
      @param stack The current stack
      @return A tuple containing the popped value and the updated stack
      @raise Empty_Stack if the stack is empty *)
  let pop (stack : secd_stack) : values * secd_stack =
    match stack with
    | [] -> raise Empty_Stack
    | top :: rest -> (top, rest)

  (** Peeks at the top value of the stack without removing it
      @param stack The current stack
      @return The top value of the stack
      @raise Empty_Stack if the stack is empty *)
  let peek (stack : secd_stack) : values =
    match stack with
    | [] -> raise Empty_Stack
    | top :: _ -> top

  (** Checks if the stack is empty
      @param stack The current stack
      @return [true] if the stack is empty, [false] otherwise *)
  let is_empty (stack : secd_stack) : bool =
    match stack with
    | [] -> true
    | _ -> false

  (** Debug utility to print all elements in the stack
      @param stack The current stack *)
  let print_stack (stack : secd_stack) =
    Printf.printf "SECD Stack:\n";
    List.iteri (fun i v ->
      match v with
      | Prim(N n) -> Printf.printf "  [%d]: Integer: %d\n" i n
      | Prim(B b) -> Printf.printf "  [%d]: Boolean: %b\n" i b
      | Clos(clos) -> Printf.printf "  [%d]: <Closure>\n" i
    ) stack

end

(** SECD_Code - Code Generation and Management for the SECD Abstract Machine
  
    This module handles the Code (C) component of the SECD machine, responsible for
    compiling abstract syntax trees into sequences of opcodes and managing code
    execution. The code represents the remaining instructions to be executed in the
    current context.
    
    The compilation process implements a post-order traversal of expression trees,
    generating appropriate opcodes for each language construct according to
    call-by-value evaluation strategy. *)
module SECD_Code = struct

  (** Exception raised for unsupported expressions during compilation *)
  exception Unsupported_Expression of string

  (** Converts an abstract syntax tree into a sequence of opcodes
      @param expr The expression to compile
      @return A list of opcodes representing the compiled code *)
  let rec compile (expr : exp) : secd_code =
    match expr with
    | Num n -> [LDN n]  (* Load numeric constant *)
    | Bool b -> [LDB b]  (* Load boolean constant *)
    | Var x -> [LOOKUP x]  (* Variable lookup *)
    | Lambda(param, body) -> [MKCLOS(param, compile body @ [RET])]  (* Create closure *)
    | App(e1, e2) -> compile e1 @ compile e2 @ [APP]  (* Function application *)
    | Plus(e1, e2) -> compile e1 @ compile e2 @ [ADD]  (* Addition *)
    | Times(e1, e2) -> compile e1 @ compile e2 @ [MUL]  (* Multiplication *)
    | Not e -> compile e @ [NOT]  (* Logical NOT *)
    | And(e1, e2) -> compile e1 @ compile e2 @ [AND]  (* Logical AND *)
    | Or(e1, e2) -> compile e1 @ compile e2 @ [OR]  (* Logical OR *)
    | Eq(e1, e2) -> compile e1 @ compile e2 @ [EQ]  (* Equality comparison *)
    | Gt(e1, e2) -> compile e1 @ compile e2 @ [GT]  (* Greater-than comparison *)
    | _ -> raise (Unsupported_Expression "Expression cannot be compiled") (* Not needed if all cases mentioned !*)

  (** Concatenates two opcode lists
      @param code1 First opcode list
      @param code2 Second opcode list
      @return Combined opcode list *)
  let concat_code (code1 : secd_code) (code2 : secd_code) : secd_code =
    code1 @ code2

  (** Prints the opcode list in a human-readable format
      @param code The opcode list to print *)
  let print_code (code : secd_code) =
    Printf.printf "SECD Code:\n";
    List.iter (fun op ->
      match op with
      | LDN n -> Printf.printf "  LDN %d\n" n
      | LDB b -> Printf.printf "  LDB %b\n" b
      | LOOKUP x -> Printf.printf "  LOOKUP %s\n" x
      | MKCLOS(param, body) -> Printf.printf "  MKCLOS %s <body>\n" param
      | APP -> Printf.printf "  APP\n"
      | RET -> Printf.printf "  RET\n"
      | ADD -> Printf.printf "  ADD\n"
      | MUL -> Printf.printf "  MUL\n"
      | NOT -> Printf.printf "  NOT\n"
      | AND -> Printf.printf "  AND\n"
      | OR -> Printf.printf "  OR\n"
      | EQ -> Printf.printf "  EQ\n"
      | GT -> Printf.printf "  GT\n"
    ) code

end

(** SECD_Dump - Continuation Management for the SECD Abstract Machine
  
    This module implements the Dump (D) component of the SECD machine, which stores
    suspended computation contexts during function calls. Each dump entry contains
    references to a saved (S,E,C) triple that represents the state to which control
    should return after a function call completes.
    
    As noted in the formal specification, the dump uses references to environment
    structures rather than copying them, ensuring memory efficiency while maintaining
    proper lexical scoping across function boundaries. *)

module SECD_Dump = struct

  (** Exception raised when attempting to pop from an empty dump *)
  exception Empty_Dump

  (** Creates a new (S, E, C) triple
      @param stack_ref Reference to the current stack (S)
      @param env_ref Reference to the current environment (E)
      @param code_ref Reference to the current code (C)
      @return A new (S, E, C) triple *)
  let create_triple (stack_ref : secd_stack ref) (env_ref : secd_env ref) (code_ref : secd_code ref) : secd_triple = 
    { st = stack_ref; env = env_ref; cd = code_ref }

  (** Pushes a new triple onto the dump
      @param dump The current dump stack
      @param triple The triple to push
      @return The updated dump stack *)
  let push (dump : secd_triple list) (triple : secd_triple) : secd_triple list =
    triple :: dump

  (** Pops a triple from the top of the dump
      @param dump The current dump stack
      @return A tuple containing the popped triple and the updated dump stack
      @raise Empty_Dump if the dump is empty *)
  let pop (dump : secd_triple list) : secd_triple * secd_triple list =
    match dump with
    | [] -> raise Empty_Dump
    | top :: rest -> (top, rest)

  (** Peeks at the top triple of the dump without removing it
      @param dump The current dump stack
      @return The top triple of the dump
      @raise Empty_Dump if the dump is empty *)
  let peek (dump : secd_triple list) : secd_triple =
    match dump with
    | [] -> raise Empty_Dump
    | top :: _ -> top

  (** Checks if the dump is empty
      @param dump The current dump stack
      @return [true] if the dump is empty, [false] otherwise *)
  let is_empty (dump : secd_triple list) : bool =
    match dump with
    | [] -> true
    | _ -> false

  (** Debugging utility to print all entries in the dump stack
      @param dump The current dump stack *)
  let print_dump (dump : secd_triple list) =
    Printf.printf "SECD Dump:\n";
    List.iteri (fun i triple ->
      Printf.printf "  [%d]:\n" i;
      Printf.printf "    Stack: %s\n" 
        (String.concat ", " 
           (List.map (function 
              | Prim(N n) -> Printf.sprintf "N(%d)" n 
              | Prim(B b) -> Printf.sprintf "B(%b)" b 
              | Clos _ -> "<Closure>") !(triple.st)));
      Printf.printf "    Environment: <env>\n"; (* Simplified for brevity *)
      Printf.printf "    Code: %s\n" 
        (String.concat ", " 
           (List.map (function 
              | LDN n -> Printf.sprintf "LDN(%d)" n 
              | LDB b -> Printf.sprintf "LDB(%b)" b 
              | LOOKUP x -> Printf.sprintf "LOOKUP(%s)" x 
              | MKCLOS _ -> "MKCLOS"
              | APP -> "APP"
              | RET -> "RET"
              | ADD -> "ADD"
              | MUL -> "MUL"
              | NOT -> "NOT"
              | AND -> "AND"
              | OR -> "OR"
              | EQ -> "EQ"
              | GT -> "GT") !(triple.cd)))
    ) dump

end

(** 
  SECD_Machine - Core implementation of the SECD Abstract Machine
  
  This module implements the SECD (Stack, Environment, Code, Dump) machine
  for call-by-value evaluation of lambda calculus with arithmetic operations.
  The machine operates through state transitions based on opcodes, following
  the formal semantics described in the lecture notes.
*)
module SECD_Machine = struct
  
  (** Exception raised when the machine reaches a non-terminal stuck state *)
  exception Stuck of string
  
  (** Exception raised when a variable lookup fails in the environment *)
  exception Var_Not_Found of string
  
  (** Creates a new machine state with the given components *)
  let create_state (stack :secd_stack) (env : secd_env ref) (code : secd_code) (dump : secd_triple list) : secd_state =
    { stack; env; code; dump }
    
  (** Initializes the machine with a compiled expression
      - Compiles the provided expression into opcodes
      - Uses Default (empty) initializations of env as well as dump 
      @return A new secd_state comprising of above specifications *)

  let init expr =
    let compiled_code = SECD_Code.compile expr in
    let empty_env = ref (SECD_Env.create ()) in
    { stack = []; env = empty_env; code = compiled_code; dump = [] }
  
  (** Performs one transition step according to SECD rules 
      As discussed in theory *)
  let step state =
    match state.code with
    | [] -> 
          (* No more op-codes left to execute *)
        if state.dump = [] then
          (* Terminal state - Execution completed , Stack Machine terminated successfully *)
          state
        else
          (* In a correctly typed expr , any abstraction call should include RET opcode at the end
             And hence return to the context present at the time of call, failing to do so would result 
             in a non empty dump at the end *)
          raise (Stuck "Code Exhausted but Dump not empty")
          
    | op :: rest_code ->
        match op with
        | LDN n ->
            (* Load numeric constant onto stack *)
            { state with 
              stack = Prim(N n) :: state.stack;
              code = rest_code }
              
        | LDB b ->
            (* Load boolean constant onto stack *)
            { state with 
              stack = Prim(B b) :: state.stack;
              code = rest_code }
              
        | LOOKUP x ->
            (* Look up variable in environment *)
            (try
              let value = SECD_Env.lookup !(state.env) x in
              { state with 
                stack = value :: state.stack;
                code = rest_code }
            with
              SECD_Env.Var_Not_Found _ -> 
                raise (Var_Not_Found ("Unbound variable: Not found in the current Environment : " ^ x)))
                
        | MKCLOS(param, body_code) ->
            (* Create closure and push onto stack *)
            let closure = Clos { 
              param; 
              code = body_code; 
              env = ref !(state.env) 
            } in
            { state with 
              stack = closure :: state.stack;
              code = rest_code }
              
        | APP ->
            (* Function application *)
            (match state.stack with
            | arg :: Clos({ param; code = body_code; env = clos_env }) :: rest_stack ->
                (* Create new environment with argument binding - Augementing the old env *)
                let new_env = SECD_Env.create ~parent:(Some !(clos_env)) () in
                SECD_Env.add_binding new_env param arg;
                
                (* Making a new triple for saving current state in dump *)
                let triple = {
                  st = ref rest_stack;
                  env = state.env;
                  cd = ref rest_code
                } in
                
                (* New state with empty stack, new environment, and closure body code *)
                { stack = [];
                  env = ref new_env;
                  code = body_code;
                  dump = triple :: state.dump }
                  
            | _ -> raise (Stuck "Invalid stack for APP opcode : stack should have form : a::⟨⟨⟨x,c′,γ'⟩⟩⟩::S,
                                \n Where a represents argument bound to parameter x"))
            
        | RET ->
            (* Return from function call *)
            (match state.stack, state.dump with
            | [result], triple :: rest_dump ->
                (* Restoring SEC triple from dump and push result onto stack *)
                { stack = result :: !(triple.st);
                  env = triple.env;
                  code = !(triple.cd);
                  dump = rest_dump }
                  
            | _, [] -> raise (Stuck "RET with empty dump : No Context found to return to after abstraction call")
            | _, _ -> raise (Stuck "Invalid stack for RET opcode : Function Call's should end with only one answer
                                    on current context's stack"))
            
        | ADD ->
            (* Addition operation *)
            (match state.stack with
            | Prim(N n2) :: Prim(N n1) :: rest_stack ->
                { state with
                  stack = Prim(N (n1 + n2)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for ADD opcode : Expected atleast two arguments on stack to be of primitive 
                                \n value type with integer associated values"))
            
        | MUL ->
            (* Multiplication operation *)
            (match state.stack with
            | Prim(N n2) :: Prim(N n1) :: rest_stack ->
                { state with
                  stack = Prim(N (n1 * n2)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for MUL opcode : Expected atleast two arguments on stack to be of primitive 
                                \n value type with integer associated values"))
            
        | NOT ->
            (* Logical NOT operation *)
            (match state.stack with
            | Prim(B b) :: rest_stack ->
                { state with
                  stack = Prim(B (not b)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for NOT opcode : Expected atleast one argument on stack to be of primitive 
                                \n value type with boolean associated value"))
            
        | AND ->
            (* Logical AND operation *)
            (match state.stack with
            | Prim(B b2) :: Prim(B b1) :: rest_stack ->
                { state with
                  stack = Prim(B (b1 && b2)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for AND opcode : Expected atleast two arguments on stack to be of primitive 
                                \n value type with boolean associated values"))
            
        | OR ->
            (* Logical OR operation *)
            (match state.stack with
            | Prim(B b2) :: Prim(B b1) :: rest_stack ->
                { state with
                  stack = Prim(B (b1 || b2)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for OR opcode : Expected atleast two arguments on stack to be of primitive 
                                \n value type with boolean associated values"))
            
        | EQ ->
            (* Equality comparison - Differs from the classic only integer comparison, possible on any primitve type *)
            (match state.stack with
            | Prim(N n2) :: Prim(N n1) :: rest_stack ->
                { state with
                  stack = Prim(B (n1 = n2)) :: rest_stack;
                  code = rest_code }
            | Prim(B b2) :: Prim(B b1) :: rest_stack ->
                { state with
                  stack = Prim(B (b1 = b2)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for EQ opcode : Expected atleast two arguments on stack to be of primitive 
                                \n value type"))
            
        | GT ->
            (* Greater-than comparison *)
            (match state.stack with
            | Prim(N n2) :: Prim(N n1) :: rest_stack ->
                { state with
                  stack = Prim(B (n1 > n2)) :: rest_stack;
                  code = rest_code }
            | _ -> raise (Stuck "Invalid stack for GT opcode : Expected atleast two arguments on stack to be of primitive 
                                \n value type with integer associated values"))
  
  (** Executes the machine until termination
      @param initial_state Starting machine state
      @return Final state with result on top of stack
      @raise Stuck if machine gets stuck in a non-terminal state *)
  let execute initial_state =
    let rec exec_loop current_state =
      try
        let next_state = step current_state in
        if next_state = current_state then
          (* Fixed point reached - machine halted *)
          current_state
        else
          exec_loop next_state
      with
        | Stuck msg -> 
            Printf.printf "Machine stuck: %s\n" msg;
            raise (Stuck msg)
        | Var_Not_Found var ->
            Printf.printf "Variable not found: %s\n" var;
            raise (Var_Not_Found var)
    in
    exec_loop initial_state
    
  (** Evaluates an expression to a value
      @param expr Expression to evaluate
      @return Resulting value
      @raise Stuck if evaluation gets stuck *)
  let eval expr =
    let final_state = execute (init expr) in
    match final_state.stack with
    | [result] -> result
    | [] -> raise (Stuck "Evaluation completed with empty stack")
    | _ -> raise (Stuck "Evaluation completed with multiple values on stack")
    
  (** Prints the current state of the machine
      @param state Current machine state *)
  let print_state state =
    Printf.printf "SECD Machine State:\n";
    Printf.printf "Stack:\n";
    List.iter (fun v ->
      match v with
      | Prim(N n) -> Printf.printf "  Integer: %d\n" n
      | Prim(B b) -> Printf.printf "  Boolean: %b\n" b
      | Clos _ -> Printf.printf "  <Closure>\n"
    ) state.stack;
    Printf.printf "Environment: <env>\n";
    Printf.printf "Code: %d instructions\n" (List.length state.code);
    Printf.printf "Dump: %d entries\n" (List.length state.dump)
    
end