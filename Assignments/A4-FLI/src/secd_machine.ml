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

module SECD_Dump = struct

  (** Exception raised when attempting to pop from an empty dump *)
  exception Empty_Dump

  (** Creates a new (S, E, C) triple
      @param stack_ref Reference to the current stack (S)
      @param env_ref Reference to the current environment (E)
      @param code_ref Reference to the current code (C)
      @return A new (S, E, C) triple *)
  let create_triple (stack_ref : secd_stack ref) 
                    (env_ref : secd_env ref) 
                    (code_ref : secd_code ref) : secd_triple = 
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

