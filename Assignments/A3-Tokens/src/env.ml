(*===================================================================================*)
              (* Environment Specifications - COL226 Assignment 3 - 2023CS50334 *)  
             (* env.ml - Handles definitions related to scoping of variables *)
(*===================================================================================*)

open My_ast 

exception Var_Not_Found of string
exception Empty_Env of string

(* We will be using a stack based environment *)

type env_frame = (string * typ * value)

type environment = env_frame list

(*===================================================================================*)
                    (* Handling Variable Type and Value Lookup *)
(*===================================================================================*)

let rec lookup_var v_name env = 
  match env with 
  | [] -> raise(Var_Not_Found("Variable Not Found : " ^ v_name))
  | eframe::xenv ->
    (
      match List.find_opt (fun (v,_,_) -> v = v_name) eframe with 
      | Some (_,v_typ,v_val) -> (v_typ,v_val)
      | None -> lookup_var v_name xenv
    )

(*===================================================================================*)
                (* Updating or adding new variable to current scope *)
(*===================================================================================*)    

let define_var v_name v_typ v_val env = 
  match env with
  | [] -> raise(Empty_Env("Empty Environment : No Scope"))
  | eframe::xenv ->
    (
      let new_frame = (v_name, v_typ, v_val) :: eframe in
      new_frame :: xenv
    )

let rec update_var v_name new_v_val env =
  match env with 
  | [] -> raise(Var_Not_Found("Variable Not Found : " ^ v_name))
  | eframe::xenv ->
    (
      if List.exists (fun (v, _, _) -> v = v_name) eframe then
        let new_frame =
          List.map
            (fun (v, v_typ, v_val) -> if v = v_name then (v, v_typ, new_v_val) else (v, v_typ, v_val))
            eframe
        in
        new_frame :: xenv
      else
        eframe :: update_var v_name new_v_val env
    )

(* ------------------------- Scope Handling ------------------------- *)

(* Pushing Onto Current Scope *)
let push_scope env =
  [] :: env

(* Popping Current Scope *)
let pop_scope env =
  match env with
  | _ :: rest -> rest
  | [] -> failwith "No scope to pop"

(* ------------------------- Utility Functions ------------------------- *)

let print_env env =
  let print_frame frame =
    List.iter (fun (name, typ, value) ->
      Printf.printf "%s -> (%s, %s)\n"
        name
        (match typ with
         | T_INT -> "INT"
         | T_FLOAT -> "FLOAT"
         | T_BOOL -> "BOOL"
         | T_VEC_N -> "VECTOR_N"
         | T_VEC_F -> "VECTOR_F"
         | T_MAT_N -> "MATRIX_N"
         | T_MAT_F -> "MATRIX_F"
         | T_INP -> "INPUT"
        )
        (match value with
         | INT_V i -> string_of_int i
         | FLT_V f -> string_of_float f
         | BL_V b -> string_of_bool b
         | NVEC_V v -> "[" ^ String.concat ", " (List.map string_of_int v) ^ "]"
         | FVEC_V v -> "[" ^ String.concat ", " (List.map string_of_float v) ^ "]"
         | NMAT_V m -> "Matrix Int"
         | FMAT_V m -> "Matrix Float"
        )
    ) frame
  in
  Printf.printf "Current Environment State:\n";
  List.iter (fun frame -> print_frame frame; Printf.printf "---\n") env

(* Initialize the environment *)
let init_env () = [[]]