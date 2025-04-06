open Secd_machine 

(*===================================================================================*)
                      (* Testing the environment implementation *)
(*===================================================================================*)
let test_env_basic () =
  let env = ref (SECD_Env.create ()) in
  SECD_Env.add_binding !env "x" (Prim(N 42));
  assert (SECD_Env.lookup !env "x" = Prim(N 42));
  let _ = SECD_Env.print_env env in
  Printf.printf "Environment test passed!\n"

let test_env_nested () =
  let parent_env = ref (SECD_Env.create ()) in
  SECD_Env.add_binding !parent_env "x" (Prim(N 10));
  SECD_Env.add_binding !parent_env "y" (Prim(B true));
  
  let child_env = ref (SECD_Env.create ~parent:(Some !parent_env) ()) in
  SECD_Env.add_binding !child_env "z" (Prim(N 30));
  
  (* Test variable lookup in child environment *)
  assert (SECD_Env.lookup !child_env "z" = Prim(N 30));
  
  (* Test variable lookup in parent environment through child *)
  assert (SECD_Env.lookup !child_env "x" = Prim(N 10));
  assert (SECD_Env.lookup !child_env "y" = Prim(B true));
  
  let _ = SECD_Env.print_env child_env in
  Printf.printf "Nested environment test passed!\n"

let test_env_shadowing () =
  let parent_env = ref (SECD_Env.create ()) in
  SECD_Env.add_binding !parent_env "x" (Prim(N 100));
  
  let child_env = ref (SECD_Env.create ~parent:(Some !parent_env) ()) in
  SECD_Env.add_binding !child_env "x" (Prim(N 200));
  
  (* Test variable shadowing *)
  assert (SECD_Env.lookup !child_env "x" = Prim(N 200));
  assert (SECD_Env.lookup !parent_env "x" = Prim(N 100));
  
  let _ = SECD_Env.print_env child_env in
  Printf.printf "Environment shadowing test passed!\n"

let test_env_all() =
  test_env_basic();
  test_env_nested();
  test_env_shadowing()

(*===================================================================================*)
(*===================================================================================*)

let test_stack () =
  let s = SECD_Stack.create () in
  let s = SECD_Stack.push s (Prim(N 10)) in
  let s = SECD_Stack.push s (Prim(B true)) in
  let (v, s') = SECD_Stack.pop s in
  assert (v = Prim(B true));
  assert (SECD_Stack.peek s' = Prim(N 10));
  let _ = SECD_Stack.print_stack s in
  Printf.printf "Stack test passed!\n"  
let test_compilation () =
  let expr = Lambda("x", Plus(Var("x"), Num(1))) in
  let code = SECD_Code.compile expr in
  assert (List.length code > 0);
  let _ = SECD_Code.print_code code in
  Printf.printf "Compilation test passed!\n"

let test_evaluation expr expected_result description =
  try
    let result = SECD_Machine.eval expr in
    (match result, expected_result with
      | Prim(N n1), Prim(N n2) when n1 = n2 -> 
          Printf.printf "✓ %s: Got %d as expected\n" description n1
      | Prim(B b1), Prim(B b2) when b1 = b2 -> 
          Printf.printf "✓ %s: Got %b as expected\n" description b1
      | Clos _, Clos _ -> 
          Printf.printf "✓ %s: Got closure as expected\n" description
      | _ -> 
          Printf.printf "✗ %s: Expected %s but got different result\n" 
            description 
            (match expected_result with
            | Prim(N n) -> string_of_int n
            | Prim(B b) -> string_of_bool b
            | Clos _ -> "<closure>"))
  with
  | SECD_Machine.Stuck msg -> 
      Printf.printf "✗ %s: Machine got stuck: %s\n" description msg
  | e -> 
      Printf.printf "✗ %s: Unexpected exception: %s\n" 
        description (Printexc.to_string e)

let run_tests () =
  test_env_all();
  test_stack();
  test_compilation();

  (* Simple expressions *)
  test_evaluation (Num 42) (Prim(N 42)) "Integer constant";
  test_evaluation (Bool true) (Prim(B true)) "Boolean constant";

  (* Arithmetic *)
  test_evaluation (Plus(Num 2, Num 3)) (Prim(N 5)) "Addition";
  test_evaluation (Times(Num 4, Num 5)) (Prim(N 20)) "Multiplication";

  (* Boolean operations *)
  test_evaluation (Not(Bool false)) (Prim(B true)) "Logical NOT";
  test_evaluation (And(Bool true, Bool false)) (Prim(B false)) "Logical AND";
  test_evaluation (Or(Bool false, Bool true)) (Prim(B true)) "Logical OR";
  test_evaluation (Eq(Num 5, Num 5)) (Prim(B true)) "Equality (true)";
  test_evaluation (Eq(Num 5, Num 6)) (Prim(B false)) "Equality (false)";
  test_evaluation (Gt(Num 10, Num 5)) (Prim(B true)) "Greater than (true)";
  test_evaluation (Gt(Num 5, Num 10)) (Prim(B false)) "Greater than (false)";

  (* Function application *)
  test_evaluation 
    (App(Lambda("x", Var("x")), Num 42))
    (Prim(N 42))
    "Identity function";
    
  test_evaluation
    (App(Lambda("x", Plus(Var("x"), Num(1))), Num(41)))
    (Prim(N 42))
    "Function with body";
    
  (* Nested function calls *)
  test_evaluation
    (App(
        App(Lambda("x", Lambda("y", Plus(Var("x"), Var("y")))), Num(20)),
        Num(22)
      ))
    (Prim(N 42))
    "Nested function application";
    
  Printf.printf "\nAll tests completed!\n"

(*===================================================================================*)
(*===================================================================================*)

let () = run_tests ()