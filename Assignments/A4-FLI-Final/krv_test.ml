(* main_test.ml - Test cases for Krivine Machine *)

open Machine

(*===================================================================================*)
(*===================================================================================*)

(* Helper function to print results *)
let string_of_prim_ans = function
  | N(n) -> string_of_int n
  | B(b) -> string_of_bool b

let string_of_answer = function
  | Prim(p) -> string_of_prim_ans p
  | Clos(_) -> "<closure>"

let extract_answer cl =
  match cl with
  | KRV_Clos krv_cl -> (
      match krv_cl.expr with
      | Num(n) -> Prim(N(n))
      | Bool(b) -> Prim(B(b))
      | _ -> Clos(ref cl)
    )
  | _ -> Clos(ref cl)

let run_test name expr expected =
  print_endline ("Test: " ^ name);
  try
    let cl = create_krv_clos expr (ref (create_new_gamma ())) in
    let result = krv_machine cl [] in
    let answer = extract_answer result in
    print_string "  Result: ";
    print_endline (string_of_answer answer);
    print_string "  Expected: ";
    print_endline (string_of_answer expected);
    print_endline (if answer = expected then "  ✓ PASS" else "  ✗ FAIL");
    print_endline ""
  with
  | e -> 
      print_string "  Error: ";
      print_endline (Printexc.to_string e);
      print_endline "  ✗ FAIL";
      print_endline ""

(* Test cases *)

(* Test 1: Simple numeric value *)
let test1 = Num(42)
let expected1 = Prim(N(42))

(* Test 2: Simple boolean value *)
let test2 = Bool(true)
let expected2 = Prim(B(true))

(* Test 3: Simple arithmetic - Plus *)
let test3 = Plus(Num(3), Num(4))
let expected3 = Prim(N(7))

(* Test 4: Simple arithmetic - Times *)
let test4 = Times(Num(5), Num(6))
let expected4 = Prim(N(30))

(* Test 5: Nested arithmetic *)
let test5 = Plus(Times(Num(2), Num(3)), Num(4))
let expected5 = Prim(N(10))

(* Test 6: Simple boolean operation - And *)
let test6 = And(Bool(true), Bool(false))
let expected6 = Prim(B(false))

(* Test 7: Simple boolean operation - Or *)
let test7 = Or(Bool(false), Bool(true))
let expected7 = Prim(B(true))

(* Test 8: Simple lambda application *)
let test8 = App(Lam("x", V("x")), Num(5))
let expected8 = Prim(N(5))

(* Test 9: Lambda with arithmetic *)
let test9 = App(Lam("x", Plus(V("x"), Num(1))), Num(5))
let expected9 = Prim(N(6))

(* Test 10: Nested lambda application *)
let test10 = App(App(Lam("x", Lam("y", Plus(V("x"), V("y")))), Num(5)), Num(3))
let expected10 = Prim(N(8))

(* Test 11: Conditional expression *)
let test11 = IFTE(Bool(true), Num(1), Num(2))
let expected11 = Prim(N(1))

(* Test 12: Conditional with comparison *)
let test12 = IFTE(Gt(Num(5), Num(3)), Num(1), Num(2))
let expected12 = Prim(N(1))

(* Test 13: Complex expression with multiple operations *)
let test13 = App(
  Lam("x", 
    IFTE(
      Gt(V("x"), Num(10)),
      Times(V("x"), Num(2)),
      Plus(V("x"), Num(5))
    )
  ),
  Num(7)
)
let expected13 = Prim(N(12))

(* Test 14: Higher-order function *)
let test14 = App(
  App(
    Lam("f", Lam("x", App(V("f"), V("x")))),
    Lam("y", Times(V("y"), Num(2)))
  ),
  Num(3)
)
let expected14 = Prim(N(6))

(* Test 15: Mixed operations *)
let test15 = App(
  Lam("x", Plus(V("x"), Times(Num(2), Num(3)))),
  Num(5)
)
let expected15 = Prim(N(11))

(* Test 16: Identity function *)
let test16 = Lam("x", V("x"))
let expected16 = Clos(ref (KRV_Clos {expr = V("x"); table = ref (create_new_gamma ())}))

(* Test 17: Application of identity function *)
let test17 = App(Lam("x", V("x")), V("y"))
let expected17 = Clos(ref (KRV_Clos {expr = V("y"); table = ref (create_new_gamma ())}))

(* Test 18: Church encoding of true *)
let test18 = Lam("x", Lam("y", V("x")))
let expected18 = Clos(ref (KRV_Clos {expr = Lam("y", V("x")); table = ref (create_new_gamma ())}))
(* Test 19: Church encoding of false *)
let test19 = Lam("x", Lam("y", V("y")))
let expected19 = Clos(ref (KRV_Clos {expr = Lam("y", V("y")); table = ref (create_new_gamma ())}))

(* Test 20: Application of Church true to two arguments *)
let test20 = App(App(test18, V("a")), V("b"))
let expected20 = Clos(ref (KRV_Clos {expr = V("a"); table = ref (create_new_gamma ())}))

(* Test 21: Application of Church false to two arguments *)
let test21 = App(App(test19, V("a")), V("b"))
let expected21 = Clos(ref (KRV_Clos {expr = V("b"); table = ref (create_new_gamma ())}))

(* Test 22: Church encoding of successor function *)
let test22 = Lam("n", Lam("f", Lam("x", App(V("f"), App(App(V("n"), V("f")), V("x"))))))
let expected22 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", App(V("f"), App(App(V("n"), V("f")), V("x"))))); table = ref (create_new_gamma ())}))

(* Test 23: Application of successor to a Church numeral *)
let church_one = Lam("f", Lam("x", App(V("f"), V("x"))))
let test23 = App(test22, church_one)
let expected23 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", App(V("f"), App(App(church_one, V("f")), V("x"))))); table = ref (create_new_gamma ())}))

let test_free_var = Lam("x", Plus(V("x"), Times(Num(2), V("y"))))
let expected_free_var = Clos(ref (KRV_Clos {expr = Lam("x", Plus(V("x"), Times(Num(2), V("y")))); table = ref (create_new_gamma ())}))

let test_lambda_plus = App(Lam("x", Plus(V("x"), Num(2))), V("y"))

let expected_lambda_plus = Clos(ref (KRV_Clos {expr = Plus(V("y"), Num(2)); table = ref (create_new_gamma ())}))
(* Run all tests *)
let () =
  run_test "Simple numeric value" test1 expected1;
  run_test "Simple boolean value" test2 expected2;
  run_test "Simple arithmetic - Plus" test3 expected3;
  run_test "Simple arithmetic - Times" test4 expected4;
  run_test "Nested arithmetic" test5 expected5;
  run_test "Simple boolean operation - And" test6 expected6;
  run_test "Simple boolean operation - Or" test7 expected7;
  run_test "Simple lambda application" test8 expected8;
  run_test "Lambda with arithmetic" test9 expected9;
  run_test "Nested lambda application" test10 expected10;
  run_test "Conditional expression" test11 expected11;
  run_test "Conditional with comparison" test12 expected12;
  run_test "Complex expression with multiple operations" test13 expected13;
  run_test "Higher-order function" test14 expected14;
  run_test "Mixed operations" test15 expected15;
  run_test "Identity Function" test16 expected16;
  run_test "Application of Identity Funcion" test17 expected17;
  run_test "Church Encoding of True" test18 expected18;
  run_test "Church Encoding of False" test19 expected19;
  run_test "Application of Church true to two arguments" test20 expected20;
  run_test "Application of Church false to two arguments" test21 expected21;
  run_test "Church encoding of successor function" test22 expected22;
  run_test "Application of successor to a Church numeral" test23 expected23;
  run_test "Lambda with free variable" test_free_var expected_free_var;
  run_test "Lambda Plus" test_lambda_plus expected_lambda_plus;


(*===================================================================================*)
(*===================================================================================*)

