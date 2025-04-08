(* main_test.ml - Test cases for SECD Machine *)

open Machine

(* Helper function to print results *)
let string_of_prim_ans = function
  | N(n) -> string_of_int n
  | B(b) -> string_of_bool b

let string_of_answer = function
  | Prim(p) -> string_of_prim_ans p
  | Clos(_) -> "<closure>"

let run_secd_test name expr expected =
  print_endline ("Test: " ^ name);
  try
    let code = compile expr in
    let empty_env = create_new_gamma () in
    let result = secd_machine (create_secd_stack()) empty_env code [] in
    print_string "  Result: ";
    print_endline (string_of_answer result);
    print_string "  Expected: ";
    print_endline (string_of_answer expected);
    print_endline (if result = expected then "  ✓ PASS" else "  ✗ FAIL");
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

(* Test 5: Simple arithmetic - Sub *)
let test5 = Sub(Num(10), Num(4))
let expected5 = Prim(N(6))

(* Test 6: Simple boolean operation - And *)
let test6 = And(Bool(true), Bool(false))
let expected6 = Prim(B(false))

(* Test 7: Simple boolean operation - Or *)
let test7 = Or(Bool(false), Bool(true))
let expected7 = Prim(B(true))

(* Test 8: Simple boolean operation - Not *)
let test8 = Not(Bool(true))
let expected8 = Prim(B(false))

(* Test 9: Comparison operation - Eq *)
let test9 = Eq(Num(5), Num(5))
let expected9 = Prim(B(true))

(* Test 10: Comparison operation - Gt *)
let test10 = Gt(Num(7), Num(3))
let expected10 = Prim(B(true))

(* Test 11: Comparison operation - Lt *)
let test11 = Lt(Num(3), Num(7))
let expected11 = Prim(B(true))

(* Test 12: Nested arithmetic *)
let test12 = Plus(Times(Num(2), Num(3)), Num(4))
let expected12 = Prim(N(10))

(* Test 13: Simple lambda and application *)
let test13 = App(Lam("x", V("x")), Num(5))
let expected13 = Prim(N(5))

(* Test 14: Lambda with arithmetic *)
let test14 = App(Lam("x", Plus(V("x"), Num(1))), Num(5))
let expected14 = Prim(N(6))

(* Test 15: Conditional expression *)
let test15 = IFTE(Gt(Num(5), Num(3)), Num(1), Num(2))
let expected15 = Prim(N(1))

(* Complex test cases for SECD Machine *)

(* Complex Arithmetic-Boolean Tests *)

(* Test 16: Complex nested arithmetic with multiple operations *)
let test16 = Plus(Times(Num(3), Plus(Num(4), Num(2))), Sub(Num(20), Times(Num(2), Num(3))))
let expected16 = Prim(N(32))  (* 3 * (4 + 2) + (20 - 2 * 3) = 3 * 6 + 14 = 18 + 14 = 32 *)

(* Test 17: Complex boolean expression with short-circuit behavior *)
let test17 = Or(And(Gt(Num(5), Num(3)), Lt(Num(10), Num(20))), And(Eq(Num(7), Num(7)), Not(Bool(false))))
let expected17 = Prim(B(true))  (* (5>3 && 10<20) || (7=7 && !false) = (true && true) || (true && true) = true || true = true *)

(* Test 18: Mixed arithmetic-boolean with comparison *)
let test18 = Eq(Plus(Times(Num(2), Num(3)), Num(4)), Times(Num(2), Plus(Num(3), Num(2))))
let expected18 = Prim(B(true))  (* (2*3+4) == 2*(3+2) = 10 == 10 = true *)

(* Test 19: Complex conditional with nested operations *)
let test19 = IFTE(
  And(Gt(Num(10), Num(5)), Not(Eq(Num(3), Num(4)))),
  Plus(Times(Num(2), Num(3)), Num(4)),
  Sub(Num(20), Num(5))
)
let expected19 = Prim(N(10))  (* if (10>5 && !(3=4)) then (2*3+4) else (20-5) = if (true && true) then 10 else 15 = 10 *)

(* Test 20: Deeply nested arithmetic with multiple operations *)
let test20 = Times(
  Plus(Num(2), Times(Num(3), Num(4))),
  Sub(Num(20), Plus(Num(5), Num(3)))
)
let expected20 = Prim(N(168))  (* (2 + 3*4) * (20 - (5+3)) = (2 + 12) * 12 = 14 * 12 = 168 *)

(* Test 21: Complex comparison chain *)
let test21 = And(
  Lt(Plus(Num(3), Num(4)), Times(Num(2), Num(5))),
  Gt(Sub(Num(20), Num(5)), Plus(Num(7), Num(3)))
)
let expected21 = Prim(B(true))  (* (3+4 < 2*5) && (20-5 > 7+3) = (7 < 10) && (15 > 10) = true && true = true *)

(* Lambda Calculus Tests *)

(* Test 22: Identity function application *)
let test22 = App(Lam("x", V("x")), Plus(Num(3), Num(4)))
let expected22 = Prim(N(7))  (* (λx.x) (3+4) = 7 *)

(* Test 23: Function composition *)
let test23 = App(
  App(
    Lam("f", Lam("g", Lam("x", App(V("f"), App(V("g"), V("x")))))),
    Lam("y", Plus(V("y"), Num(1)))
  ),
  Lam("z", Times(V("z"), Num(2)))
)
let expected23 = Clos(ref (SECD_Clos {
  param = "x";
  code = ref [MkCLOS("f", [MkCLOS("g", [MkCLOS("x", [LOOKUP "x"; LOOKUP "g"; APP; LOOKUP "f"; APP; RET])])]); APP;
             [MkCLOS("y", [LOOKUP "y"; LDN 1; ADD; RET])];APP;
             [MkCLOS("z", [LOOKUP "z"; LDN 2; MUL; RET])]];
  table = ref (create_gamma (Some (ref (create_gamma (Some (ref (create_new_gamma ())))))))
}))  (* (λf.λg.λx.f(g x)) (λy.y+1) (λz.z*2) = λx.(λy.y+1)((λz.z*2) x) *)

(* Test 24: Currying - add function *)
let test24 = App(
  App(Lam("x", Lam("y", Plus(V("x"), V("y")))), Num(5)),
  Num(3)
)
let expected24 = Prim(N(8))  (* ((λx.λy.x+y) 5) 3 = (λy.5+y) 3 = 5+3 = 8 *)

(* Test 25: Higher-order function *)
let test25 = App(
  Lam("f", App(V("f"), Num(3))),
  Lam("x", Times(V("x"), Num(2)))
)
let expected25 = Prim(N(6))  (* (λf.f 3) (λx.x*2) = (λx.x*2) 3 = 3*2 = 6 *)

(* Test 26: Church encoding of true *)
let church_true = Lam("x", Lam("y", V("x")))
let test26 = App(App(church_true, Num(1)), Num(2))
let expected26 = Prim(N(1))  (* (λx.λy.x) 1 2 = 1 *)

(* Test 27: Church encoding of false *)
let church_false = Lam("x", Lam("y", V("y")))
let test27 = App(App(church_false, Num(1)), Num(2))
let expected27 = Prim(N(2))  (* (λx.λy.y) 1 2 = 2 *)

(* Test 28: Church numeral for 2 *)
let church_two = Lam("f", Lam("x", App(V("f"), App(V("f"), V("x")))))
let test28 = App(
  App(church_two, Lam("n", Plus(V("n"), Num(1)))),
  Num(0)
)
let expected28 = Prim(N(2))  (* (λf.λx.f(f x)) (λn.n+1) 0 = (λx.(λn.n+1)((λn.n+1) x)) 0 = (λn.n+1)((λn.n+1) 0) = (λn.n+1)(0+1) = (λn.n+1)(1) = 1+1 = 2 *)

(* Test 29: Church numeral addition *)
let church_add = Lam("m", Lam("n", Lam("f", Lam("x", 
  App(App(V("m"), V("f")), App(App(V("n"), V("f")), V("x")))
))))
let church_one = Lam("f", Lam("x", App(V("f"), V("x"))))
let test29 = App(
  App(
    App(App(church_add, church_one), church_two),
    Lam("n", Plus(V("n"), Num(1)))
  ),
  Num(0)
)
let expected29 = Prim(N(3))  (* church_add church_one church_two = church_three, which applies f 3 times *)

(* Test 30: Complex mixed expression with lambda and arithmetic *)
let test30 = App(
  Lam("x", 
    IFTE(
      Gt(V("x"), Num(10)),
      App(Lam("y", Times(V("y"), Num(2))), V("x")),
      App(Lam("z", Plus(V("z"), Num(5))), V("x"))
    )
  ),
  Plus(Num(3), Num(4))
)
let expected30 = Prim(N(12))  (* (λx.if x>10 then (λy.y*2) x else (λz.z+5) x) (3+4) = (λx.if x>10 then (λy.y*2) x else (λz.z+5) x) 7 = if 7>10 then (λy.y*2) 7 else (λz.z+5) 7 = (λz.z+5) 7 = 7+5 = 12 *)

(* Test 31: Nested lambda with arithmetic *)
let test31 = App(
  Lam("x", 
    App(
      Lam("y", Plus(Times(V("x"), V("x")), Times(V("y"), V("y")))),
      Plus(V("x"), Num(1))
    )
  ),
  Num(3)
)
let expected31 = Prim(N(25))  (* (λx.(λy.x*x + y*y) (x+1)) 3 = (λy.3*3 + y*y) (3+1) = (λy.9 + y*y) 4 = 9 + 4*4 = 9 + 16 = 25 *)


(* Run all tests *)
let () =
run_secd_test "Simple numeric value" test1 expected1;
run_secd_test "Simple boolean value" test2 expected2;
run_secd_test "Simple arithmetic - Plus" test3 expected3;
run_secd_test "Simple arithmetic - Times" test4 expected4;
run_secd_test "Simple arithmetic - Sub" test5 expected5;
run_secd_test "Simple boolean operation - And" test6 expected6;
run_secd_test "Simple boolean operation - Or" test7 expected7;
run_secd_test "Simple boolean operation - Not" test8 expected8;
run_secd_test "Comparison operation - Eq" test9 expected9;
run_secd_test "Comparison operation - Gt" test10 expected10;
run_secd_test "Comparison operation - Lt" test11 expected11;
run_secd_test "Nested arithmetic" test12 expected12;
run_secd_test "Simple lambda and application" test13 expected13;
run_secd_test "Lambda with arithmetic" test14 expected14;
run_secd_test "Conditional expression" test15 expected15;
run_secd_test "Complex nested arithmetic" test16 expected16;
run_secd_test "Complex boolean expression" test17 expected17;
run_secd_test "Mixed arithmetic-boolean with comparison" test18 expected18;
run_secd_test "Complex conditional with nested operations" test19 expected19;
run_secd_test "Deeply nested arithmetic" test20 expected20;
run_secd_test "Complex comparison chain" test21 expected21;
run_secd_test "Identity function application" test22 expected22;
run_secd_test "Function composition" test23 expected23;
run_secd_test "Currying - add function" test24 expected24;
run_secd_test "Higher-order function" test25 expected25;
run_secd_test "Church encoding of true" test26 expected26;
run_secd_test "Church encoding of false" test27 expected27;
run_secd_test "Church numeral for 2" test28 expected28;
run_secd_test "Church numeral addition" test29 expected29;
run_secd_test "Complex mixed expression" test30 expected30;
run_secd_test "Nested lambda with arithmetic" test31 expected31
 