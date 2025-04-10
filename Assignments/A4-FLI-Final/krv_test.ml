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

  exception Mismatch of string
let run_test name expr gamma_ref expected =
    print_endline ("Test: " ^ name);
    try
      let cl = create_krv_clos expr gamma_ref in
      (* let _ = print_endline (string_of_lamexp expr) in *)
      let result = krv_machine (KRV_Clos cl) [] in
      let answer = extract_answer result in
      print_string "  Result: ";
      print_endline (string_of_answer answer);
      print_string "  Expected: ";
      print_endline (string_of_answer expected);
      
      (* Compare based on the type of answer *)
      let passes = match answer, expected with
        | Prim(p1), Prim(p2) -> p1 = p2  (* For primitive values, compare directly *)
        | Clos(ref_cl1), Clos(ref_cl2) -> (
            match !ref_cl1, !ref_cl2 with
            | KRV_Clos krv1, KRV_Clos krv2 -> 
                (* For closures, compare only the expression part *)
                let _ = print_endline ("Expected :"^ string_of_lamexp krv2.expr) in
                let _ = print_endline ("Produced :"^ string_of_lamexp krv1.expr) in
                krv1.expr = krv2.expr
            | _, _ -> raise (Mismatch("Mismatched Test Case Answer and Machine Answer"))
          )
        | _, _ -> raise (Mismatch("Mismatched Test Case Answer and Machine Answer"))
      in
      
      print_endline (if passes then "  ✓ PASS" else "  ✗ FAIL");
      print_endline ""
    with
    | e -> 
        print_string "  Error: ";
        print_endline (Printexc.to_string e);
        print_endline "  ✗ FAIL";
        print_endline ""

(*===================================================================================*)
(*===================================================================================*)

(* Test 1: Simple arithmetic - Plus *)
let test1 = Plus(Num(3), Num(4))
let expected1 = Prim(N(7))

(* Test 2: Nested arithmetic *)
let test2 = Plus(Times(Num(2), Num(3)), Num(4))
let expected2 = Prim(N(10))

(* Test 3: Boolean operations *)
let test3 = And(Bool(true), Or(Bool(false), Bool(true)))
let expected3 = Prim(B(true))

(* Test 4: Conditional expression *)
let test4 = IFTE(Gt(Num(5), Num(3)), Plus(Num(1), Num(2)), Times(Num(2), Num(3)))
let expected4 = Prim(N(3))

(* Test 5: Simple lambda application *)
let test5 = App(Lam("x", Plus(V("x"), Num(1))), Num(5))
let expected5 = Prim(N(6))

(* Test 6: Higher-order function *)
let test6 = App(
  App(
    Lam("f", Lam("x", App(V("f"), V("x")))),
    Lam("y", Times(V("y"), Num(2)))
  ),
  Num(3)
)
let expected6 = Prim(N(6))

(* Test 7: Lambda with free variable *)
let test7_env = ref (create_new_gamma())
let _ = add_binding test7_env "y" (Prim(N(10)))
let test7 = Lam("x", Plus(V("x"), V("y")))
let expected7 = Clos(ref (KRV_Clos {expr = Lam("x", Plus(V("x"), V("y"))); table = test7_env}))

(* Test 8: Application with environment *)
let test8_env = ref (create_new_gamma())
let _ = add_binding test8_env "z" (Clos(ref (KRV_Clos {expr = Num(5); table = ref (create_new_gamma())})))
let test8 = App(Lam("x", Plus(V("x"), V("z"))), Num(3))
let expected8 = Prim(N(8))

(* Test 9: Church encoding of true with environment *)
let test9_env = ref (create_new_gamma())
let test9 = App(App(Lam("x", Lam("y", V("x"))), V("a")), V("b"))
let _ = add_binding test9_env "a" (Clos(ref (KRV_Clos {expr = Num(1); table = ref (create_new_gamma())})))
let _ = add_binding test9_env "b" (Clos(ref (KRV_Clos {expr = Num(2); table = ref (create_new_gamma())})))
let expected9 = Prim(N(1))

(* Test 10: Church encoding of successor with environment *)
let test10_env = ref (create_new_gamma())
(* let church_one = Lam("f", Lam("x", App(V("f"), V("x"))))
let church_succ = Lam("n", Lam("f", Lam("x", App(V("f"), App(App(V("n"), V("f")), V("x"))))))
let _ = add_binding test10_env "n" (Clos(ref (KRV_Clos {expr = church_one; table = ref (create_new_gamma())}))) *)
let test10 = Lam("f", Lam("x", App(V("f"), App(App(V("n"), V("f")), V("x")))))
let expected10 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", App(V("f"), App(App(V("n"), V("f")), V("x"))))); table = test10_env}))

let test11_env = ref (create_new_gamma())
let _ = add_binding test11_env "y" (Clos(ref (KRV_Clos {expr = Num(10); table = ref (create_new_gamma())})))
let test11 = App(Lam("x", Plus(V("x"), V("y"))), Num(5))
let expected11 = Prim(N(15))

let test12_env = ref (create_new_gamma())
let _ = add_binding test12_env "z" (Clos(ref (KRV_Clos {expr = Num(3); table = ref (create_new_gamma())})))
let test12 = App(App(Lam("x", Lam("y", Plus(V("x"), Plus(V("y"), V("z"))))), Num(1)), Num(2))
let expected12 = Prim(N(6))


let test13_env = ref (create_new_gamma())
let _ = add_binding test13_env "f" (Clos(ref (KRV_Clos {expr = Lam("x", Times(V("x"), Num(2))); table = ref (create_new_gamma())})))
let test13 = App(App(Lam("g", Lam("x", App(V("g"), App(V("f"), V("x"))))), Lam("y", Plus(V("y"), Num(1)))), Num(3))
let expected13 = Prim(N(7))

let test14_env = ref (create_new_gamma())
let _ = add_binding test14_env "cond" (Clos(ref (KRV_Clos {expr = Bool(true); table = ref (create_new_gamma())})))
let test14 = IFTE(V("cond"), Num(1), Num(2))
let expected14 = Prim(N(1))

let test15_env = ref (create_new_gamma())
let _ = add_binding test15_env "a" (Clos(ref (KRV_Clos {expr = Num(2); table = ref (create_new_gamma())})))
let _ = add_binding test15_env "b" (Clos(ref (KRV_Clos {expr = Num(3); table = ref (create_new_gamma())})))
let test15 = Plus(Times(V("a"), V("b")), App(Lam("x", Plus(V("x"), Num(1))), Times(V("a"), V("b"))))
let expected15 = Prim(N(13))

let test16_env = ref (create_new_gamma())
let _ = add_binding test16_env "h" (Clos(ref (KRV_Clos {expr = Lam("x", Plus(V("x"), Num(1))); table = ref (create_new_gamma())})))
let test16 = App(App(Lam("f", Lam("x", App(V("f"), App(V("h"), V("x"))))), Lam("y", Times(V("y"), Num(2)))), Num(3))
let expected16 = Prim(N(8))

let test17_env = ref (create_new_gamma())
let _ = add_binding test17_env "x" (Clos(ref (KRV_Clos {expr = Num(5); table = ref (create_new_gamma())})))
let _ = add_binding test17_env "y" (Clos(ref (KRV_Clos {expr = Num(3); table = ref (create_new_gamma())})))
let test17 = App(Lam("z", Plus(Times(V("x"), V("y")), V("z"))), App(Lam("w", Plus(V("w"), V("y"))), V("x")))
let expected17 = Prim(N(23))

let test18_env = ref (create_new_gamma())
let _ = add_binding test18_env "p" (Clos(ref (KRV_Clos {expr = Bool(true); table = ref (create_new_gamma())})))
let _ = add_binding test18_env "q" (Clos(ref (KRV_Clos {expr = Bool(false); table = ref (create_new_gamma())})))
let test18 = And(V("p"), Or(V("q"), Not(V("p"))))
let expected18 = Prim(B(false))

let test19_env = ref (create_new_gamma())
let _ = add_binding test19_env "z" (Clos(ref (KRV_Clos {expr = Num(4); table = ref (create_new_gamma())})))
let test19 = App(App(App(Lam("x", Lam("y", Lam("w", Plus(Plus(V("x"), V("y")), Plus(V("w"), V("z")))))), Num(1)), Num(2)), Num(3))
let expected19 = Prim(N(10))

let test20_env = ref (create_new_gamma())
let _ = add_binding test20_env "a" (Clos(ref (KRV_Clos {expr = Num(5); table = ref (create_new_gamma())})))
let _ = add_binding test20_env "b" (Clos(ref (KRV_Clos {expr = Num(3); table = ref (create_new_gamma())})))
let test20 = IFTE(Gt(V("a"), V("b")), 
                  App(Lam("x", Plus(V("x"), V("a"))), V("b")),
                  App(Lam("y", Times(V("y"), V("b"))), V("a")))
let expected20 = Prim(N(8))


let test21_env = ref (create_new_gamma())
let _ = add_binding test21_env "add" (Clos(ref (KRV_Clos {expr = Lam("x", Lam("y", Plus(V("x"), V("y")))); table = ref (create_new_gamma())})))
let test21 = App(App(App(Lam("op", Lam("a", Lam("b", App(App(V("op"), V("a")), V("b"))))), V("add")), Num(7)), Num(5))
let expected21 = Prim(N(12))


let test22_env = ref (create_new_gamma())
let _ = add_binding test22_env "x" (Clos(ref (KRV_Clos {expr = Num(3); table = ref (create_new_gamma())})))
let _ = add_binding test22_env "y" (Clos(ref (KRV_Clos {expr = Num(4); table = ref (create_new_gamma())})))
let test22 = Plus(Times(Plus(V("x"), Num(2)), Sub(Times(V("y"), Num(2)), Num(3))), Times(Num(2), Plus(V("x"), V("y"))))
let expected22 = Prim(N(39))


let test23_env = ref (create_new_gamma())
let _ = add_binding test23_env "f" (Clos(ref (KRV_Clos {expr = Lam("x", Plus(V("x"), Num(1))); table = ref (create_new_gamma())})))
let _ = add_binding test23_env "g" (Clos(ref (KRV_Clos {expr = Lam("x", Times(V("x"), Num(2))); table = ref (create_new_gamma())})))
let test23 = App(App(Lam("f", Lam("g", Lam("x", App(V("f"), App(V("g"), V("x")))))), V("f")), V("g"))
let expected23 = Clos(ref (KRV_Clos {expr = Lam("x", App(V("f"), App(V("g"), V("x")))); table = test23_env}))

let test24_env = ref (create_new_gamma())
let _ = add_binding test24_env "threshold" (Clos(ref (KRV_Clos {expr = Num(10); table = ref (create_new_gamma())})))
let test24 = IFTE(Gt(Times(Num(3), Num(4)), V("threshold")),
                  IFTE(Eq(Plus(Num(5), Num(5)), V("threshold")),
                       Num(1),
                       Num(2)),
                  IFTE(Lt(Num(5), V("threshold")),
                       Num(3),
                       Num(4)))
let expected24 = Prim(N(1))

(* Using Fixed point Y-Combinator to emulate factorial function - It works !*)
let test25_env = ref (create_new_gamma())
let factorial_body = Lam("n", IFTE(Eq(V("n"), Num(0)),
                                  Num(1),
                                  Times(V("n"), App(V("factorial"), Sub(V("n"), Num(1))))))
let factorial_closure = ref (KRV_Clos {expr = factorial_body; table = test25_env})
let _ = add_binding test25_env "factorial" (Clos(factorial_closure))
let test25 = App(V("factorial"), Num(4))
let expected25 = Prim(N(24))


let test26_env = ref (create_new_gamma())
let _ = add_binding test26_env "map" (Clos(ref (KRV_Clos {
  expr = Lam("f", Lam("x", Lam("y", Lam("z", 
    App(App(App(V("f"), V("x")), V("y")), V("z"))))));
  table = ref (create_new_gamma())})))
let _ = add_binding test26_env "combine" (Clos(ref (KRV_Clos {
  expr = Lam("a", Lam("b", Lam("c", Plus(Plus(V("a"), V("b")), V("c")))));
  table = ref (create_new_gamma())})))
let test26 = App(App(App(App(V("map"), V("combine")), Num(1)), Num(2)), Num(3))
let expected26 = Prim(N(6))


let test27 = App(
  Lam("x", 
    App(
      Lam("y", 
        App(
          Lam("z", 
            App(
              Lam("x", Plus(Plus(V("x"), V("y")), V("z"))),
              Times(V("x"), Num(2))
            )
          ),
          Times(V("x"), Num(3))
        )
      ),
      Plus(V("x"), Num(1))
    )
  ),
  Num(2)
)
let expected27 = Prim(N(13))


let test28_env = ref (create_new_gamma())
let _ = add_binding test28_env "a" (Clos(ref (KRV_Clos {expr = Bool(true); table = ref (create_new_gamma())})))
let _ = add_binding test28_env "b" (Clos(ref (KRV_Clos {expr = Bool(false); table = ref (create_new_gamma())})))
let _ = add_binding test28_env "c" (Clos(ref (KRV_Clos {expr = Bool(true); table = ref (create_new_gamma())})))
let test28 = Or(And(V("a"), Or(V("b"), Not(V("c")))), And(Not(V("a")), And(V("b"), V("c"))))
let expected28 = Prim(B(false))

let test29_env = ref (create_new_gamma())
let _ = add_binding test29_env "base" (Clos(ref (KRV_Clos {expr = Num(10); table = ref (create_new_gamma())})))
let _ = add_binding test29_env "multiplier" (Clos(ref (KRV_Clos {expr = Num(2); table = ref (create_new_gamma())})))
let test29 = App(
  App(
    App(
      App(
        Lam("base", Lam("mult", Lam("a", Lam("b", 
          Plus(Times(V("base"), V("mult")), Plus(V("a"), V("b")))
        )))),
        V("base")
      ),
      V("multiplier")
    ),
    Num(5)
  ),
  Num(7)
)
let expected29 = Prim(N(32))


let test30_env = ref (create_new_gamma())
let _ = add_binding test30_env "compute" (Clos(ref (KRV_Clos {
  expr = Lam("x", Lam("y", 
    IFTE(Gt(V("x"), V("y")),
         Plus(V("x"), Times(V("y"), Num(2))),
         Times(V("x"), Plus(V("y"), Num(2))))
  ));
  table = ref (create_new_gamma())})))
let _ = add_binding test30_env "transform" (Clos(ref (KRV_Clos {
  expr = Lam("f", Lam("a", Lam("b", 
    App(App(V("f"), Times(V("a"), Num(2))), Plus(V("b"), Num(3)))
  )));
  table = ref (create_new_gamma())})))
let test30 = App(App(App(V("transform"), V("compute")), Num(4)), Num(7))
let expected30 = Prim(N(96))


(* Define the K and S combinators *)
let k = Lam("x", Lam("y", V("x")))
let s = Lam("x", Lam("y", Lam("z", App(App(V("x"), V("z")), App(V("y"), V("z"))))))

(* Create an environment with k and s bound *)
let sk_env = ref (create_new_gamma())
let _ = add_binding sk_env "k" (Clos(ref (KRV_Clos {expr = k; table = ref (create_new_gamma())})))
let _ = add_binding sk_env "s" (Clos(ref (KRV_Clos {expr = s; table = ref (create_new_gamma())})))

(* Test case: (s k) k *)
let test_sk = App(App(V("s"), V("k")), V("k"))

(* Expected result: λz.k z (k z) = λz.z *)
(* Since k z returns z and discards its second argument, (k z) (k z) simplifies to z *)
let expected_sk = Clos(ref (KRV_Clos {expr = Lam("z", App(App(V("k"), V("z")), App(V("k"), V("z")))); table = ref (create_new_gamma())}))

(* Test case: ((s k) k) v *)

let sk_app_env = ref (create_new_gamma ())
let _ = add_binding sk_app_env "k" (Clos(ref (KRV_Clos {expr = k; table = ref (create_new_gamma())})))
let _ = add_binding sk_app_env "s" (Clos(ref (KRV_Clos {expr = s; table = ref (create_new_gamma())})))
let _ = add_binding sk_app_env "v"  (Clos(ref (KRV_Clos {expr = Num(42); table = ref (create_new_gamma())})))
let test_sk_applied = App(App(App(V("s"), V("k")), V("k")), V("v"))

(* Expected result: v *)
let expected_sk_applied = Prim(N(42))

let omega = Lam("x", App(V("x"), V("x")))

(* Create an environment *)
let omega_env = ref (create_new_gamma())
let _ = add_binding omega_env "omega" (Clos(ref (KRV_Clos {expr = omega; table = ref (create_new_gamma())})))

(* Test case 1: Just the omega combinator itself *)
let test_omega1 = V("omega")
let expected_omega1 = Clos(ref (KRV_Clos {expr = omega; table = ref (create_new_gamma())}))

(* Test case 2: Omega applied to itself - should not terminate if evaluated fully *)
(* But in call-by-name, it should return a closure without evaluating further *)
let test_omega2 = App(omega, omega)

(* In call-by-name, the machine should return a closure representing the application *)
(* without attempting to fully evaluate it *)
let expected_omega2 = Clos(ref (KRV_Clos {expr = App(omega, omega); table = ref (create_new_gamma())}))
(* Run all tests *)
let () =
  run_test "Simple arithmetic - Plus" test1 (ref (create_new_gamma())) expected1;
  run_test "Nested arithmetic" test2 (ref (create_new_gamma())) expected2;
  run_test "Boolean operations" test3 (ref (create_new_gamma())) expected3;
  run_test "Conditional expression" test4 (ref (create_new_gamma())) expected4;
  run_test "Simple lambda application" test5 (ref (create_new_gamma())) expected5;
  run_test "Higher-order function" test6 (ref (create_new_gamma())) expected6;
  run_test "Lambda with free variable" test7 test7_env expected7;
  run_test "Application with environment" test8 test8_env expected8;
  run_test "Church encoding of true with environment" test9 test9_env expected9;
  run_test "Church encoding of successor with environment" test10 (ref (create_new_gamma())) expected10;
  run_test "Application with external binding" test11 test11_env expected11;
  run_test "Nested lambda with external binding" test12 test12_env expected12;
  run_test "Higher-order function with external binding" test13 test13_env expected13;
  run_test "Conditional with external binding" test14 test14_env expected14;
  run_test "Complex arithmetic with external bindings" test15 test15_env expected15;
  run_test "Nested application with external binding" test16 test16_env expected16;
  run_test "Multiple external bindings in complex expression" test17 test17_env expected17;
  run_test "Boolean operations with external bindings" test18 test18_env expected18;
  run_test "Lambda with multiple arguments using currying" test19 test19_env expected19;
  run_test "Complex conditional with external bindings" test20 test20_env expected20;
  run_test "HOF applying function to two integers" test21 test21_env expected21;
  run_test "Complex nested arithmetic" test22 test22_env expected22;
  run_test "Function composition" test23 test23_env expected23;
  run_test "Nested conditionals" test24 test24_env expected24;
  run_test "Recursive-style function" test25 test25_env expected25;
  run_test "Complex HOF with multiple arguments" test26 test26_env expected26;
  run_test "Nested lambdas with shared variables" test27 (ref (create_new_gamma())) expected27;
  run_test "Complex boolean logic" test28 test28_env expected28;
  run_test "Curried function with multiple applications" test29 test29_env expected29;
  run_test "Complex expression combining features" test30 test30_env expected30;
  run_test "SK Combinator Test: (s k) k" test_sk sk_env expected_sk;
  run_test "SK Combinator Applied: ((s k) k) v" test_sk_applied sk_app_env expected_sk_applied;
  run_test "Omega Combinator" test_omega1 omega_env expected_omega1;
  run_test "Omega applied to itself" test_omega2 (ref (create_new_gamma())) expected_omega2;
  print_endline "Test completed successfully - machine correctly implements call-by-name evaluation";