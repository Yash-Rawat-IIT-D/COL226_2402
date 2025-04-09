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
  let run_test name expr expected =
    print_endline ("Test: " ^ name);
    try
      let cl = create_krv_clos expr (ref (create_new_gamma ())) in
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
let expected23 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", App(V("f"), App(App(V("n"), V("f")), V("x"))))); 
table = 
(let new_table = ref (create_new_gamma ()) in
 let _ = add_binding new_table "n" (Clos (ref (KRV_Clos {expr = church_one; table = ref (create_new_gamma())}))) in
 new_table
)}))

let test_free_var = Lam("x", Plus(V("x"), Times(Num(2), V("y"))))
let expected_free_var = Clos(ref (KRV_Clos {expr = Lam("x", Plus(V("x"), Times(Num(2), V("y")))); table = ref (create_new_gamma ())}))

let test_lambda_plus = App(Lam("x", Plus(V("x"), Num(2))), V("y"))

let expected_lambda_plus = Clos(ref (KRV_Clos {expr = Plus(V("y"), Num(2));
 table = 
 (let new_table = ref (create_new_gamma ()) in
  let _ = add_binding new_table "x" (Clos (ref (KRV_Clos {expr = V("y"); table = ref (create_new_gamma())}))) in
  new_table
 )}))

(* Test 24: Church encoding of numeral zero *)
let church_zero = Lam("f", Lam("x", V("x")))
let expected24 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", V("x"))); table = ref (create_new_gamma ())}))

(* Test 25: Church encoding of numeral two *)
let church_two = Lam("f", Lam("x", App(V("f"), App(V("f"), V("x")))))
let expected25 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", App(V("f"), App(V("f"), V("x"))))); table = ref (create_new_gamma ())}))

(* Test 26: Church encoding of addition *)
let church_add = Lam("m", Lam("n", Lam("f", Lam("x", App(App(V("m"), V("f")), App(App(V("n"), V("f")), V("x")))))))
let expected26 = Clos(ref (KRV_Clos {expr = Lam("m", Lam("n", Lam("f", Lam("x", App(App(V("m"), V("f")), App(App(V("n"), V("f")), V("x"))))))); table = ref (create_new_gamma ())}))

(* Test 27: Application of Church addition to Church numerals one and two *)
let test27 = App(App(church_add, church_one), church_two)
let expected27 = Clos(ref (KRV_Clos {expr = Lam("f", Lam("x", App(App(V("m"), V("f")), App(App(V("n"), V("f")), V("x"))))); 
  table = 
  (let new_table = ref (create_new_gamma ()) in
   let _ = add_binding new_table "m" (Clos (ref (KRV_Clos {expr = church_one; table = ref (create_new_gamma())}))) in
   let _ = add_binding new_table "n" (Clos (ref (KRV_Clos {expr = church_two; table = ref (create_new_gamma())}))) in
   new_table
  )}))

(* Test 28: Church encoding of multiplication *)
let church_mult = Lam("m", Lam("n", Lam("f", App(V("m"), App(V("n"), V("f"))))))
let expected28 = Clos(ref (KRV_Clos {expr = Lam("m", Lam("n", Lam("f", App(V("m"), App(V("n"), V("f")))))); table = ref (create_new_gamma ())}))

(* Test 29: Application of Church multiplication to Church numerals two and one *)
let test29 = App(App(church_mult, church_two), church_one)
let expected29 = Clos(ref (KRV_Clos {expr = Lam("f", App(V("m"), App(V("n"), V("f")))); 
  table = 
  (let new_table = ref (create_new_gamma ()) in
   let _ = add_binding new_table "m" (Clos (ref (KRV_Clos {expr = church_two; table = ref (create_new_gamma())}))) in
   let _ = add_binding new_table "n" (Clos (ref (KRV_Clos {expr = church_one; table = ref (create_new_gamma())}))) in
   new_table
  )}))

(* Test 30: Church encoding of predecessor function *)
let church_pred = Lam("n", Lam("f", Lam("x", App(App(App(V("n"), Lam("g", Lam("h", App(V("h"), App(V("g"), V("f")))))), Lam("u", V("x"))), Lam("u", V("u"))))))
let expected30 = Clos(ref (KRV_Clos {expr = Lam("n", Lam("f", Lam("x", App(App(App(V("n"), Lam("g", Lam("h", App(V("h"), App(V("g"), V("f")))))), Lam("u", V("x"))), Lam("u", V("u")))))); table = ref (create_new_gamma ())}))

(* Test 31: Y combinator (for recursion) *)
let y_combinator = Lam("f", App(Lam("x", App(V("f"), App(V("x"), V("x")))), Lam("x", App(V("f"), App(V("x"), V("x"))))))
let expected31 = Clos(ref (KRV_Clos {expr = Lam("f", App(Lam("x", App(V("f"), App(V("x"), V("x")))), Lam("x", App(V("f"), App(V("x"), V("x")))))); table = ref (create_new_gamma ())}))

(* Test 32: Nested application with free variables *)
let test32 = App(App(Lam("x", Lam("y", Plus(V("x"), V("y")))), V("z")), Num(5))
let expected32 = Clos(ref (KRV_Clos {expr = Plus(V("z"), Num(5));
  table = 
  (let new_table = ref (create_new_gamma ()) in
   let _ = add_binding new_table "x" (Clos (ref (KRV_Clos {expr = V("z"); table = ref (create_new_gamma())}))) in
   let _ = add_binding new_table "y" (Clos (ref (KRV_Clos {expr = Num(5); table = ref (create_new_gamma())}))) in
   new_table
  )}))

(* Test 33: Church encoding of boolean NOT *)
let church_not = Lam("b", Lam("x", Lam("y", App(App(V("b"), V("y")), V("x")))))
let expected33 = Clos(ref (KRV_Clos {expr = Lam("b", Lam("x", Lam("y", App(App(V("b"), V("y")), V("x"))))); table = ref (create_new_gamma ())}))

(* Test 34: Application of Church NOT to Church true *)
let test34 = App(church_not, Lam("x", Lam("y", V("x"))))
let expected34 = Clos(ref (KRV_Clos {expr = Lam("x", Lam("y", App(App(V("b"), V("y")), V("x")))); 
  table = 
  (let new_table = ref (create_new_gamma ()) in
   let _ = add_binding new_table "b" (Clos (ref (KRV_Clos {expr = Lam("x", Lam("y", V("x"))); table = ref (create_new_gamma())}))) in
   new_table
  )}))

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
  (* run_test "Identity Function" test16 expected16; *)
  run_test "Application of Identity Funcion" test17 expected17;
  (* run_test "Church Encoding of True" test18 expected18; *)
  (* run_test "Church Encoding of False" test19 expected19; *)
  run_test "Application of Church true to two arguments" test20 expected20;
  run_test "Application of Church false to two arguments" test21 expected21;
  (* run_test "Church encoding of successor function" test22 expected22; *)
  run_test "Application of successor to a Church numeral" test23 expected23;
  run_test "Lambda with free variable" test_free_var expected_free_var;
  run_test "Lambda Plus" test_lambda_plus expected_lambda_plus;
  run_test "Church encoding of numeral zero" church_zero expected24;
  run_test "Church encoding of numeral two" church_two expected25;
  run_test "Church encoding of addition" church_add expected26;
  run_test "Application of Church addition to Church numerals one and two" test27 expected27;
  run_test "Church encoding of multiplication" church_mult expected28;
  run_test "Application of Church multiplication to Church numerals two and one" test29 expected29;
  run_test "Church encoding of predecessor function" church_pred expected30;
  run_test "Y combinator (for recursion)" y_combinator expected31;
  run_test "Nested application with free variables" test32 expected32;
  run_test "Church encoding of boolean NOT" church_not expected33;
  run_test "Application of Church NOT to Church true" test34 expected34;    

(*===================================================================================*)
(*===================================================================================*)

