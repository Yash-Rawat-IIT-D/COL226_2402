#use "a2.ml"



(*===================================================================================*)

let print_result (desc : string) (result : bool) : unit =
  Printf.printf "%s: %s\n" desc (if result then "Passed" else "Failed")
;;

(* Test cases for type_of function *)

let test_type_bool () = 
  let tests = [
    (T, Bool, "Type of T");
    (F, Bool, "Type of F");
    (Add (T, F), Bool, "Type of Add (T, F)");
    (Inv (T), Bool, "Type of Inv T");
    (ScalProd(Inv (T),Add(Inv (T),Inv (F))), Bool, "Type of ScalProd(Inv (T),Add(Inv (T),Inv (F)))");
    (ScalProd (T, F), Bool, "Type of ScalProd (T, F)");
    (IsZero (ConstV [0.0; 0.0; 0.0; 0.0]), Bool, "Type of IsZero (ConstV [0.0; 0.0; 0.0; 0.0])");
    ] in
  List.iter (fun (expr, expected_type, desc) ->
    let result = try type_of expr = expected_type with _ -> false in
    print_result desc result
  ) tests
;;

let test_type_scalar() = 
  let tests = [
    (ConstS 3.1415, Scalar, "Type of ConstS 3.14");
    (Add (ConstS 192.01, ConstS 57.44), Scalar, "Type of Add (ConstS 192.01, ConstS 57.44)");
    (Sub (Add (ConstS 192.01, ConstS 57.44), Add (ConstS 46.44, ConstS 341.76)), Scalar, "Type of Sub (Add (ConstS 192.01, ConstS 57.44), Add (ConstS 46.44, ConstS 341.76)) :");
    (Inv ( DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])), Scalar, "Type of Inv ( DotProd (ConstV [1.0; 2.0; 3.0], ConstV [4.0; 5.0; 6.0])) : ");
    (Mag (ScalProd (ConstS 2.0, ConstV [1.0; 2.0])), Vector 2, "Type of ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
    (DotProd (ScalProd (ConstS 5.43, ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS 1.11)), Scalar, "DotProd (ScalProd (ConstS 5.43, ConstV [1.33; 2.23]), ScalProd (ConstV [7.76; 2.35],ConstS 1.11))) : ");
    (* (Mag (ConstV [3.0; 4.0]), Scalar, "Type of Mag (ConstV [3.0; 4.0])"); *)
    (Angle (ConstV [1.0; 2.0; 3.0], ConstV [-3.0; -4.0; 5.0]), Scalar, "Type ofAngle (ConstV [1.0; 2.0; 3.0], ConstV [-3.0; -4.0; 5.0]) : ");
    (IsZero (ConstS 0.0), Bool, "Type of IsZero (ConstS 0.0)");
    (Cond (T, ConstS 1.0, ConstS 2.0), Scalar, "Type of Cond (T, ConstS 1.0, ConstS 2.0)")
  ] in
  List.iter (fun (expr, expected_type, desc) ->
    let result = try type_of expr = expected_type with _ -> false in
    print_result desc result
  ) tests

  let test_type_vector () = 
    let tests = [
      (ConstV [1.0; 2.0; 3.0], Vector 3, "Type of ConstV [1.0; 2.0; 3.0]");
      (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Vector 2, "Type of Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
      (Sub (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Vector 2, "Type of Sub (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
      (Inv (ConstV [1.0; 2.0]), Vector 2, "Type of Inv (ConstV [1.0; 2.0])");
      (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), Vector 2, "Type of ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
      (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Scalar, "Type of DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
      (Mag (ConstV [3.0; 4.0]), Scalar, "Type of Mag (ConstV [3.0; 4.0])");
      (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Scalar, "Type of Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])");
      (IsZero (ConstV [0.0; 0.0]), Bool, "Type of IsZero (ConstV [0.0; 0.0])");
      (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Vector 2, "Type of Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])")
    ] in
    List.iter (fun (expr, expected_type, desc) ->
      let result = try type_of expr = expected_type with _ -> false in
      print_result desc result
    ) tests
  ;;

let test_type () =

  let tests = [
    (T, Bool, "Type of T");
    (F, Bool, "Type of F");
    (ConstS 3.14, Scalar, "Type of ConstS 3.14");
    (ConstV [1.0; 2.0; 3.0], Vector 3, "Type of ConstV [1.0; 2.0; 3.0]");
    (Add (ConstS 1.0, ConstS 2.0), Scalar, "Type of Add (ConstS 1.0, ConstS 2.0)");
    (Sub (ConstS 1.0, ConstS 2.0), Scalar, "Type of Sub (ConstS 1.0, ConstS 2.0)");
    (Inv (ConstS 1.0), Scalar, "Type of Inv (ConstS 1.0)");
    (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), Vector 2, "Type of ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
    (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), Scalar, "Type of DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
    (Mag (ConstV [3.0; 4.0]), Scalar, "Type of Mag (ConstV [3.0; 4.0])");
    (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), Scalar, "Type of Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])");
    (IsZero (ConstS 0.0), Bool, "Type of IsZero (ConstS 0.0)");
    (Cond (T, ConstS 1.0, ConstS 2.0), Scalar, "Type of Cond (T, ConstS 1.0, ConstS 2.0)")
  ] in
  List.iter (fun (expr, expected_type, desc) ->
    let result = try type_of expr = expected_type with _ -> false in
    print_result desc result
  ) tests
;;

(* Test cases for eval function *)
let test_eval () =
    let tests = [
    (T, B true, "Eval T");
    (F, B false, "Eval F");
    (ConstS 3.14, S 3.14, "Eval ConstS 3.14");
    (ConstV [1.0; 2.0; 3.0], V [1.0; 2.0; 3.0], "Eval ConstV [1.0; 2.0; 3.0]");
    (Add (ConstS 1.0, ConstS 2.0), S 3.0, "Eval Add (ConstS 1.0, ConstS 2.0)");
    (Sub (ConstS 1.0, ConstS 2.0), S (-1.0), "Eval Sub (ConstS 1.0, ConstS 2.0)");
    (Inv (ConstS 1.0), S (-1.0), "Eval Inv (ConstS 1.0)");
    (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), V [2.0; 4.0], "Eval ScalProd (ConstS 2.0, ConstV [1.0; 2.0])");
    (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), S 11.0, "Eval DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0])");
    (Mag (ConstV [3.0; 4.0]), S 5.0, "Eval Mag (ConstV [3.0; 4.0])");
    (Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0]), S (Float.pi /. 2.0), "Eval Angle (ConstV [1.0; 0.0], ConstV [0.0; 1.0])");
    (IsZero (ConstS 0.0), B true, "Eval IsZero (ConstS 0.0)");
    (Cond (T, ConstS 1.0, ConstS 2.0), S 1.0, "Eval Cond (T, ConstS 1.0, ConstS 2.0)")
  ] in
  List.iter (fun (expr, expected_value, desc) ->
    let result = try eval expr = expected_value with _ -> false in
    print_result desc result
  ) tests
;;

(* Run all tests *)
let () =
  print_endline "===================================================================================";
  print_endline "Running type_bool tests:";
  test_type_bool();
  print_endline "===================================================================================";
  print_endline "Running type_scalar tests:";
  test_type_scalar();
  print_endline "===================================================================================";
  print_endline "Running eval tests:";
  test_type_vector();
  
  print_endline "===================================================================================";
  print_endline "Running eval tests:";
  test_eval ();
;;


(*===================================================================================*)