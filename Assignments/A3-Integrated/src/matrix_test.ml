exception Dimension_Mismatch of string
exception Division_by_zero of string
let mat_dim mat = match mat with 
  | [] -> failwith "Empty Matrix not allowed"
  | row::_ -> (List.length mat, List.length row)

(* Transpose a matrix - works for both integer and float matrices *)
let transpose_matrix mat =
  if mat = [] then []
  else
    let (rows, cols) = mat_dim mat in
    List.init cols (fun col ->
      List.init rows (fun row ->
        List.nth (List.nth mat row) col
      )
    )
(* Aliases for type-spec operations *)
let transpose_matrix_n = transpose_matrix
let transpose_matrix_f = transpose_matrix
let submatrix mat row col =
  mat
  |> List.mapi (fun i r -> 
                  if i <> row then 
                    r |> List.filteri (fun j _ -> j <> col) 
                  else [] ) 
  |> List.filter (fun r -> r <> [])
(* Aliases for type-spec operations *)
let submatrix_n = submatrix
let submatrix_f = submatrix

let rec determinant_n mat =
  let (rows, cols) = mat_dim mat in
  
  (* Check if square matrix *)
  if rows <> cols then
    raise (Dimension_Mismatch "Determinant requires square matrix")
  else 
    match rows with
    | 0 -> raise (Dimension_Mismatch "Empty matrix has no determinant")
    | 1 -> List.hd (List.hd mat) (* 1x1 matrix *)
    | 2 -> (match mat with
           |[[a; b]; [c; d]] -> a * d - b * c
           | _ -> raise(Dimension_Mismatch("Expected Two By Two Matrix in the case"))
           )
    | n ->
        (* For larger matrices, use cofactor expansion along first row *)
        let first_row = List.hd mat in
        
        (* Calculate determinant using cofactor expansion *)
        List.mapi (fun j element ->
          let sign = if j mod 2 = 0 then 1 else -1 in
          let sub = submatrix mat 0 j in
          sign * element * determinant_n sub
        ) first_row
        |> List.fold_left (+) 0

(* Determinant calculation for float matrices *)
let rec determinant_f mat =
  let (rows, cols) = mat_dim mat in
  
  (* Check if square matrix *)
  if rows <> cols then
    raise (Dimension_Mismatch "Determinant requires square matrix")
  else 
    match rows with
    | 0 -> raise (Dimension_Mismatch "Empty matrix has no determinant")
    | 1 -> List.hd (List.hd mat) (* 1x1 matrix *)
    | 2 -> (match mat with
              |[[a; b]; [c; d]] -> a *. d -. b *. c
              | _ -> raise(Dimension_Mismatch("Expected Two By Two Matrix in the case"))
          )
    | n ->
        (* For larger matrices, use cofactor expansion along first row *)
        let first_row = List.hd mat in
        
        (* Calculate determinant using cofactor expansion *)
        List.mapi (fun j element ->
          let sign = if j mod 2 = 0 then 1.0 else -1.0 in
          let sub = submatrix mat 0 j in
          sign *. element *. determinant_f sub
        ) first_row
        |> List.fold_left (+.) 0.0

(* Calculate the cofactor of a matrix element mat[i][j] *)
let cofactor_n mat i j =
  let sub = submatrix mat i j in
  let det = determinant_n sub in
  if (i + j) mod 2 = 0 then det else -1 * det

let cofactor_f mat i j =
  let sub = submatrix mat i j in
  let det = determinant_f sub in
  if (i + j) mod 2 = 0 then det else -1.0 *. det

(* Calculate the adjoint of a matrix *)
let adjoint_n mat =
  let n = List.length mat in
  List.init n (fun i ->
    List.init n (fun j ->
      cofactor_n mat j i  (* Note: j,i instead of i,j for transpose *)
    )
  )

let adjoint_f mat =
  let n = List.length mat in
  List.init n (fun i ->
    List.init n (fun j ->
      cofactor_f mat j i  (* Note: j,i instead of i,j for transpose *)
    )
  )

(* Calculate inverse of a matrix using cofactor method *)
let inverse_matrix_n mat =
  let n = List.length mat in
  if n <> List.length (List.hd mat) then
    raise (Dimension_Mismatch "Inverse requires square matrix")
  else 
    let det = determinant_n mat in
    if det = 0 then
      raise (Division_by_zero "Matrix is not invertible (determinant is zero)")
    else
      let adj = adjoint_n mat in
      List.map (fun row ->
        List.map (fun elem -> float_of_int elem /. float_of_int det) row
      ) adj

let inverse_matrix_f mat =
  let n = List.length mat in
  if n <> List.length (List.hd mat) then
    raise (Dimension_Mismatch "Inverse requires square matrix")
  else 
    let det = determinant_f mat in
    if det = 0.0 then
      raise (Division_by_zero "Matrix is not invertible (determinant is zero)")
    else
      let adj = adjoint_f mat in
      List.map (fun row ->
        List.map (fun elem -> elem /. det) row
      ) adj      

(* Helper function to print an int matrix *)
let print_int_matrix mat =
  List.iter (fun row ->
    List.iter (fun elem -> Printf.printf "%d\t" elem) row;
    print_newline ()
  ) mat;
  print_newline ()

(* Helper function to print a float matrix *)
let print_float_matrix mat =
  List.iter (fun row ->
    List.iter (fun elem -> Printf.printf "%.2f\t" elem) row;
    print_newline ()
  ) mat;
  print_newline ()

(* Test cases for matrix operations *)
let () =
  (* Test transpose with various matrix sizes *)
  print_endline "===== TRANSPOSE TESTS =====";
  
  (* 2x2 matrix transpose *)
  let m2x2 = [[1; 2]; [3; 4]] in
  print_endline "Original 2x2 matrix:";
  print_int_matrix m2x2;
  
  let m2x2_t = transpose_matrix m2x2 in
  print_endline "Transposed 2x2 matrix:";
  print_int_matrix m2x2_t;
  
  (* 3x3 matrix transpose *)
  let m3x3 = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] in
  print_endline "Original 3x3 matrix:";
  print_int_matrix m3x3;
  
  let m3x3_t = transpose_matrix m3x3 in
  print_endline "Transposed 3x3 matrix:";
  print_int_matrix m3x3_t;
  
  (* 3x2 non-square matrix transpose *)
  let m3x2 = [[1; 2]; [3; 4]; [5; 6]] in
  print_endline "Original 3x2 matrix:";
  print_int_matrix m3x2;
  
  let m3x2_t = transpose_matrix m3x2 in
  print_endline "Transposed 3x2 matrix (should be 2x3):";
  print_int_matrix m3x2_t;
  
  (* 4x4 matrix transpose *)
  let m4x4 = [[1; 2; 3; 4]; [5; 6; 7; 8]; [9; 10; 11; 12]; [13; 14; 15; 16]] in
  print_endline "Original 4x4 matrix:";
  print_int_matrix m4x4;
  
  let m4x4_t = transpose_matrix m4x4 in
  print_endline "Transposed 4x4 matrix:";
  print_int_matrix m4x4_t;
  
  (* 5x5 matrix transpose *)
  let m5x5 = [
    [1; 2; 3; 4; 5]; 
    [6; 7; 8; 9; 10]; 
    [11; 12; 13; 14; 15]; 
    [16; 17; 18; 19; 20];
    [21; 22; 23; 24; 25]
  ] in
  print_endline "Original 5x5 matrix:";
  print_int_matrix m5x5;
  
  let m5x5_t = transpose_matrix m5x5 in
  print_endline "Transposed 5x5 matrix:";
  print_int_matrix m5x5_t;
  
  (* Test determinant calculation *)
  print_endline "\n===== DETERMINANT TESTS =====";
  
  (* 1x1 matrix determinant *)
  let m1x1 = [[5]] in
  print_endline "1x1 matrix:";
  print_int_matrix m1x1;
  
  let det_1x1 = determinant_n m1x1 in
  Printf.printf "Determinant: %d\n\n" det_1x1;
  
  (* 2x2 matrix determinant *)
  print_endline "2x2 matrix:";
  print_int_matrix m2x2;
  
  let det_2x2 = determinant_n m2x2 in
  Printf.printf "Determinant: %d\n\n" det_2x2;
  
  (* 3x3 matrix determinant *)
  print_endline "3x3 matrix:";
  print_int_matrix m3x3;
  
  let det_3x3 = determinant_n m3x3 in
  Printf.printf "Determinant: %d\n\n" det_3x3;
  
  (* 3x3 matrix with non-zero determinant *)
  let m3x3_nonzero = [[2; 3; 1]; [4; 1; 3]; [1; 2; 5]] in
  print_endline "3x3 matrix with non-zero determinant:";
  print_int_matrix m3x3_nonzero;
  
  let det_3x3_nonzero = determinant_n m3x3_nonzero in
  Printf.printf "Determinant: %d\n\n" det_3x3_nonzero;
  
  (* 4x4 matrix determinant *)
  print_endline "4x4 matrix:";
  print_int_matrix m4x4;
  
  let det_4x4 = determinant_n m4x4 in
  Printf.printf "Determinant: %d\n\n" det_4x4;
  
  (* 4x4 matrix with non-zero determinant *)
  let m4x4_nonzero = [
    [1; 3; 5; 9]; 
    [1; 3; 1; 7]; 
    [4; 3; 9; 7]; 
    [5; 2; 0; 9]
  ] in
  print_endline "4x4 matrix with non-zero determinant:";
  print_int_matrix m4x4_nonzero;
  
  let det_4x4_nonzero = determinant_n m4x4_nonzero in
  Printf.printf "Determinant: %d\n\n" det_4x4_nonzero;
  
  (* Test matrix inverse *)
  print_endline "\n===== MATRIX INVERSE TESTS =====";
  
  (* 2x2 matrix inverse *)
  let m2x2_inv = [[4; 7]; [2; 6]] in
  print_endline "Original 2x2 matrix:";
  print_int_matrix m2x2_inv;
  
  let inv_2x2 = inverse_matrix_n m2x2_inv in
  print_endline "Inverse matrix:";
  print_float_matrix inv_2x2;
  
  (* 3x3 matrix inverse *)
  let m3x3_inv = [[1; 2; 3]; [0; 1; 4]; [5; 6; 0]] in
  print_endline "Original 3x3 matrix:";
  print_int_matrix m3x3_inv;
  
  let inv_3x3 = inverse_matrix_n m3x3_inv in
  print_endline "Inverse matrix:";
  print_float_matrix inv_3x3;
  
  (* Test error cases *)
  print_endline "\n===== ERROR HANDLING TESTS =====";
  
  (* Non-square matrix for determinant *)
  print_endline "Testing determinant of non-square matrix:";
  print_int_matrix m3x2;
  
  try
    let _ = determinant_n m3x2 in
    print_endline "Error: Should have failed on non-square matrix"
  with Dimension_Mismatch msg ->
    Printf.printf "Correctly caught error: %s\n\n" msg;
  
  (* Singular matrix (determinant = 0) for inverse *)
  let singular_matrix = [[1; 2]; [2; 4]] in
  print_endline "Testing inverse of singular matrix:";
  print_int_matrix singular_matrix;
  
  try
    let _ = inverse_matrix_n singular_matrix in
    print_endline "Error: Should have failed on singular matrix"
  with Division_by_zero msg ->
    Printf.printf "Correctly caught error: %s\n\n" msg;
  
  (* Float matrix operations *)
  print_endline "\n===== FLOAT MATRIX OPERATIONS =====";
  
  (* Float matrix transpose *)
  let mf_2x2 = [[1.5; 2.3]; [3.7; 4.1]] in
  print_endline "Original float matrix:";
  print_float_matrix mf_2x2;
  
  let mf_2x2_t = transpose_matrix mf_2x2 in
  print_endline "Transposed float matrix:";
  print_float_matrix mf_2x2_t;
  
  (* Float matrix determinant *)
  let det_f = determinant_f mf_2x2 in
  Printf.printf "Determinant of float matrix: %.2f\n\n" det_f;
  
  (* Float matrix inverse *)
  let inv_f = inverse_matrix_f mf_2x2 in
  print_endline "Inverse of float matrix:";
  print_float_matrix inv_f;
