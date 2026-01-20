open! Core
open Ecosify

(* let%expect_test "simepl atom expr 2" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variable ws in *)
(*   let x2 = variable ws in *)
(*   let x3 = variable ws in *)
(*   let l = const 10. in *)
(*   let open Infix in *)
(*   let a = x1 + x2 + x3 == l in *)
(*   Model.sexp_of_affine_expr a |> print_s; *)
(*   [%expect *)
(*     {| *)
(*     (Constr_atom *)
(*      (Eq (Add (Add (Var (Atom 1)) (Var (Atom 2))) (Var (Atom 3))) (Const 10))) *)
(*     |}] *)
(* ;; *)

(* let%expect_test "simepl vec expr" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variables ws 10 in *)
(*   let x2 = variables ws 10 in *)
(*   let x3 = variables ws 10 in *)
(*   let g = consts_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in *)
(*   let open Infix in *)
(*   let a = x1 + x2 + x3 == g in *)
(*   Model.sexp_of_affine_expr a |> print_s; *)
(*   [%expect *)
(*     {| *)
(*     (Constr_vec *)
(*      (Eq *)
(*       (Add *)
(*        (Add *)
(*         (Var *)
(*          (Vec *)
(*           ((Atom 1) (Atom 2) (Atom 3) (Atom 4) (Atom 5) (Atom 6) (Atom 7) *)
(*            (Atom 8) (Atom 9) (Atom 10)))) *)
(*         (Var *)
(*          (Vec *)
(*           ((Atom 11) (Atom 12) (Atom 13) (Atom 14) (Atom 15) (Atom 16) (Atom 17) *)
(*            (Atom 18) (Atom 19) (Atom 20))))) *)
(*        (Var *)
(*         (Vec *)
(*          ((Atom 21) (Atom 22) (Atom 23) (Atom 24) (Atom 25) (Atom 26) (Atom 27) *)
(*           (Atom 28) (Atom 29) (Atom 30))))) *)
(*       (Data (1 2 3 4 5 6 7 8 9 10)))) *)
(*     |}] *)
(* ;; *)

(* let%expect_test "simepl vec expr" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variables ws 10 in *)
(*   let x2 = variables ws 10 in *)
(*   let x3 = variables ws 10 in *)
(*   let g = consts_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in *)
(*   let open Infix in *)
(*   let a = 3. * ((2. * x1) + x2 + x3) == g in *)
(*   Model.sexp_of_affine_expr a |> print_s; *)
(*   [%expect *)
(*     {| *)
(*     (Constr_vec *)
(*      (Eq *)
(*       (Mul 3 *)
(*        (Add *)
(*         (Add *)
(*          (Mul 2 *)
(*           (Var *)
(*            (Vec *)
(*             ((Atom 1) (Atom 2) (Atom 3) (Atom 4) (Atom 5) (Atom 6) (Atom 7) *)
(*              (Atom 8) (Atom 9) (Atom 10))))) *)
(*          (Var *)
(*           (Vec *)
(*            ((Atom 11) (Atom 12) (Atom 13) (Atom 14) (Atom 15) (Atom 16) (Atom 17) *)
(*             (Atom 18) (Atom 19) (Atom 20))))) *)
(*         (Var *)
(*          (Vec *)
(*           ((Atom 21) (Atom 22) (Atom 23) (Atom 24) (Atom 25) (Atom 26) (Atom 27) *)
(*            (Atom 28) (Atom 29) (Atom 30)))))) *)
(*       (Data (1 2 3 4 5 6 7 8 9 10)))) *)
(*     |}] *)
(* ;; *)

(* let%expect_test "linearised equality" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variable ws in *)
(*   let x2 = variable ws in *)
(*   let x3 = variable ws in *)
(*   let l = const 10. in *)
(*   let open Infix in *)
(*   let a = x1 + (2. * (x2 + x3)) + (2. * x1) == l in *)
(*   Eval.of_affine_expr ws a |> List.sexp_of_t Eval.sexp_of_t |> print_s; *)
(*   [%expect {| ((Equality (((3 2) (2 2) (1 3)) 10))) |}] *)
(* ;; *)

(* let%expect_test "linearised equality" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variable ws in *)
(*   let x2 = variable ws in *)
(*   let x3 = variable ws in *)
(*   let open Infix in *)
(*   let a = x1 + (2. * x3) + (2. * x1) == x2 in *)
(*   Eval.of_affine_expr ws a |> List.sexp_of_t Eval.sexp_of_t |> print_s; *)
(*   [%expect {| ((Equality (((3 2) (2 -1) (1 3)) -0))) |}] *)
(* ;; *)

(* let%expect_test "linearised inequality" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variable ws in *)
(*   let x2 = variable ws in *)
(*   let x3 = variable ws in *)
(*   let l = const 10. in *)
(*   let open Infix in *)
(*   let a = x1 + (2. * (x2 + x3)) + (2. * x1) <= l in *)
(*   Eval.of_affine_expr ws a |> List.sexp_of_t Eval.sexp_of_t |> print_s; *)
(*   [%expect {| ((Inequality_le (((3 2) (2 2) (1 3)) 10))) |}] *)
(* ;; *)

(* let%expect_test "linearised inequality 2" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variable ws in *)
(*   let x2 = variable ws in *)
(*   let x3 = variable ws in *)
(*   let l = Const 10. in *)
(*   let open Infix in *)
(*   let a = x1 + (2. * (x2 + x3)) + (2. * x1) >= l in *)
(*   Eval.of_affine_expr ws a |> List.sexp_of_t Eval.sexp_of_t |> print_s; *)
(*   [%expect {| ((Inequality_le (((3 -2) (2 -2) (1 -3)) -10))) |}] *)
(* ;; *)

(* let%expect_test "linearised equality 2" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variables ws 9 in *)
(*   let x2 = variables ws 10 in *)
(*   let x3 = variables ws 10 in *)
(*   let g = const_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in *)
(*   let open Infix in *)
(*   let a = 3. * ((2. * x1) + x2 + x3) == g in *)
(*   Eval.of_affine_expr ws a |> List.sexp_of_t Eval.sexp_of_t |> print_s; *)
(*   [%expect *)
(*     {| *)
(*     ((Equality (((20 3) (10 3) (1 6)) 1)) (Equality (((21 3) (11 3) (2 6)) 2)) *)
(*      (Equality (((22 3) (12 3) (3 6)) 3)) (Equality (((23 3) (13 3) (4 6)) 4)) *)
(*      (Equality (((24 3) (14 3) (5 6)) 5)) (Equality (((25 3) (15 3) (6 6)) 6)) *)
(*      (Equality (((26 3) (16 3) (7 6)) 7)) (Equality (((27 3) (17 3) (8 6)) 8)) *)
(*      (Equality (((28 3) (18 3) (9 6)) 9)) (Equality (((29 3) (19 3) (0 0)) 10))) *)
(*     |}] *)
(* ;; *)

(* let%expect_test "linearised equality 3" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variables ws 10 in *)
(*   let x2 = variables ws 10 in *)
(*   let x3 = variables ws 10 in *)
(*   let open Infix in *)
(*   let a = 3. * ((2. * x1) + x3) == x2 in *)
(*   Eval.of_affine_expr ws a |> List.sexp_of_t Eval.sexp_of_t |> print_s; *)
(*   [%expect *)
(*     {| *)
(*     ((Equality (((21 3) (11 -1) (1 6)) 0)) (Equality (((22 3) (12 -1) (2 6)) 0)) *)
(*      (Equality (((23 3) (13 -1) (3 6)) 0)) (Equality (((24 3) (14 -1) (4 6)) 0)) *)
(*      (Equality (((25 3) (15 -1) (5 6)) 0)) (Equality (((26 3) (16 -1) (6 6)) 0)) *)
(*      (Equality (((27 3) (17 -1) (7 6)) 0)) (Equality (((28 3) (18 -1) (8 6)) 0)) *)
(*      (Equality (((29 3) (19 -1) (9 6)) 0)) *)
(*      (Equality (((30 3) (20 -1) (10 6)) 0))) *)
(*     |}] *)
(* ;; *)

let%expect_test "create vec matrices" =
  let open Model in
  let ws = new_ws () in
  let x1 = variables ws 10 in
  let x2 = variables ws 10 in
  let x3 = variables ws 10 in
  let open Infix in
  constr ws (3. * ((2. * x1) + x3) == x2);
  cost ws (x2 - x1 - x3);
  (* constr ws (x1 <= const 5.); *)
  get_params ws |> sexp_of_ecos_params |> print_s;
  [%expect
    {|
    ((n 30) (m 0) (p 10) (l 0) (ncones 0) (q ()) (e 0) (g ())
     (a
      ((0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3)
       (0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0)
       (0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0)
       (0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0)
       (0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0 0)
       (0 0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0)
       (0 0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0)
       (0 0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
       (0 6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0)
       (6 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0)))
     (h ()) (b (0 0 0 0 0 0 0 0 0 0))
     (c (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    |}]
;;

(* let%expect_test "matrices" = *)
(*   let open Model in *)
(*   let ws = new_ws () in *)
(*   let x1 = variable ws in *)
(*   let x2 = variable ws in *)
(*   let x3 = variable ws in *)
(*   let l = Const 10. in *)
(*   let open Infix in *)
(*   let a = x1 + (2. * (x2 + x3)) + (2. * x1) >= l in *)
(*   Eval.of_affine_expr ws a |> Constr_Set.create ws |> Constr_Set.sexp_of_t |> print_s; *)
(*   [%expect {| ((a ()) (b ()) (g ((-3 -2 -2))) (h (-10)) (c (0 0 0))) |}] *)
(* ;; *)

let%expect_test "Cost" =
  let open Model in
  let ws = new_ws () in
  let x1 = variable ws in
  let x2 = variable ws in
  let x3 = variable ws in
  let open Infix in
  cost ws (x1 + x2 + x3);
  get_params ws |> sexp_of_ecos_params |> print_s;
  [%expect
    {|
    ((n 3) (m 0) (p 0) (l 0) (ncones 0) (q ()) (e 0) (g ()) (a ()) (h ())
     (b ()) (c (1 1 1)))
    |}]
;;

let%expect_test "create vec matrices" =
  let open Model in
  let ws = new_ws () in
  let x1 = variables ws 10 in
  let x2 = variables ws 10 in
  let x3 = variables ws 10 in
  let open Infix in
  cost ws (x1 - x2 + x3);
  get_params ws |> sexp_of_ecos_params |> print_s;
  [%expect
    {|
    ((n 30) (m 0) (p 0) (l 0) (ncones 0) (q ()) (e 0) (g ()) (a ()) (h ())
     (b ())
     (c (1 1 1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 1 1 1 1)))
    |}]
;;

let%expect_test "params" =
  let open Model in
  let ws = new_ws () in
  let x1 = variable ws in
  let x2 = variable ws in
  let x3 = variable ws in
  let open Infix in
  cost ws (x1 + x2 + x3);
  constr ws (x1 + x2 == const 1.);
  constr ws (x1 <= const 2.);
  constr ws (x1 >= const 4.);
  get_params ws |> sexp_of_ecos_params |> print_s;
  [%expect
    {|
    ((n 3) (m 2) (p 1) (l 2) (ncones 0) (q ()) (e 0) (g ((1 0 0) (-1 0 0)))
     (a ((1 1 0))) (h (2 -4)) (b (1)) (c (1 1 1)))
    |}]
;;
