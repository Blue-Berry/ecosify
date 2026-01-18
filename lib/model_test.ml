open! Core
open Ecosify

let%expect_test "simepl atom expr" =
  let open Model in
  let open Infix in
  let x1 = Var (Var.Atom 1) in
  let x2 = Var (Var.Atom 2) in
  let x3 = Var (Var.Atom 3) in
  let l = Const 10. in
  let a = x1 + x2 + x3 == l in
  Model.sexp_of_constr_packed a |> print_s;
  [%expect
    {|
    (Constr_atom
     (Eq (Add (Add (Var (Atom 1)) (Var (Atom 2))) (Var (Atom 3))) (Const 10)))
    |}]
;;

let%expect_test "simepl atom expr 2" =
  let open Model in
  let ws = new_ws () in
  let x1 = variable ws in
  let x2 = variable ws in
  let x3 = variable ws in
  let l = Const 10. in
  let open Infix in
  let a = x1 + x2 + x3 == l in
  Model.sexp_of_constr_packed a |> print_s;
  [%expect
    {|
    (Constr_atom
     (Eq (Add (Add (Var (Atom 1)) (Var (Atom 2))) (Var (Atom 3))) (Const 10)))
    |}]
;;

let%expect_test "simepl vec expr" =
  let open Model in
  let ws = new_ws () in
  let x1 = variables ws 10 in
  let x2 = variables ws 10 in
  let x3 = variables ws 10 in
  let g = const_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in
  let open Infix in
  let a = x1 + x2 + x3 == g in
  Model.sexp_of_constr_packed a |> print_s;
  [%expect
    {|
    (Constr_vec
     (Eq
      (Add
       (Add
        (Var
         (Vec
          ((Atom 1) (Atom 2) (Atom 3) (Atom 4) (Atom 5) (Atom 6) (Atom 7)
           (Atom 8) (Atom 9) (Atom 10))))
        (Var
         (Vec
          ((Atom 11) (Atom 12) (Atom 13) (Atom 14) (Atom 15) (Atom 16) (Atom 17)
           (Atom 18) (Atom 19) (Atom 20)))))
       (Var
        (Vec
         ((Atom 21) (Atom 22) (Atom 23) (Atom 24) (Atom 25) (Atom 26) (Atom 27)
          (Atom 28) (Atom 29) (Atom 30)))))
      (Data (1 2 3 4 5 6 7 8 9 10))))
    |}]
;;

let%expect_test "simepl vec expr" =
  let open Model in
  let ws = new_ws () in
  let x1 = variables ws 10 in
  let x2 = variables ws 10 in
  let x3 = variables ws 10 in
  let g = const_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in
  let open Infix in
  let a = 3. * ((2. * x1) + x2 + x3) == g in
  Model.sexp_of_constr_packed a |> print_s;
  [%expect
    {|
    (Constr_vec
     (Eq
      (Mul 3
       (Add
        (Add
         (Mul 2
          (Var
           (Vec
            ((Atom 1) (Atom 2) (Atom 3) (Atom 4) (Atom 5) (Atom 6) (Atom 7)
             (Atom 8) (Atom 9) (Atom 10)))))
         (Var
          (Vec
           ((Atom 11) (Atom 12) (Atom 13) (Atom 14) (Atom 15) (Atom 16) (Atom 17)
            (Atom 18) (Atom 19) (Atom 20)))))
        (Var
         (Vec
          ((Atom 21) (Atom 22) (Atom 23) (Atom 24) (Atom 25) (Atom 26) (Atom 27)
           (Atom 28) (Atom 29) (Atom 30))))))
      (Data (1 2 3 4 5 6 7 8 9 10))))
    |}]
;;

let%expect_test "linearised equality" =
  let open Model in
  let ws = new_ws () in
  let x1 = variable ws in
  let x2 = variable ws in
  let x3 = variable ws in
  let l = Const 10. in
  let open Infix in
  let a = x1 + (2. * (x2 + x3)) + (2. * x1) == l in
  Eval_constr.eval_constr a |> List.sexp_of_t Eval_constr.sexp_of_t |> print_s;
  [%expect {| ((Equality (((3 2) (2 2) (1 3)) 10))) |}]
;;

let%expect_test "linearised inequality" =
  let open Model in
  let ws = new_ws () in
  let x1 = variable ws in
  let x2 = variable ws in
  let x3 = variable ws in
  let l = Const 10. in
  let open Infix in
  let a = x1 + (2. * (x2 + x3)) + (2. * x1) <= l in
  Eval_constr.eval_constr a |> List.sexp_of_t Eval_constr.sexp_of_t |> print_s;
  [%expect {| ((Inequality_lt (((3 2) (2 2) (1 3)) 10))) |}]
;;

let%expect_test "linearised inequality 2" =
  let open Model in
  let ws = new_ws () in
  let x1 = variable ws in
  let x2 = variable ws in
  let x3 = variable ws in
  let l = Const 10. in
  let open Infix in
  let a = x1 + (2. * (x2 + x3)) + (2. * x1) >= l in
  Eval_constr.eval_constr a |> List.sexp_of_t Eval_constr.sexp_of_t |> print_s;
  [%expect {| ((Inequality_lt (((3 -2) (2 -2) (1 -3)) -10))) |}]
;;

let%expect_test "linearised inequality 2" =
  let open Model in
  let ws = new_ws () in
  let x1 = variables ws 10 in
  let x2 = variables ws 10 in
  let x3 = variables ws 10 in
  let g = const_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in
  let open Infix in
  let a = 3. * ((2. * x1) + x2 + x3) == g in
  Eval_constr.eval_constr a |> List.sexp_of_t Eval_constr.sexp_of_t |> print_s;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Invalid_argument "length mismatch in Array.map2_exn: 0 <> 10")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Base__Array.map2_exn in file "src/array.ml", line 571, characters 2-42
  Called from Ecosify__Model.Eval_constr.Vec.merge in file "lib/model.ml", line 229, characters 23-76
  Called from Ecosify__Model.Eval_constr.Vec.sub in file "lib/model.ml" (inlined), line 233, characters 18-39
  Called from Ecosify__Model.Eval_constr.Vec.eval_constr in file "lib/model.ml", line 258, characters 21-34
  Called from Ecosify_test__Model_test.(fun) in file "lib/model_test.ml", line 151, characters 2-27
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;
