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
     (Eq (Add (Add (Var (Atom 1)) (Var (Atom 2))) (Var (Atom 3)))
      (Const Atom_wit 10)))
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
     (Eq (Add (Add (Var (Atom 1)) (Var (Atom 2))) (Var (Atom 3)))
      (Const Atom_wit 10)))
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
      (Const Vec_wit 10)))
    |}]
;;

let%expect_test "simepl vec expr" =
  let open Model in
  let ws = new_ws () in
  let x1 = variables ws 10 in
  let x2 = variables ws 10 in
  let x3 = variables ws 10 in
  let g = const_of_list [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ] in
  let l = Const (Var.Vec_wit, 10.) in
  let open Infix in
  let a = x1 + x2 + x3 == l in
  Model.sexp_of_constr_packed a |> print_s;
  [%expect
    {|
    (Constr_vec
     (Eq
      (Add
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
       (Data (1 2 3 4 5 6 7 8 9 10)))
      (Const Vec_wit 10)))
    |}]
;;
