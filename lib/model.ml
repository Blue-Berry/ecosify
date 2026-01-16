open! Core

(*What I want:
             let grid = decision_variable(size, cost)
             let gen = decision_variable(size, cost)
             let pv = decision_variable(size, cost)
             let bat_chrg = decision_variable(size, cost)
             let bat_dis = decision_variable(size, cost)
             let bat_soc = decision_variable(size, cost)
             let load = const(size, data)
             add_ineq (gridadd_ineq>= 0);
             add_ineq (gen >= 0);
             add_ineq (pv >= 0);
             add_expr ( grid + gen + pv + bat_dis - bat_chrg = load);
             for t = 1 to 24 do
               add_expr ( soc[t+1] = soc[t]+ bat_chrg[t]*dt - bat_dis[t]*dt);
             done;

ideas infix operators for >=, +, -, = etc
decision variables are lists
 *)
module Var = struct
  type vec
  type atom
  type matrix
  type var_id = int

  (* Operations should work for individual values and vector of values *)
  type _ t =
    | Atom : var_id -> atom t
    | Vec : atom t array -> vec t
  (* | Matrix : atom t array array -> matrix t *)

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Atom id -> Int.sexp_of_t id
    | Vec atoms -> Array.sexp_of_t sexp_of_t atoms
  ;;

  let ( .%{} ) (Vec ts : vec t) i = ts.(i)
  let ( .%{}<- ) (Vec ts : vec t) i v = Array.set ts i v
end

type expr =
  | Const of float
  | Var of Var.atom Var.t
  | Var_vec of Var.vec Var.t
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of float * expr

type constr =
  | Eq of expr * expr
  | Le of expr * expr
  | Ge of expr * expr

let next_id =
  let r = ref 0 in
  fun () ->
    incr r;
    !r
;;

(* module M = struct *)
(*   type ineq *)

(*   type t = *)
(*     { decision_variables : int *)
(*     ; eq : Equality.t list *)
(*     ; ineq : ineq list *)
(*     } *)

(*   let decision_variable (t : t) (size : int) = *)
(*     ( { t with decision_variables = t.decision_variables + size } *)
(*     , Array.init size ~f:(fun i -> t.decision_variables + i) ) *)
(*   ;; *)

(*   let add_eq (t : t) (eq : Equality.t) = { t with eq = eq :: t.eq } *)
(*   let add_ineq (t : t) (ineq : ineq) = { t with ineq = ineq :: t.ineq } *)
(* end *)
