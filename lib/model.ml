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

  type _ witness =
    | Atom_wit : atom witness
    | Vec_wit : vec witness

  let witness_of_t : type a. a t -> a witness = function
    | Atom _ -> Atom_wit
    | Vec _ -> Vec_wit
  ;;

  let rec sexp_of_t : type a. a t -> Sexp.t = function
    | Atom id -> Int.sexp_of_t id
    | Vec atoms -> Array.sexp_of_t sexp_of_t atoms
  ;;

  let ( .%{} ) (Vec ts : vec t) i = ts.(i)
  let ( .%{}<- ) (Vec ts : vec t) i v = Array.set ts i v

  let size : type a. a t -> int = function
    | Atom _ -> 1
    | Vec ts -> Array.length ts
  ;;
end

type 'a expr =
  | Const of 'a Var.witness * float
  | Var of 'a Var.t
  | Add of 'a expr * 'a expr
  | Sub of 'a expr * 'a expr
  | Mul of float * 'a expr

type 'a constr =
  | Eq of 'a expr * 'a expr
  | Le of 'a expr * 'a expr
  | Ge of 'a expr * 'a expr

type constr_packed =
  | Constr_vec : Var.vec constr -> constr_packed
  | Constr_atom : Var.atom constr -> constr_packed

let rec witness_of_expr : type a. a expr -> a Var.witness = function
  | Const (w, _) -> w
  | Var e -> Var.witness_of_t e
  | Add (l, _) -> witness_of_expr l
  | Sub (l, _) -> witness_of_expr l
  | Mul (_, r) -> witness_of_expr r
;;

let pack_contr : type a. a Var.witness -> a constr -> constr_packed =
  fun witness constr ->
  match witness with
  | Atom_wit -> Constr_atom constr
  | Vec_wit -> Constr_vec constr
;;

module Eval = struct
  let ( == ) : type a. a expr -> a expr -> constr_packed =
    fun l r -> pack_contr (witness_of_expr l) (Eq (l, r))
  ;;

  let ( <= ) : type a. a expr -> a expr -> constr_packed =
    fun l r -> pack_contr (witness_of_expr l) (Le (l, r))
  ;;

  let ( >= ) : type a. a expr -> a expr -> constr_packed =
    fun l r -> pack_contr (witness_of_expr l) (Ge (l, r))
  ;;

  let ( + ) : type a. a expr -> a expr -> a expr = fun l r -> Add (l, r)
  let ( - ) : type a. a expr -> a expr -> a expr = fun l r -> Sub (l, r)
  let ( * ) : type a. float -> a expr -> a expr = fun l r -> Mul (l, r)
end

type ws =
  { vars : int ref
  ; mutable constraints : constr_packed list
  }

let variable ws =
  incr ws.vars;
  Var.Atom !(ws.vars)
;;

let variables ws size =
  Var.Vec
    (Array.init size ~f:(fun _ ->
       incr ws.vars;
       Var.Atom !(ws.vars)))
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
