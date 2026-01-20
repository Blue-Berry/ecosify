module Var : sig
  type 'a t
  type atom
  type vec
end

type 'a expr

val sexp_of_expr : ('a -> Sexplib0.Sexp.t) -> 'a expr -> Sexplib0.Sexp.t

type 'a constr

val sexp_of_constr : ('a -> Sexplib0.Sexp.t) -> 'a constr -> Sexplib0.Sexp.t

type affine_expr

val sexp_of_affine_expr : affine_expr -> Sexplib0.Sexp.t

module Infix : sig
  val ( == ) : 'a expr -> 'a expr -> affine_expr
  val ( <= ) : 'a expr -> 'a expr -> affine_expr
  val ( >= ) : 'a expr -> 'a expr -> affine_expr
  val ( + ) : 'a expr -> 'a expr -> 'a expr
  val ( - ) : 'a expr -> 'a expr -> 'a expr
  val ( * ) : float -> 'a expr -> 'a expr
  val ( .%{} ) : Core.vec Var.t expr -> int -> Var.atom Var.t
  val ( .%{}<- ) : Core.vec Var.t expr -> int -> Var.atom Var.t -> unit
end

type ws =
  { vars : int ref
  ; mutable affine_exprs : affine_expr list
  }

val new_ws : unit -> ws
val variable : ws -> Var.atom expr
val variables : ws -> int -> Var.vec expr
val add_cost : ws -> 'a expr -> unit
val add_constr : ws -> affine_expr -> unit
val consts_of_list : float list -> Var.vec expr

module Eval : sig
  type coeff
  type row
  type t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val of_affine_expr : ws -> affine_expr -> t list
end

module Constr_Set : sig
  type a

  val sexp_of_a : a -> Sexplib0.Sexp.t

  type b

  val sexp_of_b : b -> Sexplib0.Sexp.t

  type g

  val sexp_of_g : g -> Sexplib0.Sexp.t

  type h

  val sexp_of_h : h -> Sexplib0.Sexp.t

  type c

  val sexp_of_c : c -> Sexplib0.Sexp.t

  type t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val create_matrix : ws -> Eval.coeff list list -> float array array
  val create_cost : ws -> Eval.coeff list -> c
  val create : ws -> Eval.t list -> t
end

type ecos_params =
  { n : int
  ; m : int
  ; p : int
  ; l : int
  ; ncones : int
  ; q : int array
  ; e : int
  ; g : Constr_Set.g
  ; a : Constr_Set.a
  ; h : Constr_Set.h
  ; b : Constr_Set.b
  ; c : Constr_Set.c
  }

val sexp_of_ecos_params : ecos_params -> Sexplib0.Sexp.t
val get_params : ws -> ecos_params
