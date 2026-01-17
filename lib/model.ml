open! Core

module Var = struct
  type vec [@@deriving sexp_of]
  type atom [@@deriving sexp_of]
  type var_id = int [@@deriving sexp_of]

  type _ t =
    | Atom : var_id -> atom t
    | Vec : atom t array -> vec t
  [@@deriving sexp_of]

  type _ witness =
    | Atom_wit : atom witness
    | Vec_wit : vec witness
  [@@deriving sexp_of]

  let witness_of_t : type a. a t -> a witness = function
    | Atom _ -> Atom_wit
    | Vec _ -> Vec_wit
  ;;

  let size : type a. a t -> int = function
    | Atom _ -> 1
    | Vec ts -> Array.length ts
  ;;
end

type 'a expr =
  | Const : float -> Var.atom expr
  | Var : 'a Var.t -> 'a expr
  | Add : 'a expr * 'a expr -> 'a expr
  | Sub : 'a expr * 'a expr -> 'a expr
  | Mul : float * 'a expr -> 'a expr
  | Data : float array -> Var.vec expr
[@@deriving sexp_of]

type 'a constr =
  | Eq of 'a expr * 'a expr
  | Le of 'a expr * 'a expr
  | Ge of 'a expr * 'a expr
[@@deriving sexp_of]

type constr_packed =
  | Constr_vec : Var.vec constr -> constr_packed
  | Constr_atom : Var.atom constr -> constr_packed
[@@deriving sexp_of]

let rec witness_of_expr : type a. a expr -> a Var.witness = function
  | Const _ -> Var.Atom_wit
  | Var e -> Var.witness_of_t e
  | Add (l, _) -> witness_of_expr l
  | Sub (l, _) -> witness_of_expr l
  | Mul (_, r) -> witness_of_expr r
  | Data _ -> Var.Vec_wit
;;

let pack_contr : type a. a Var.witness -> a constr -> constr_packed =
  fun witness constr ->
  match witness with
  | Atom_wit -> Constr_atom constr
  | Vec_wit -> Constr_vec constr
;;

module Infix = struct
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

  let ( .%{} ) (expr : vec Var.t expr) i =
    match expr with
    | Var (Vec x) -> x.(i)
    | _ -> failwith "Can't index non variable expressions"
  ;;

  let ( .%{}<- ) (expr : vec Var.t expr) i v =
    match expr with
    | Var (Vec x) -> Array.set x i v
    | _ -> failwith "Can't index non variable expressions"
  ;;
end

type ws =
  { vars : int ref
  ; mutable constraints : constr_packed list
  }

let new_ws : unit -> ws = fun () -> { vars = ref 0; constraints = [] }

let variable ws =
  incr ws.vars;
  Var (Var.Atom !(ws.vars))
;;

let variables ws size =
  Var
    (Var.Vec
       (Array.init size ~f:(fun _ ->
          incr ws.vars;
          Var.Atom !(ws.vars))))
;;

let const_of_list xs = Data (Array.of_list xs)

module Linear_constr : sig
  type t

  val sexp_of_t : t -> Sexp.t
  val linearise_constr : constr_packed -> t
end = struct
  type t =
    | Equality of ((Var.var_id * float) list * float)
    | Inequality_lt of ((Var.var_id * float) list * float)
  [@@deriving sexp_of]

  type linear_atom =
    { coeffs : (Var.var_id * float) list
    ; const : float
    }

  let of_var id = { coeffs = [ id, 1. ]; const = 0. }
  let of_const c = { coeffs = []; const = c }

  let scale s { coeffs; const } =
    { coeffs = List.map coeffs ~f:(fun (id, c) -> id, s *. c); const = s *. const }
  ;;

  let merge_atoms (xs : linear_atom) (ys : linear_atom) =
    let rec join coeffs ~acc =
      match coeffs with
      | [] -> acc
      | x :: x' :: xs when Int.(fst x = fst x') ->
        join ((fst x, snd x +. snd x') :: xs) ~acc
      | x :: xs -> join xs ~acc:(x :: acc)
    in
    let coeffs =
      xs.coeffs @ ys.coeffs
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      |> join ~acc:[]
    in
    { coeffs; const = xs.const +. ys.const }
  ;;

  let (add : linear_atom -> linear_atom -> linear_atom) = merge_atoms
  let sub l r = add l (scale (-1.) r)

  let rec linearize_atom : Var.atom expr -> linear_atom = function
    | Var (Var.Atom id) -> of_var id
    | Const c -> of_const c
    | Add (l, r) -> add (linearize_atom l) (linearize_atom r)
    | Sub (l, r) -> sub (linearize_atom l) (linearize_atom r)
    | Mul (s, e) -> scale s (linearize_atom e)
    | Var (Var.Vec _) -> failwith "Can't linearise vec"
    | Data _ -> failwith "Can't linearise vec"
  ;;

  let equality_atom : Var.atom constr -> t = function
    | Eq (lhs, rhs) ->
      let lhs = linearize_atom lhs in
      let rhs = linearize_atom rhs in
      let coeffs = (sub lhs rhs).coeffs in
      let const = (sub rhs lhs).const in
      Equality (coeffs, const)
    | Le (lhs, rhs) ->
      let lhs = linearize_atom lhs in
      let rhs = linearize_atom rhs in
      let coeffs = (sub lhs rhs).coeffs in
      let const = (sub rhs lhs).const in
      Inequality_lt (coeffs, const)
    | Ge (lhs, rhs) ->
      let lhs = linearize_atom lhs in
      let rhs = linearize_atom rhs in
      let coeffs = (sub rhs lhs).coeffs in
      let const = (sub lhs rhs).const in
      Inequality_lt (coeffs, const)
  ;;

  let linearise_constr : constr_packed -> t = function
    | Constr_atom c -> equality_atom c
    | _ -> failwith "todo"
  ;;
end
