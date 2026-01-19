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

module Eval_constr = struct
  type coeff = Var.var_id * float [@@deriving sexp_of]
  type row = coeff list * float [@@deriving sexp_of]

  type t =
    | Equality of row
    | Inequality_le of row
  [@@deriving sexp_of]

  module Atom = struct
    type t =
      { coeffs : coeff list
      ; const : float
      }

    let of_var id = { coeffs = [ id, 1. ]; const = 0. }
    let of_const c = { coeffs = []; const = c }

    let scale s { coeffs; const } =
      { coeffs = List.map coeffs ~f:(fun (id, c) -> id, s *. c); const = s *. const }
    ;;

    let add (xs : t) (ys : t) =
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

    let sub l r = add l (scale (-1.) r)

    let rec linearize_expr : Var.atom expr -> t = function
      | Var (Var.Atom id) -> of_var id
      | Const c -> of_const c
      | Add (l, r) -> add (linearize_expr l) (linearize_expr r)
      | Sub (l, r) -> sub (linearize_expr l) (linearize_expr r)
      | Mul (s, e) -> scale s (linearize_expr e)
      | Var (Var.Vec _) -> failwith "Can't linearise vec"
      | Data _ -> failwith "Can't linearise vec"
    ;;

    let eval_constr = function
      | Eq (lhs, rhs) ->
        let lhs = linearize_expr lhs in
        let rhs = linearize_expr rhs in
        let coeffs = (sub lhs rhs).coeffs in
        let const = (sub rhs lhs).const in
        Equality (coeffs, const)
      | Le (lhs, rhs) ->
        let lhs = linearize_expr lhs in
        let rhs = linearize_expr rhs in
        let coeffs = (sub lhs rhs).coeffs in
        let const = (sub rhs lhs).const in
        Inequality_le (coeffs, const)
      | Ge (lhs, rhs) ->
        let lhs = linearize_expr lhs in
        let rhs = linearize_expr rhs in
        let coeffs = (sub rhs lhs).coeffs in
        let const = (sub lhs rhs).const in
        Inequality_le (coeffs, const)
    ;;
  end

  module Vec = struct
    type t =
      { coeffs : coeff array list
      ; consts : float array
      }

    let of_var (v : Var.atom Var.t array) =
      { coeffs =
          [ Array.map v ~f:(function
              | Var.Vec _ -> failwith "Not possible"
              | Atom id -> id, 1.)
          ]
      ; consts = [||]
      }
    ;;

    let of_data d = { coeffs = []; consts = d }

    let scale s { coeffs; consts } =
      { coeffs = List.map coeffs ~f:(fun v -> Array.map v ~f:(fun (id, c) -> id, c *. s))
      ; consts = Array.map consts ~f:(fun x -> x *. s)
      }
    ;;

    let add (xs : t) (ys : t) =
      let n_max =
        max (Array.length xs.consts) (Array.length ys.consts)
        |> max
             (List.max_elt xs.coeffs ~compare:(fun a b ->
                Int.compare (Array.length a) (Array.length b))
              |> Option.value ~default:[||]
              |> Array.length)
        |> max
             (List.max_elt ys.coeffs ~compare:(fun a b ->
                Int.compare (Array.length a) (Array.length b))
              |> Option.value ~default:[||]
              |> Array.length)
      in
      let pad x =
        if Array.length x < n_max
        then (
          let x' = Array.create (0, 0.) ~len:n_max in
          Array.blito ~src:x ~dst:x' ();
          x')
        else x
      in
      let rec join (coeffs : coeff array list) ~(acc : coeff array list) =
        match coeffs with
        | [] -> acc
        | x :: x1 :: xs when Int.(fst x.(0) = fst x1.(0)) ->
          let x = pad x in
          let x1 = pad x1 in
          let x' =
            Array.map2_exn x x1 ~f:(fun (id1, coeff1) (id2, coeff2) ->
              assert (Int.(id1 = id2));
              id1, coeff1 +. coeff2)
          in
          join (x' :: xs) ~acc
        | x :: xs -> join xs ~acc:(x :: acc)
      in
      let coeffs : coeff array list =
        xs.coeffs @ ys.coeffs
        |> List.sort ~compare:(fun a b -> Int.compare (fst a.(0)) (fst b.(0)))
        |> join ~acc:[]
      in
      if Array.length xs.consts = Array.length ys.consts
      then { coeffs; consts = Array.map2_exn xs.consts ys.consts ~f:(fun a b -> a +. b) }
      else (
        match Array.length xs.consts, Array.length ys.consts with
        | _, 0 -> { coeffs; consts = xs.consts }
        | 0, _ -> { coeffs; consts = ys.consts }
        | _ -> failwith "Invalid array length")
    ;;

    let sub l r = add l (scale (-1.) r)

    let rec linearize_expr : Var.vec expr -> t = function
      | Var (Var.Vec id) -> of_var id
      | Data d -> of_data d
      | Add (l, r) -> add (linearize_expr l) (linearize_expr r)
      | Sub (l, r) -> sub (linearize_expr l) (linearize_expr r)
      | Mul (s, e) -> scale s (linearize_expr e)
      | Var (Var.Atom _) -> failwith "Can't linearise Atom as Vec"
      | Const _ -> failwith "Can't linearise Atom as Vec"
    ;;

    let expand (t : t) : row list =
      let n_max =
        max
          (Array.length t.consts)
          (List.max_elt t.coeffs ~compare:(fun a b ->
             Int.compare (Array.length a) (Array.length b))
           |> Option.value ~default:[||]
           |> Array.length)
      in
      let pad_coeffs x =
        if Array.length x < n_max
        then (
          let x' = Array.create (0, 0.) ~len:n_max in
          Array.blito ~src:x ~dst:x' ();
          x')
        else x
      in
      let pad_data x =
        if Array.length x < n_max
        then (
          let x' = Array.create 0. ~len:n_max in
          Array.blito ~src:x ~dst:x' ();
          x')
        else x
      in
      let ts : row list =
        List.init n_max ~f:(fun i ->
          List.map t.coeffs ~f:(fun x -> (pad_coeffs x).(i)), (pad_data t.consts).(i))
      in
      ts
    ;;

    let eval_constr (c : Var.vec constr) =
      match c with
      | Eq (lhs, rhs) ->
        let lhs = linearize_expr lhs in
        let rhs = linearize_expr rhs in
        let coeffs = (sub lhs rhs).coeffs in
        let consts = (sub rhs lhs).consts in
        expand { coeffs; consts } |> List.map ~f:(fun x -> Equality x)
      | Le (lhs, rhs) ->
        let lhs = linearize_expr lhs in
        let rhs = linearize_expr rhs in
        let coeffs = (sub lhs rhs).coeffs in
        let consts = (sub rhs lhs).consts in
        expand { coeffs; consts } |> List.map ~f:(fun x -> Inequality_le x)
      | Ge (lhs, rhs) ->
        let lhs = linearize_expr lhs in
        let rhs = linearize_expr rhs in
        let coeffs = (sub rhs lhs).coeffs in
        let consts = (sub lhs rhs).consts in
        expand { coeffs; consts } |> List.map ~f:(fun x -> Inequality_le x)
    ;;
  end

  let eval_constr = function
    | Constr_atom c -> [ Atom.eval_constr c ]
    | Constr_vec cs -> Vec.eval_constr cs
  ;;
end
