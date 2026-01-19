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

    let eval_constr constr =
      let lhs_expr, rhs_expr, wrap =
        match constr with
        | Eq (l, r) -> l, r, fun x -> Equality x
        | Le (l, r) -> l, r, fun x -> Inequality_le x
        | Ge (l, r) -> r, l, fun x -> Inequality_le x
      in
      let lhs = linearize_expr lhs_expr in
      let rhs = linearize_expr rhs_expr in
      let diff = sub lhs rhs in
      wrap (diff.coeffs, ~-.(diff.const))
    ;;
  end

  module Vec = struct
    type t =
      { coeffs : coeff array list
      ; consts : float array
      }

    let max_length { coeffs; consts } =
      max
        (Array.length consts)
        (List.max_elt coeffs ~compare:(fun a b ->
           Int.compare (Array.length a) (Array.length b))
         |> Option.value ~default:[||]
         |> Array.length)
    ;;

    let pad_array ~default ~len x =
      if Array.length x < len
      then (
        let x' = Array.create default ~len in
        Array.blito ~src:x ~dst:x' ();
        x')
      else x
    ;;

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
      let n_max = max (max_length xs) (max_length ys) in
      let pad = pad_array ~default:(0, 0.) ~len:n_max in
      let rec join (coeffs : coeff array list) ~(acc : coeff array list) =
        match coeffs with
        | [] -> acc
        | x :: x1 :: xs when Int.(fst x.(0) = fst x1.(0)) ->
          let x = pad x in
          let x1 = pad x1 in
          let x' =
            Array.map2_exn x x1 ~f:(fun (id1, coeff1) (id2, coeff2) ->
              if Int.(id1 <> id2)
              then
                failwith
                  [%string
                    "Vec.add: mismatched variable ids during coefficient merge: \
                     %{id1#Int} <> %{id2#Int}"]
              else id1, coeff1 +. coeff2)
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
      let n_max = max_length t in
      let pad_coeffs = pad_array ~default:(0, 0.) ~len:n_max in
      let consts = pad_array ~default:0. ~len:n_max t.consts in
      List.init n_max ~f:(fun i ->
        List.map t.coeffs ~f:(fun x -> (pad_coeffs x).(i)), consts.(i))
    ;;

    let eval_constr constr =
      let lhs_expr, rhs_expr, wrap =
        match constr with
        | Eq (l, r) -> l, r, fun x -> Equality x
        | Le (l, r) -> l, r, fun x -> Inequality_le x
        | Ge (l, r) -> r, l, fun x -> Inequality_le x
      in
      let lhs = linearize_expr lhs_expr in
      let rhs = linearize_expr rhs_expr in
      let diff = sub lhs rhs in
      expand { coeffs = diff.coeffs; consts = Array.map diff.consts ~f:Float.neg }
      |> List.map ~f:wrap
    ;;
  end

  let eval_constr = function
    | Constr_atom c -> [ Atom.eval_constr c ]
    | Constr_vec cs -> Vec.eval_constr cs
  ;;
end

module Constr_Set = struct
  type a = float array array [@@deriving sexp_of]
  type b = float array [@@deriving sexp_of]
  type g = float array array [@@deriving sexp_of]
  type h = float array [@@deriving sexp_of]

  type t =
    { a : a
    ; b : b
    ; g : g
    ; h : h
    }
  [@@deriving sexp_of]

  let create_matrix (rows : Eval_constr.coeff list list) =
    let n_rows = List.length rows in
    (* NOTE: What if they are all the same, what does it return *)
    let n_cols =
      List.map rows ~f:(fun row -> List.map row ~f:(fun (idx, _) -> idx))
      |> List.concat_no_order
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    let m = Array.make_matrix ~dimx:n_rows ~dimy:n_cols 0. in
    List.iteri rows ~f:(fun i row ->
      List.iteri row ~f:(fun _ (idx, coeff) -> m.(i).(idx - 1) <- coeff));
    m
  ;;

  let create (constrs : Eval_constr.t list) : t =
    let rec aux (constrs : Eval_constr.t list) a b g h =
      match constrs with
      | [] -> a, b, g, h
      | Eval_constr.Equality (coeffs, const) :: constrs ->
        aux constrs (coeffs :: a) (const :: b) g h
      | Eval_constr.Inequality_le (coeffs, const) :: constrs ->
        aux constrs a b (coeffs :: g) (const :: h)
    in
    let a, b, g, h = aux constrs [] [] [] [] in
    let a = create_matrix a in
    let g = create_matrix g in
    let b = Array.of_list b in
    let h = Array.of_list h in
    { a; b; g; h }
  ;;
end
