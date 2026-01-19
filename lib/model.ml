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

type affine_expr =
  | Constr_vec : Var.vec constr -> affine_expr
  | Constr_atom : Var.atom constr -> affine_expr
  | Cost_atom : Var.atom expr -> affine_expr
  | Cost_vec : Var.vec expr -> affine_expr
[@@deriving sexp_of]

let rec witness_of_expr : type a. a expr -> a Var.witness = function
  | Const _ -> Var.Atom_wit
  | Var e -> Var.witness_of_t e
  | Add (l, _) -> witness_of_expr l
  | Sub (l, _) -> witness_of_expr l
  | Mul (_, r) -> witness_of_expr r
  | Data _ -> Var.Vec_wit
;;

let pack_contr : type a. a Var.witness -> a constr -> affine_expr =
  fun witness constr ->
  match witness with
  | Atom_wit -> Constr_atom constr
  | Vec_wit -> Constr_vec constr
;;

module Infix = struct
  let ( == ) : type a. a expr -> a expr -> affine_expr =
    fun l r -> pack_contr (witness_of_expr l) (Eq (l, r))
  ;;

  let ( <= ) : type a. a expr -> a expr -> affine_expr =
    fun l r -> pack_contr (witness_of_expr l) (Le (l, r))
  ;;

  let ( >= ) : type a. a expr -> a expr -> affine_expr =
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
  ; mutable affine_exprs : affine_expr list
  }

let new_ws : unit -> ws = fun () -> { vars = ref 0; affine_exprs = [] }

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

let add_cost e : affine_expr =
  match witness_of_expr e with
  | Var.Atom_wit -> Cost_atom e
  | Var.Vec_wit -> Cost_vec e
;;

let const_of_list xs = Data (Array.of_list xs)

module Eval = struct
  type coeff = Var.var_id * float [@@deriving sexp_of]
  type row = coeff list * float [@@deriving sexp_of]

  type t =
    | Equality of row
    | Inequality_le of row
    | Cost of coeff list
  [@@deriving sexp_of]

  module Linear = struct
    let scale : float -> coeff list -> coeff list =
      fun s cs -> List.map cs ~f:(fun (id, c) -> id, s *. c)
    ;;

    let merge : coeff list -> coeff list -> coeff list =
      fun a b ->
      let rec join coeffs ~acc =
        match coeffs with
        | [] -> acc
        | x :: x' :: xs when Int.(fst x = fst x') ->
          join ((fst x, snd x +. snd x') :: xs) ~acc
        | x :: xs -> join xs ~acc:(x :: acc)
      in
      let xs = a @ b |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
      join xs ~acc:[]
    ;;
  end

  module Constr = struct
    (* TODO: duplicated coefficient code in cost and constraints *)
    module Atom = struct
      type t =
        { coeffs : coeff list
        ; const : float
        }

      let of_var id = { coeffs = [ id, 1. ]; const = 0. }
      let of_const c = { coeffs = []; const = c }

      let scale s { coeffs; const } =
        { coeffs = Linear.scale s coeffs; const = s *. const }
      ;;

      let add (xs : t) (ys : t) =
        { coeffs = Linear.merge xs.coeffs ys.coeffs; const = xs.const +. ys.const }
      ;;

      let sub l r = add l (scale (-1.) r)

      let rec of_expr : Var.atom expr -> t = function
        | Var (Var.Atom id) -> of_var id
        | Const c -> of_const c
        | Add (l, r) -> add (of_expr l) (of_expr r)
        | Sub (l, r) -> sub (of_expr l) (of_expr r)
        | Mul (s, e) -> scale s (of_expr e)
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
        let lhs = of_expr lhs_expr in
        let rhs = of_expr rhs_expr in
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
        { coeffs =
            List.map coeffs ~f:(fun v -> Array.map v ~f:(fun (id, c) -> id, c *. s))
        ; consts = Array.map consts ~f:(fun x -> x *. s)
        }
      ;;

      let add ws (xs : t) (ys : t) =
        let n_max = !(ws.vars) in
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
        then
          { coeffs; consts = Array.map2_exn xs.consts ys.consts ~f:(fun a b -> a +. b) }
        else (
          match Array.length xs.consts, Array.length ys.consts with
          | _, 0 -> { coeffs; consts = xs.consts }
          | 0, _ -> { coeffs; consts = ys.consts }
          | _ -> failwith "Invalid array length")
      ;;

      let sub l r = add l (scale (-1.) r)

      let rec of_expr : ws -> Var.vec expr -> t =
        fun ws e ->
        match e with
        | Var (Var.Vec id) -> of_var id
        | Data d -> of_data d
        | Add (l, r) -> add ws (of_expr ws l) (of_expr ws r)
        | Sub (l, r) -> sub ws (of_expr ws l) (of_expr ws r)
        | Mul (s, e) -> scale s (of_expr ws e)
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

      let eval ws constr =
        let lhs_expr, rhs_expr, wrap =
          match constr with
          | Eq (l, r) -> l, r, fun x -> Equality x
          | Le (l, r) -> l, r, fun x -> Inequality_le x
          | Ge (l, r) -> r, l, fun x -> Inequality_le x
        in
        let lhs = of_expr ws lhs_expr in
        let rhs = of_expr ws rhs_expr in
        let diff = sub ws lhs rhs in
        expand { coeffs = diff.coeffs; consts = Array.map diff.consts ~f:Float.neg }
        |> List.map ~f:wrap
      ;;
    end
  end

  module Cost = struct
    module Atom = struct
      type t = coeff list

      let of_var id : t = [ id, 1. ]
      let scale = Linear.scale
      let add = Linear.merge
      let sub l r = add l (scale (-1.) r)

      let rec of_expr : Var.atom expr -> t = function
        | Var (Var.Atom id) -> of_var id
        | Const _ -> failwith "Can't have a constant in a cost function"
        | Add (l, r) -> add (of_expr l) (of_expr r)
        | Sub (l, r) -> sub (of_expr l) (of_expr r)
        | Mul (s, e) -> scale s (of_expr e)
        | Var (Var.Vec _) -> failwith "Can't linearise vec"
        | Data _ -> failwith "Can't linearise vec"
      ;;

      let eval e = Cost (of_expr e)
    end

    module Vec = struct
      type t = coeff array list

      let pad_array ~default ~len x =
        if Array.length x < len
        then (
          let x' = Array.create default ~len in
          Array.blito ~src:x ~dst:x' ();
          x')
        else x
      ;;

      let of_var (v : Var.atom Var.t array) : t =
        [ Array.map v ~f:(function
            | Var.Vec _ -> failwith "Not possible"
            | Atom id -> id, 1.)
        ]
      ;;

      let scale s coeffs =
        List.map coeffs ~f:(fun v -> Array.map v ~f:(fun (id, c) -> id, c *. s))
      ;;

      let add ws (xs : t) (ys : t) =
        let n_max = !(ws.vars) in
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
        let coeffs : t =
          xs @ ys
          |> List.sort ~compare:(fun a b -> Int.compare (fst a.(0)) (fst b.(0)))
          |> join ~acc:[]
        in
        coeffs
      ;;

      let sub l r = add l (scale (-1.) r)

      let rec of_expr : ws -> Var.vec expr -> t =
        fun ws e ->
        match e with
        | Var (Var.Vec id) -> of_var id
        | Data _ -> failwith "Can't use data in cost function"
        | Add (l, r) -> add ws (of_expr ws l) (of_expr ws r)
        | Sub (l, r) -> sub ws (of_expr ws l) (of_expr ws r)
        | Mul (s, e) -> scale s (of_expr ws e)
        | Var (Var.Atom _) -> failwith "Can't linearise Atom as Vec"
        | Const _ -> failwith "Can't linearise Atom as Vec"
      ;;

      let expand ws (t : t) =
        let n_max = !(ws.vars) in
        let pad_coeffs = pad_array ~default:(0, 0.) ~len:n_max in
        List.init n_max ~f:(fun i -> List.map t ~f:(fun x -> (pad_coeffs x).(i)))
        |> List.concat
      ;;

      let eval ws e = Cost (of_expr ws e |> expand ws)
    end
  end

  let eval_constr ws cs =
    match cs with
    | Constr_atom c -> [ Constr.Atom.eval_constr c ]
    | Constr_vec cs -> Constr.Vec.eval ws cs
    | Cost_atom e -> [ Cost.Atom.eval e ]
    | Cost_vec e -> [ Cost.Vec.eval ws e ]
  ;;
end

module Constr_Set = struct
  type a = float array array [@@deriving sexp_of]
  type b = float array [@@deriving sexp_of]
  type g = float array array [@@deriving sexp_of]
  type h = float array [@@deriving sexp_of]
  type c = float array [@@deriving sexp_of]

  type t =
    { a : a
    ; b : b
    ; g : g
    ; h : h
    ; c : c
    }
  [@@deriving sexp_of]

  let create_matrix (ws : ws) (rows : Eval.coeff list list) =
    let n_rows = List.length rows in
    (* NOTE: What if they are all the same, what does it return *)
    let n_cols = !(ws.vars) in
    let m = Array.make_matrix ~dimx:n_rows ~dimy:n_cols 0. in
    List.iteri rows ~f:(fun i row ->
      List.iteri row ~f:(fun _ (idx, coeff) -> m.(i).(idx - 1) <- coeff));
    m
  ;;

  let create_cost (ws : ws) (coeffs : Eval.coeff list) : c =
    let n = !(ws.vars) in
    let find_coeff i = List.find coeffs ~f:(fun (idx, _) -> Int.(i + 1 = idx)) in
    Array.init n ~f:(fun i -> find_coeff i |> Option.value ~default:(0, 0.) |> snd)
  ;;

  let create ws (constrs : Eval.t list) : t =
    let rec aux (constrs : Eval.t list) a b g h c =
      match constrs with
      | [] -> a, b, g, h, c
      | Eval.Equality (coeffs, const) :: constrs ->
        aux constrs (coeffs :: a) (const :: b) g h c
      | Eval.Inequality_le (coeffs, const) :: constrs ->
        aux constrs a b (coeffs :: g) (const :: h) c
      | Eval.Cost coeffs :: constrs -> aux constrs a b g h (coeffs :: c)
    in
    let a, b, g, h, c = aux constrs [] [] [] [] [] in
    let a = create_matrix ws a in
    let g = create_matrix ws g in
    let b = Array.of_list b in
    let h = Array.of_list h in
    let c = create_cost ws (List.concat c) in
    { a; b; g; h; c }
  ;;
end

type ecos_params =
  { n : int (** Number of primal variables *)
  ; m : int (** Number of constraints. Dimension 1 of matrix h *)
  ; p : int
    (** Number of equality constraints. Dimension 1 of Matrix A and length of vector b*)
  ; l : int (** Dimentsion of the positive orthant *)
  ; ncones : int (** Number of second-order cones present in problem *)
  ; q : int array (** Array of length ncones; q[i] defines the dimension of the cone i *)
  ; e : int (** Number of exponential cones present in problem *)
  ; g : Constr_Set.g
  ; a : Constr_Set.a
  ; h : Constr_Set.h
  ; b : Constr_Set.b (* TODO: cost vector *)
  }

let get_params (ws : ws) =
  let constrs =
    List.map ws.affine_exprs ~f:(fun e -> Eval.eval_constr ws e) |> List.concat
  in
  let n = !(ws.vars) in
  let set = Constr_Set.create ws constrs in
  let m = Array.length set.h in
  let p = Array.length set.b in
  let l = Array.length set.g in
  let ncones = 0 in
  let q = [||] in
  let e = 0 in
  let a = set.a in
  let b = set.b in
  let g = set.g in
  let h = set.h in
  { n; m; p; l; ncones; q; e; a; b; g; h }
;;
