open Z3;;
(* open Types;; *)

type value =
  | Bool of bool
  | Int of int
;;

type expr =
  | Val of value
  (* Algebra *)
  | Addition of expr * expr
  | Subtraction of expr * expr
  | Multiplication of expr * expr
  | Division of expr * expr
  (* Inequalities *)
  | Eq of expr * expr
  | Gt of expr * expr
  | Gte of expr * expr
  | Lt of expr * expr
  | Lte of expr * expr
  (* Boolean *)
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
;;

(* Boolean solving *)
let bool_of_z3 e = Z3.Boolean.is_true e;;

let solve_z3_bool_expr expr mk =
  let solver = (Solver.mk_simple_solver mk) in
    let model = Option.get (Solver.get_model solver) in
    bool_of_z3 (Option.get (Model.eval model expr true));;

let rec z3_expr_of_expr expr mk =
  match expr with
  | Val v -> (match v with
    | Bool b -> Z3.Boolean.mk_val mk b
    | Int n -> Z3.Arithmetic.Integer.mk_numeral_i mk n)
  | And (x, y) -> Z3.Boolean.mk_and mk [
    (z3_expr_of_expr x mk);
    (z3_expr_of_expr y mk)]
  | Or (x, y) -> Z3.Boolean.mk_or mk [
    (z3_expr_of_expr x mk);
    (z3_expr_of_expr y mk)]
  | Not x -> Z3.Boolean.mk_not mk (z3_expr_of_expr x mk)
  | Eq (x, y) -> Z3.Boolean.mk_eq mk (z3_expr_of_expr x mk) (z3_expr_of_expr y mk)
  | Gt (x, y) -> Z3.Arithmetic.mk_gt mk (z3_expr_of_expr x mk) (z3_expr_of_expr y mk)
  | Lt (x, y) -> Z3.Arithmetic.mk_lt mk (z3_expr_of_expr x mk) (z3_expr_of_expr y mk)
  | Gte (x, y) -> Z3.Arithmetic.mk_ge mk (z3_expr_of_expr x mk) (z3_expr_of_expr y mk)
  | Lte (x, y) -> Z3.Arithmetic.mk_le mk (z3_expr_of_expr x mk) (z3_expr_of_expr y mk)
  | _ -> failwith("invalid boolean expression");;


let eval_bool expr =
  let mk = mk_context [] in
    let bool_result = solve_z3_bool_expr (z3_expr_of_expr expr mk) mk in
      if bool_result == true then (Bool true) else (Bool false);;

