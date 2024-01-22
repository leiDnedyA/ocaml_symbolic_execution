open Z3;;
open Types;;
open Util;;

let rec z3_expr_of_sym_expr expr env mk =
  match expr with
  | Val v -> (match v with
    | Bool b -> Z3.Boolean.mk_val mk b
    | Int n -> mk_numeral mk n
    | Sym s -> Z3.Arithmetic.Integer.mk_const_s mk s
    )
  | Var v -> Hashtbl.find env v
  | And (x, y) -> Z3.Boolean.mk_and mk [
    (z3_expr_of_sym_expr x env mk);
    (z3_expr_of_sym_expr y env mk)]
  | Or (x, y) -> Z3.Boolean.mk_or mk [
    (z3_expr_of_sym_expr x env mk);
    (z3_expr_of_sym_expr y env mk)]
  | Not x -> Z3.Boolean.mk_not mk (z3_expr_of_sym_expr x env mk)
  | Eq (x, y) -> Z3.Boolean.mk_eq mk (z3_expr_of_sym_expr x env mk) (z3_expr_of_sym_expr y env mk)
  | Gt (x, y) -> Z3.Arithmetic.mk_gt mk (z3_expr_of_sym_expr x env mk) (z3_expr_of_sym_expr y env mk)
  | Lt (x, y) -> Z3.Arithmetic.mk_lt mk (z3_expr_of_sym_expr x env mk) (z3_expr_of_sym_expr y env mk)
  | Gte (x, y) -> Z3.Arithmetic.mk_ge mk (z3_expr_of_sym_expr x env mk) (z3_expr_of_sym_expr y env mk)
  | Lte (x, y) -> Z3.Arithmetic.mk_le mk (z3_expr_of_sym_expr x env mk) (z3_expr_of_sym_expr y env mk)
  | Add (x, y) -> Z3.Arithmetic.mk_add mk [(z3_expr_of_sym_expr x env mk);(z3_expr_of_sym_expr y env mk)]
  | Sub (x, y) -> Z3.Arithmetic.mk_sub mk [(z3_expr_of_sym_expr x env mk);(z3_expr_of_sym_expr y env mk)]
  | Mul (x, y) -> Z3.Arithmetic.mk_mul mk [(z3_expr_of_sym_expr x env mk);(z3_expr_of_sym_expr y env mk)]
  | Div (x, y) -> Z3.Arithmetic.mk_div mk (z3_expr_of_sym_expr x env mk) (z3_expr_of_sym_expr y env mk)
;;

