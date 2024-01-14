open Z3;;

type value =
  | Bool of bool
  | Int of int
;;

type expr =
  | Value of value
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

(* type alg_expr = *)
(*   (* Algebra *) *)
(*   | Addition of expr * expr *)
(*   | Subtraction of expr * expr *)
(*   | Multiplication of expr * expr *)
(*   | Division of expr * expr *)
(* ;; *)
(**)
(* type ineq_expr = *)
(*   (* Inequalities *) *)
(*   | Eq of expr * expr *)
(*   | Gt of expr * expr *)
(*   | Gte of expr * expr *)
(*   | Lt of expr * expr *)
(*   | Lte of expr * expr *)
(* ;; *)
(**)
(* type bool_expr = *)
(*   (* Boolean *) *)
(*   | Not of expr *)
(*   | And of expr * expr *)
(*   | Or of expr * expr *)
(* ;; *)
