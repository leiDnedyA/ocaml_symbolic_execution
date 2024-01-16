
type value =
  | Bool of bool
  | Int of int
;;

type expr =
  | Var of string 
  | Val of value
  (* Algebra *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
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

type stmt =
  | Assign of string * expr
  | If of expr * int
  | Goto of int 
;;
