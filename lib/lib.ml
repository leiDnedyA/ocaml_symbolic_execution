open Z3;;
include Types;;

(* Aliases / helper functions *)

let mk_numeral mk i = Z3.Arithmetic.Integer.mk_numeral_i mk i;;

let int_expr i = Val(Int(i));;

(* Expressions *)

let bool_of_z3 e = Z3.Boolean.is_true e;;

let solve_z3_bool_expr expr mk =
  let solver = (Solver.mk_simple_solver mk) in
    let model = Option.get (Solver.get_model solver) in
    bool_of_z3 (Option.get (Model.eval model expr true));;

let solve_z3_expr expr mk =
  let solver = (Solver.mk_simple_solver mk) in
    let model = Option.get (Solver.get_model solver) in
    (Option.get (Model.eval model expr true));;

let rec z3_expr_of_expr expr env mk =
  match expr with
  | Val v -> (match v with
    | Bool b -> Z3.Boolean.mk_val mk b
    | Int n -> mk_numeral mk n)
  | Var v -> mk_numeral mk (Hashtbl.find env v)
  | And (x, y) -> Z3.Boolean.mk_and mk [
    (z3_expr_of_expr x env mk);
    (z3_expr_of_expr y env mk)]
  | Or (x, y) -> Z3.Boolean.mk_or mk [
    (z3_expr_of_expr x env mk);
    (z3_expr_of_expr y env mk)]
  | Not x -> Z3.Boolean.mk_not mk (z3_expr_of_expr x env mk)
  | Eq (x, y) -> Z3.Boolean.mk_eq mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk)
  | Gt (x, y) -> Z3.Arithmetic.mk_gt mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk)
  | Lt (x, y) -> Z3.Arithmetic.mk_lt mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk)
  | Gte (x, y) -> Z3.Arithmetic.mk_ge mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk)
  | Lte (x, y) -> Z3.Arithmetic.mk_le mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk)
  | Add (x, y) -> Z3.Arithmetic.mk_add mk [(z3_expr_of_expr x env mk);(z3_expr_of_expr y env mk)]
  | Sub (x, y) -> Z3.Arithmetic.mk_sub mk [(z3_expr_of_expr x env mk);(z3_expr_of_expr y env mk)]
  | Mul (x, y) -> Z3.Arithmetic.mk_mul mk [(z3_expr_of_expr x env mk);(z3_expr_of_expr y env mk)]
  | Div (x, y) -> Z3.Arithmetic.mk_div mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk);;

(* let eval_bool expr env = *)
(*   let mk = mk_context [] in *)
(*     let bool_result = solve_z3_bool_expr (z3_expr_of_expr expr mk) mk in *)
(*       if bool_result == true then (Bool true) else (Bool false);; *)

let eval_expr expr env =
  let mk = mk_context [] in
  Z3.Expr.to_string (solve_z3_expr (z3_expr_of_expr expr env mk) mk);;

let eval_expr_int expr env =
  int_of_string (eval_expr expr env);;

(* Statements *)

let eval_stmt env pc stmt = (*env -> hashtable, pc -> program counter (line number), stmt -> statement*)
  (*evaluate a statement -> return an updated env and updated pc*)
  match stmt with
  | Assign (str, expr) ->
      Hashtbl.replace env str (eval_expr_int expr env); pc + 1
  | If (expr, if_pc) ->
      (
      match (eval_expr expr env) with
      | "true" -> if_pc
      | "false" -> pc + 1
      | _ -> failwith("Invalid boolean result")
      )
  | Goto (goto_pc) -> goto_pc
;;


(* Programs *)

let rec eval_prgm env pc (prgm:stmt list) = 
  if (pc >= List.length prgm) then
    0
  else
    eval_prgm env (eval_stmt env pc (List.nth prgm pc)) prgm
;;
