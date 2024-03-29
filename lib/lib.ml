open Z3;;
include Types;;
include Symbolic_expr;;
include Util;;



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
    | Int n -> mk_numeral mk n
    | Sym s -> Z3.Arithmetic.Integer.mk_const_s mk s
    )
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
  | Div (x, y) -> Z3.Arithmetic.mk_div mk (z3_expr_of_expr x env mk) (z3_expr_of_expr y env mk)
;;

(* let eval_bool expr env = *)
(*   let mk = mk_context [] in *)
(*     let bool_result = solve_z3_bool_expr (z3_expr_of_expr expr mk) mk in *)
(*       if bool_result == true then (Bool true) else (Bool false);; *)

let eval_expr expr env =
  let mk = (empty_context) in
    let result = Z3.Expr.to_string (solve_z3_expr (z3_expr_of_expr expr env mk) mk) in
    match result with
    | "true" -> Bool(true)
    | "false" -> Bool(false)
    | _ -> Int(int_of_string result)
;;

let eval_expr_int expr env =
  let int_val = eval_expr expr env in
    match int_val with
    | Int i -> i
    | _ -> failwith("expressions passed to eval_expr_int must evaluate to Int values, not Bools")
;;

let eval_expr_bool expr env =
  let bool_val = eval_expr expr env in
    match bool_val with
    | Bool b -> b
    | _ -> failwith("expressions passed to eval_expr_bool must evaluate to Bool values, not Int")
;;

(* Statements *)

let eval_stmt env pc stmt = (*env -> hashtable, pc -> program counter (line number), stmt -> statement*)
  (*evaluate a statement -> return an updated env and updated pc*)
  match stmt with
  | Assign (str, expr) ->
      Hashtbl.replace env str (eval_expr_int expr env); pc + 1
  | If (expr, if_pc) ->
      (
      match (eval_expr expr env) with
      | Bool(true) -> if_pc
      | Bool(false) -> pc + 1
      | _ -> failwith("Invalid boolean result")
      )
  | Goto (goto_pc) -> goto_pc
;;

(* Takes a state and returns a tuple of one or two states
   based on the following conditions
   If -> If both t and f conditions are possible based on
         the existing path, return both states.
         If only one condition is possible, return only that 
         state.
         If neither is possible, fail
   Goto -> Return existing state with updated program counter
   Assign -> Assign the provided value in the environment

   state record (st):
    env: hashmap of strings to exprs
    pc: program counter
    path: z3 boolean expression

 *)
let eval_sym_stmt st mk cmd =
  match cmd with
  | Assign (sym, expr) -> 
      Hashtbl.replace st.env sym (z3_expr_of_sym_expr expr st.env mk);
      {st with pc=st.pc + 1}, None
  | Goto pc' -> {st with pc=pc'}, None
  | If (expr, pc') -> 
      let z3_expr = z3_expr_of_sym_expr expr st.env mk in
      let z3_not_expr = Boolean.mk_not mk z3_expr in
      let true_path = Boolean.mk_and mk [st.path;z3_expr] in
      let false_path = Boolean.mk_and mk [st.path;z3_not_expr] in
      let st_f = {st with pc=st.pc+1; path=false_path} in
      let st_t = {st with pc=pc'; path=true_path} in
      let maybe_t =
        begin
          let solver = Solver.mk_simple_solver mk in
          Solver.add solver [true_path];
          Solver.check solver [];
        end in
      let maybe_f =
        begin
          let solver = Solver.mk_simple_solver mk in
          Solver.add solver [false_path];
          Solver.check solver [];
        end in        
      match maybe_t, maybe_f with
      | Solver.SATISFIABLE, Solver.SATISFIABLE ->
        st_t, Some(st_f)
      | Solver.SATISFIABLE, Solver.UNSATISFIABLE ->
        st_t, None
      | Solver.UNSATISFIABLE, Solver.SATISFIABLE ->
        st_f, None
      | Solver.UNSATISFIABLE, Solver.UNSATISFIABLE -> failwith("Impossible")
      | _ -> failwith("Not implemented.")
;;

(* Program evaluation (normal and symbolic) *)

let rec eval_prgm env pc (prgm:stmt list) = 
  if (pc >= List.length prgm) then
    0
  else
    eval_prgm env (eval_stmt env pc (List.nth prgm pc)) prgm
;;

let rec eval_sym_prgm mk prgm st =
  let result = [] in
  let eval_prgm = eval_sym_prgm mk prgm in
  if st.pc >= List.length prgm then
    [Expr.to_string st.path;]
  else
    let result' = eval_sym_stmt st mk (List.nth prgm st.pc) in
    match result' with
    | a, Some b -> List.concat [eval_prgm b; eval_prgm a;]
    | a, None -> eval_prgm a
    | _, _ -> failwith("Invalid result");;

(* let eval_sym_program p = *)
(*   let mk = empty_context in *)
(*   let rec *)
