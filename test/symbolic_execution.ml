open Lib;;
open Z3;;

let run_test condition test_number =
  if condition = false then
    failwith(Printf.sprintf
    "Test %d failed.\n"
    test_number)
  else
    None;;

let get_first_entry t =
  match t with
  | v, _ -> v
;;

let get_second_entry t =
  match t with
  | _, v -> v
;;

let extract_option o =
  match o with
  | Some v -> v
  | None -> failwith("None option provided")
;;

(* Test 1, Assign stmt*)
let env_1 = Hashtbl.create 1;;
let test_assign_stmt = Assign("x", int_expr 1) in
eval_stmt env_1 0 test_assign_stmt;;
run_test ((Hashtbl.find env_1 "x") = 1) 1;;

(* Test 2, If stmt*)
let test_if_stmt = If(Val(Bool(true)), 15) in
let result = eval_stmt env_1 0 test_if_stmt in
run_test (result = 15) 2;;

(* Test 3, Goto stmt*)
let test_goto_stmt = Goto(15) in
let result = eval_stmt env_1 0 test_goto_stmt in
run_test (result = 15) 3;;

(* Test 4, Assign stmt w/ nested expr*)
let test_assign_stmt = Assign("x", Add(int_expr 5, int_expr 10)) in
eval_stmt env_1 0 test_assign_stmt;;
run_test ((Hashtbl.find env_1 "x") = 15) 4;;

(* Test 5, full program*)
let env_2 = Hashtbl.create 1;;
let prgm = [
  If(Gt(int_expr 1, int_expr 0), 3);
  Assign("x", int_expr 1);
  Goto(4);
  Assign("x", int_expr 3)
  ] in
eval_prgm env_2 0 prgm;;
run_test ((Hashtbl.find env_2 "x") = 3) 5;;

(* Test 6, int expressions*)
let env = Hashtbl.create 1 in
let test_expr = Add(int_expr 1, Div(int_expr 10, int_expr 2)) in
let result = eval_expr_int test_expr env in
run_test (result = 6) 6;;

(* Test 7, int expressions*)
let env = Hashtbl.create 1 in
let test_expr = And(Gt(int_expr 3, int_expr 1), Or(Val(Bool(true)), Val(Bool(false)))) in
let result = eval_expr_bool test_expr env in
run_test (result = true) 7;;

(* Test 8, Z3 Arithmetic const evaluation*)
let mk = empty_context in
let env = Hashtbl.create 1 in
let expr = Add(Val(Sym "x"), Val(Int 1)) in
let z3_expr = z3_expr_of_expr expr env mk in
let output = Z3.Expr.to_string z3_expr in
run_test (output = "(+ x 1)") 8;;

(* Test 9, symbolic exeuction GOTO statement evaluation*)
let test_env = Hashtbl.create 1 in
let mk = empty_context in
let state = {env=test_env; pc=0; path=(Boolean.mk_true mk)} in
let cmd = Goto(1) in
let result = eval_sym_stmt state mk cmd in
run_test ((get_first_entry result).pc = 1) 9;;

(* Test 10, symbolic execution IF statement evaluation*)
let test_env = Hashtbl.create 1 in
let mk = empty_context in
let state = {env=test_env; pc=0; path=(Boolean.mk_true mk)} in
let true_pc = 20 in
let cmd = If(Gt(int_expr 1, int_expr 2), true_pc) in
let result = eval_sym_stmt state mk cmd in
let entry2 = get_second_entry result in
let cond1 = ((get_first_entry result).pc = 1) in
let cond2 = (entry2 = None) in
run_test (cond1 && cond2) 10;;

(* Test 11, symbolic execution IF statement evaluation w two vald paths*)
let test_env = Hashtbl.create 1 in
let mk = empty_context in
let state = {env=test_env; pc=0; path=(Boolean.mk_true mk)} in
let true_pc = 20 in
let assn = Assign("x", Val(Sym("x"))) in
let cmd = If(Gt(int_expr 1, Var("x")), true_pc) in
let _ = eval_sym_stmt state mk assn in
let result = eval_sym_stmt state mk cmd in
let entry2 = extract_option (get_second_entry result) in
let cond1 = ((get_first_entry result).pc = 20) in
let cond2 = entry2.pc = 1 in
run_test (cond1 && cond2) 11;;

(* Test 12, symbolic execution of entire program*)
let test_env = Hashtbl.create 1 in
let program = [
  Assign("x", Val(Sym("x")));
  If(Gt(int_expr 1, Var("x")), 3);
] in
let mk = empty_context in
let state = {env=test_env; pc=0; path=(Boolean.mk_true mk)} in
let result = eval_sym_prgm mk program state in
let cond = (List.length result) = 2 in
(* List.iter (fun result -> print_endline result) result; *)
run_test cond 12;;
