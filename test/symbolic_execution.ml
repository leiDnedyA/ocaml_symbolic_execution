open Lib;;

let run_test condition test_number =
  if condition = false then
    failwith(Printf.sprintf
    "Test %d failed, issue with assign statement eval.\n"
    test_number)
  else
    None;;


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
