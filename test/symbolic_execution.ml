open Lib;;

let env_1 = Hashtbl.create 1;;
let integration_1 = [
  If(Gt(int_expr 1, int_expr 0), 3);
  Assign("x", int_expr 1);
  Goto(4);
  Assign("x", int_expr 3)
  ];;

(* Test 1, Assign stmt*)
let test_stmt = Assign("x", int_expr 1);;
eval_stmt env_1 0 test_stmt;;
if (Hashtbl.find env_1 "x") != 1 then
  failwith("Test 1 failed, issue with assign statement eval.")
else
  None;;

