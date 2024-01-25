open Lib;;
open Z3;;

let test_env = Hashtbl.create 1 in
let program = [
  Assign("x", Val(Sym("x")));
  If(Gt(int_expr 1, Var("x")), 3);
] in
let mk = empty_context in
let state = {env=test_env; pc=0; path=(Boolean.mk_true mk)} in
let result = eval_sym_prgm mk program state in
List.iter (fun result -> print_endline result) result;
