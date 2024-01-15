open Lib;;

let make_int_val i = Val (Int (i))

(* let result = eval_bool (Gte ((Val (Int 0)), (Val (Int 1))));; *)
let result = eval_expr (Sub (make_int_val 20, make_int_val 10));;


let extract_bool v =
  match v with
  | Bool b -> b
  | _ -> failwith("Not a Bool Value")
;;

Printf.printf "%s\n" result;;
