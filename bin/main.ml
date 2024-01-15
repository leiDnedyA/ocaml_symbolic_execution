open Lib;;

let result = eval_bool (Gte ((Val (Int 0)), (Val (Int 1))));;

let extract_bool v =
  match v with
  | Bool b -> b
  | _ -> failwith("Not a Bool Value")
;;

Printf.printf "%b\n" (extract_bool result)
