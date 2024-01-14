open Lib;;

let result = eval_bool (Not (Value (Bool true)));;

let extract_bool v =
  match v with
  | Bool b -> b
  | _ -> failwith("Not a Bool Value")
;;

Printf.printf "%b\n" (extract_bool result)
