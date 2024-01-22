open Z3;;
open Types;;


(* Aliases / helper functions *)

let mk_numeral mk i = Z3.Arithmetic.Integer.mk_numeral_i mk i;;

let int_expr i = Val(Int(i));;

let empty_context = mk_context [];;

