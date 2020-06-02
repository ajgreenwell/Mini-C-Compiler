(*
 * file: typ.ml
 *
 * Robert Muller
 * February 20, 2009
 *
 * This file contains an abstract syntax for types for the programming language
 * miniC.
 *)
let fmt = Printf.sprintf

type t = Int
       | Bool
       | Void
       | Arrow of {from : t; too : t}
       | Product of t list

let rec format t =
  match t with
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Arrow {from; too} -> fmt "%s -> %s" (format from) (format too)
  | Product [] -> ""
  | Product [only] -> format only
  | Product (t :: ts) -> fmt "%s * %s" (format t) (format (Product ts))

let rec equal t1 t2 =
  match (t1, t2) with
  | (Int, Int) | (Bool, Bool) | (Void, Void) -> true
  | (Arrow {from = f1; too = t1}, Arrow {from = f2; too = t2}) ->
    (equal f1 f2) && (equal t1 t2)
  | (Product [], Product []) -> true
  | (Product (x :: xs), Product (y :: ys)) ->
    (equal x y) && (equal (Product xs) (Product ys))
  | _ -> false
