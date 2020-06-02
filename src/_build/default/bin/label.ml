(* file: label.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   An implementation of labels for branching.
*)
type t = string

let fromString (x:string) : t = x
let format x = x
let fresh () =
  Lib.fmt "l%d" (Lib.fresh ())
