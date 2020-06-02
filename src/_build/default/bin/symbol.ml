(* file: symbol.ml
   author: Bob Muller

   CSCI 3366 Programming Languages

   This module contains the representation of symbols.
*)
type t = string

let fromString s = s
let format sym = sym
let fresh() =
  Lib.fmt "x%d" (Lib.fresh ())

let compare = String.compare
