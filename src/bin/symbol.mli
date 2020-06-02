(* file: symbol.mli
   author: Bob Muller

    CSCI 3366 Programming Languages

   This is the interface file for symbols.
*)
type t

val fromString : string -> t
val format : t -> string
val fresh : unit -> t
val compare : t -> t -> int
