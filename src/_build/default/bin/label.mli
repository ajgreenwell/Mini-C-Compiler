(* file: label.mli
   author: Bob Muller

   CSCI 3366 Programming Languages

   The interface for labels.
*)
type t

val fresh : unit -> t
val format : t -> string
val fromString : string -> t
