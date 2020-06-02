(* file: env.mli
 * author: Robert Muller
 *
 * CSCI 3366 Programming Languages
 *
 * This file contains the signature of maps in the miniC compiler.
 *)

type key = Symbol.t
type 'a t

val empty : 'a t
val add : key -> 'a -> 'a t -> 'a t
val find : key -> 'a t -> 'a
val mem : key -> 'a t -> bool
val iter : (key -> 'a -> unit) -> 'a t -> unit
val bindings : 'a t -> (key * 'a) list

val keyFormat : key -> string
  val make : 'a list -> 'a t
val toString : ('a -> string) -> 'a t -> string
