(* file: basis.mli
 * author: Bob Muller
 *)
type t = Symbol.t

val primOps : t list
val isPrim  : t -> bool
