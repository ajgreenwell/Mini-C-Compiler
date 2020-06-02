(* file: env.ml
 * author: Robert Muller
 *
 * CSCI 3366 Programming Languages
 *
 * This file contains the environment code for miniC.
 *)

module M = Map.Make(Symbol)

type key = Symbol.t
type 'a t = 'a M.t

let empty = M.empty
let add = M.add
let find = M.find
let mem = M.mem
let iter = M.iter
let bindings = M.bindings

let keyFormat = Symbol.format

(* make : 'a list -> 'a t *)
let make values =
  let folder map (key, value) = add key value map in
  let keyValuePairs = List.combine Basis.primOps values
  in
  List.fold_left folder empty keyValuePairs

let toString stringer map =
  let bindings = M.bindings map in
  let folder s (key, value) =
    Lib.fmt "%s = %s; %s" (Symbol.format key) (stringer value) s
  in
  Lib.fmt "{%s}" (List.fold_left folder "" bindings)
