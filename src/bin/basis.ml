(* file: basis.ml
 * author: R. Muller
 * date: 4-4-2002, converted from SML to Ocaml on 1-8-2009.
 *
 * This file contains the code for building both the static and dynamic basis.
 * The function makeBasis will construct a static basis when applied to the
 * list Bases.primOpTypes.  The same function will construct a dynamic
 * basis when applied to the list implementationsOfPrimitives.   These bases
 * are constructed in the preamble to the top-level read-eval-print loop in
 * Lx.sml.  The apply functions can used to actually put a type checker for (or
 * implementation of) a primitive to use.
 *
 * To extend the basis with new primitives, the list primNames must be
 * extended with an appropriate identifier.
 *)

(*
 * This is the master list of names of primitive operators.
 *
 * NB: THESE NAMES ARE LAYED OUT IN A FIXED ORDER!
 *)
let primOpNames = [ "+"
                  ; "-"
                  ; "*"
                  ; "/"
                  ; "%"
                  ; "**"
                  ; "<"
                  ; "<="
                  ; "=="
                  ; "<>"
                  ; ">="
                  ; ">"
                  ; "not"
                  ]

let primOps = List.map Symbol.fromString primOpNames

type t = Symbol.t

let isPrim id = List.mem id primOps
