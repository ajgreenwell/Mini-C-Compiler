(* file: staticbasis.ml
 * author: R. Muller
 * revised as of 2/19
*)
let intCrossInt2Int =
  Typ.Arrow { from = Typ.Product [Typ.Int; Typ.Int]
            ; too  = Typ.Int
            }

let intCrossInt2Bool =
  Typ.Arrow { from = Typ.Product [Typ.Int; Typ.Int]
            ; too  = Typ.Bool
            }

let bool2Bool = Typ.Arrow { from = Typ.Bool
                          ; too  = Typ.Bool
                          }

(* NB: The order of these types must agree with the ordering of
   the primitive operations in the Basis module and with the code
   generators in the Dynamicbasis module.
*)
let operatorTypes = [ intCrossInt2Int   (* + *)
                    ; intCrossInt2Int   (* - *)
                    ; intCrossInt2Int   (* * *)
                    ; intCrossInt2Int   (* / *)
                    ; intCrossInt2Int   (* % *)
                    ; intCrossInt2Int   (* ** *)
                    ; intCrossInt2Bool  (* < *)
                    ; intCrossInt2Bool  (* <= *)
                    ; intCrossInt2Bool  (* == *)
                    ; intCrossInt2Bool  (* <> *)
                    ; intCrossInt2Bool  (* >= *)
                    ; intCrossInt2Bool  (* > *)
                    ; bool2Bool         (* not *)
                    ]
