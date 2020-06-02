(* file: quads.mli
   author: Bob Muller
   date: March 2, 2009
   reworked/cleaned up: January 2016
   extended by Art Zhu: April, 2017
   revised: February 2019
*)
type operand = Id of Symbol.t
             | Word of { typ : Typ.t
                       ; bits : int
                       }

type operands = { src1 : operand
                ; src2 : operand
                }

type rhs = Operand of operand
         | BinPrimOp of { op    : Basis.t
                        ; opnds : operands
                        }
         | UnPrimOp  of { op   : Basis.t
                        ; opnd : operand
                        }
         | FunCall of { label : Label.t
                      ; opnds : operand list
                      }

type operation = Gets of { dst : operand
                         ; src : rhs
                         }
               | Jmp of Label.t
               | JmpZero of { cond : rhs      (* jump when rhs is false *)
                            ; dest : Label.t
                            }
               | Call of { label : Label.t
                         ; opnds : operand list
                         }
               | Ret of operand
               | Print of rhs                 (* Added by Art Zhu *)
               | Noop

type instruction = Instruction of { label : Label.t option
                                  ; op : operation
                                  }

type procedure   = Procedure of { entry   : Label.t
                                ; formals : Symbol.t list
                                ; code    : instruction list
                                }

type program = Program of (procedure list)


val dumpInstructionStream : program -> unit
