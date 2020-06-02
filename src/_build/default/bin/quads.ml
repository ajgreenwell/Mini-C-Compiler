(* file: quads.ml
   author: Bob Muller
   date: March 2, 2009
   reworked/cleaned up: January 2016
   extended by Art Zhu: April, 2017
   revised: February 2019

   This file contains the definition of an intermediate language
   suitable for the explication of control flow for mini-C programs.
*)
let fmt = Printf.sprintf

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

let formatOperand opnd =
  match opnd with
  | Id s  -> Symbol.format s
  | Word {typ = _; bits} -> string_of_int bits

let formatCommaSeparated formatter items =
  let rec repeat items =
    match items with
    | [] -> ""
    | [item] -> formatter item
    | item :: items -> Lib.fmt "%s, %s" (formatter item) (repeat items)
  in
  repeat items

(* formats a list of comma-separated operands
*)
let formatOperands opnds = formatCommaSeparated formatOperand opnds

let formatRHS rhs =
  match rhs with
  | Operand op -> formatOperand op

  | BinPrimOp {op; opnds = {src1; src2}} ->
    let ops = Symbol.format op in
    let src1s = formatOperand src1 in
    let src2s = formatOperand src2
    in
    Lib.fmt "%s %s %s" src1s ops src2s

  | UnPrimOp {op; opnd} ->
    let ops = Symbol.format op in
    let opnds = formatOperand opnd
    in
    Lib.fmt "%s %s" ops opnds

  | FunCall {label; opnds} ->
    let labels = Label.format label in
    let opndss = formatOperands opnds
    in
    Lib.fmt "call %s(%s)" labels opndss

let formatOperation opn =
  match opn with
  | Gets {dst; src} ->
    let dsts = formatOperand dst in
    let srcs = formatRHS src
    in
    Lib.fmt "%s = %s" dsts srcs

  | Jmp label -> fmt "jmp %s" (Label.format label)

  | JmpZero {cond; dest} ->
    let conds = formatRHS cond in
    let dests = Label.format dest
    in
    fmt "jmpzero %s, %s" conds dests

  | Ret opnd -> "return " ^ (formatOperand opnd)

  | Print rhs -> "print " ^ (formatRHS rhs)

  | Noop -> "nop"

  | Call {label; opnds} ->
    let labels = Label.format label in
    let opndss = formatOperands opnds
    in
    fmt "call %s(%s)" labels opndss

let formatInstruction (Instruction {label = maybeLabel; op}) =
  let opns = formatOperation op
  in
  match maybeLabel with
  | None -> fmt "\t%s\n" opns
  | Some label ->
    let labels = Label.format label
    in
    fmt "%s:\t%s\n" labels opns

let formatProcedure (Procedure {entry; formals; code}) =
  let formals = formatCommaSeparated Symbol.format formals in
  let entrys = Label.format entry in
  let header = fmt "\n%s:\t(%s)\n" entrys formals
  in
  header ^ (List.fold_left (^) "" (List.map formatInstruction code))

let dumpInstructionStream (Program procedures) =
  let print = output_string !Debug.dbgout
  in
  List.iter (fun procedure -> print (formatProcedure procedure)) procedures
