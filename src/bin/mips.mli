(* file: mips.mli
 * author: Bob Muller
 * date: 1-9-2009
 *
 * This is the interface file for the representation of the MIPS architecture.
*)

module Operand :
  sig
    type reg = Value of int
             | Arg of int
             | Temp of int
             | CalleeSave of int
	           | Zero              (* $0 is always 0 *)
	           | GlobalPointer     (* callee-save *)
	           | StackPointer      (* callee-save *)
	           | FramePointer      (* callee-save *)
	           | ReturnAddress;;

    type t = Const8 of int
           | Const16 of int
           | Const32 of int
           | Label of Label.t
           | String of string
           | Reg of reg
           | Indirect of {offset : int option; reg : reg}  (* either ($t0) or e.g., 8($t0) *)

    val format : t -> string
  end

module Operation :
  sig
    module Opnd = Operand
    type opnd = Opnd.t

    type t =

      (* Arithmetic
      *)
      | Add   of {rd : opnd; rs : opnd; rt : opnd}
      | Addi  of {rd : opnd; rs : opnd; const16 : opnd}
      | Addiu of {rd : opnd; rs : opnd; const16 : opnd}
      | Addu  of {rd : opnd; rs : opnd; rt : opnd}
      | Clo   of {rd : opnd; rs : opnd}
      | Clz   of {rd : opnd; rs : opnd}
      | La    of {rd : opnd; label : opnd}
      | Li    of {rd : opnd; imm32 : opnd}
      | Liu   of {rd : opnd; const16 : opnd}
      | Move  of {rd : opnd; rs : opnd}
      | Negu  of {rd : opnd; rs : opnd}
      | Seb   of {rd : opnd; rs : opnd}
      | Seh   of {rd : opnd; rs : opnd}
      | Sub   of {rd : opnd; rs : opnd; rt : opnd}
      | Subu  of {rd : opnd; rs : opnd; rt : opnd}

      (* Shift and Rotate Operations
      *)
      | Rotr  of {rd : opnd; rs : opnd; bits5 : opnd}
      | Rotrv of {rd : opnd; rs : opnd; rt : opnd}
      | Sll   of {rd : opnd; rs : opnd; shift5 : opnd}
      | Sllv  of {rd : opnd; rs : opnd; rt : opnd}
      | Sra   of {rd : opnd; rs : opnd; shift5 : opnd}
      | Srav  of {rd : opnd; rs : opnd; rt : opnd}
      | Srl   of {rd : opnd; rs : opnd; shift5 : opnd}
      | Srlv  of {rd : opnd; rs : opnd; rt : opnd}

      (* Logical and Bitfield Operations
      *)
      | And  of {rd : opnd; rs : opnd; rt : opnd}
      | Andi of {rd : opnd; rs : opnd; const16 : opnd}
      | Nop
      | Nor  of {rd : opnd; rs : opnd; rt : opnd}
      | Not  of {rd : opnd; rs : opnd}
      | Or   of {rd : opnd; rs : opnd; rt : opnd}
      | Ori  of {rd : opnd; rs : opnd; const16 : opnd}
      | Xor  of {rd : opnd; rs : opnd; rt : opnd}
      | Xori of {rd : opnd; rs : opnd; const16 : opnd}

      (* Condition Testing and Conditional Move.
      *)
      | Movn  of {rd : opnd; rs : opnd; rt : opnd}
      | Movz  of {rd : opnd; rs : opnd; rt : opnd}
      | Slt   of {rd : opnd; rs : opnd; rt : opnd}
      | Slti  of {rd : opnd; rs : opnd; const16 : opnd}
      | Sltiu of {rd : opnd; rs : opnd; const16 : opnd}
      | Sltu  of {rd : opnd; rs : opnd; rt : opnd}

      (* Multiply and Divide.
      *)
      | Div   of {rs : opnd; rt : opnd}
      | Divu  of {rs : opnd; rt : opnd}
      | Madd  of {rs : opnd; rt : opnd}
      | Maddu of {rs : opnd; rt : opnd}
      | Msub  of {rs : opnd; rt : opnd}
      | Msubu of {rs : opnd; rt : opnd}
      | Mul   of {rd : opnd; rs : opnd; rt : opnd}
      | Mult  of {rs : opnd; rt : opnd}
      | Multu of {rs : opnd; rt : opnd}

      (* Accumulator Access Operations.
      *)
      | Mfhi of opnd                      (* load from hi *)
      | Mflo of opnd                      (* move from low *)
      | Mthi of opnd                      (* move to high *)
      | Mtlo of opnd                      (* move to low *)

      (* Jumps and Branches
      *)
      | B      of opnd
      | Bal    of opnd
      | Beq    of {rs : opnd; rt : opnd; off18 : opnd}
      | Beqz   of {rs : opnd; off18 : opnd}
      | Bgez   of {rs : opnd; off18 : opnd}
      | Bgezal of {rs : opnd; off18 : opnd}
      | Bgtz   of {rs : opnd; off18 : opnd}
      | Blez   of {rs : opnd; off18 : opnd}
      | Bltz   of {rs : opnd; off18 : opnd}
      | Bltzal of {rs : opnd; off18 : opnd}
      | Bne    of {rs : opnd; rt : opnd; off18 : opnd}
      | Bnez   of {rs : opnd; off18 : opnd}
      | J      of opnd
      | Jal    of opnd
      | Jalr   of {rd : opnd; rs : opnd}
      | Jr     of opnd

      (* Load and Store Operations
      *)
      | Lb of  {rd : opnd; rs : opnd}  (* NB: rs will be an indirect opnd *)
      | Lbu of {rd : opnd; rs : opnd}
      | Lh of  {rd : opnd; rs : opnd}
      | Lhu of {rd : opnd; rs : opnd}
      | Lw of  {rd : opnd; rs : opnd}
      | Lwl of {rd : opnd; rs : opnd}
      | Lwr of {rd : opnd; rs : opnd}

      | Sb of  {rs : opnd; rt : opnd}
      | Sh of  {rs : opnd; rt : opnd}
      | Sw of  {rs : opnd; rt : opnd}
      | Swl of {rs : opnd; rt : opnd}
      | Swr of {rs : opnd; rt : opnd}

      | Ulw of {rd : opnd; rs : opnd}
      | Usw of {rs : opnd; rt : opnd}

      (* System Calls.
      *)
      | Syscall

      (* Pseudo-ops
      *)
      | Byte of opnd          (* NB: Operand must be a Const8. *)
      | Word of opnd
      | Space of opnd
      | Text
      | Data
      | Asciiz of opnd        (* NB: Operand must be a string. *)

    val format : t -> string

  end;;

module Instruction :
  sig
    type t

    val toInstruction : Label.t option * Operation.t * string option -> t
    val instructionLabel : t -> Label.t option
    val instructionOperation : t -> Operation.t
    val instructionComment : t -> string option

    val format : t -> string
  end;;

module Codestream :
  sig
    type t

    val fromInstructionList : Instruction.t list -> t
    val empty : t
    val concat : t -> t -> t
    val emit : string * t -> unit
  end;;
