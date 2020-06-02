(* file: mips.ml
  author: Bob Muller
  date: 1-2-2009
  revised: January 2016

  This module is a representation of the MIPS instruction set.
  It consists of 4 submodules:

  1. Operand
  2. Operation
  3. Instruction
  4. Codestream.
*)
let fmt = Printf.sprintf

module Operand =
struct

  type reg = Value of int
           | Arg of int
           | Temp of int
           | CalleeSave of int
           | Zero
           | GlobalPointer     (* callee-save *)
           | StackPointer
           | FramePointer
           | ReturnAddress

  let formatReg reg =
    match reg with
    | Value n -> fmt "v%s" (string_of_int n)
    | Arg n -> fmt "a%s" (string_of_int n)
    | Temp n -> fmt "t%s" (string_of_int n)
    | CalleeSave n -> fmt "s%s" (string_of_int n)
    | Zero -> "zero"
    | GlobalPointer -> "gp"
    | ReturnAddress -> "ra"
    | StackPointer -> "sp"
    | FramePointer -> "fp"

  type t = Const8 of int
         | Const16 of int
         | Const32 of int
         | Label of Label.t
         | String of string
         | Reg of reg
         | Indirect of {offset : int option; reg : reg}

  let format opnd =
    match opnd with
    | Const8 n  -> string_of_int n
    | Const16 n -> string_of_int n
    | Const32 n -> string_of_int n
    | Label l -> Label.format l
    | String s -> fmt "\"%s\"" s
    | Reg r -> fmt "$%s" (formatReg r)
    | Indirect {offset = None; reg} -> fmt "($%s)" (formatReg reg)
    | Indirect {offset = Some n; reg} ->
      fmt "%s($%s)" (string_of_int n) (formatReg reg)
end

module Operation =
struct
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

(* format code.  The first three functions are local helpers.
 *)
  let fmt1 fmtstring opnd =
    fmt fmtstring (Opnd.format opnd)

  let fmt2 fmtstring op1 op2 =
    fmt fmtstring (Opnd.format op1) (Opnd.format op2)

  let fmt3 fmtstring op1 op2 op3 =
    fmt fmtstring (Opnd.format op1) (Opnd.format op2) (Opnd.format op3)

  let format inst =
    match inst with

    (* Arithmetic
    *)
    | Add   {rd; rs; rt}      -> fmt3 "add\t%s, %s, %s" rd rs rt
    | Addi  {rd; rs; const16} -> fmt3 "addi\t%s, %s, %s" rd rs const16
    | Addiu {rd; rs; const16} -> fmt3 "addiu\t%s, %s, %s" rd rs const16
    | Addu  {rd; rs; rt}      -> fmt3 "addu\t%s, %s, %s" rd rs rt
    | Clo   {rd; rs}          -> fmt2 "clo\t%s, %s" rd rs
    | Clz   {rd; rs}          -> fmt2 "clz\t%s, %s" rd rs
    | La    {rd; label}       -> fmt2 "la\t%s, %s" rd label
    | Li    {rd; imm32}       -> fmt2 "li\t%s, %s" rd imm32
    | Liu   {rd; const16}     -> fmt2 "liu\t%s, %s" rd const16
    | Move  {rd; rs}          -> fmt2 "move\t%s, %s" rd rs
    | Negu  {rd; rs}          -> fmt2 "negu\t%s, %s" rd rs
    | Seb   {rd; rs}          -> fmt2 "seb\t%s, %s" rd rs
    | Seh   {rd; rs}          -> fmt2 "seh\t%s, %s" rd rs
    | Sub   {rd; rs; rt}      -> fmt3 "sub\t%s, %s, %s" rd rs rt
    | Subu  {rd; rs; rt}      -> fmt3 "subu\t%s, %s, %s" rd rs rt

    (* Shift and Rotate Operations
     *)
    | Rotr {rd; rs; bits5} -> fmt3 "rotr\t%s, %s, %s" rd rs bits5
    | Rotrv   {rd; rs; rt} -> fmt3 "rotrv\t%s, %s, %s" rd rs rt
    | Sll {rd; rs; shift5} -> fmt3 "sll\t%s, %s, %s" rd rs shift5
    | Sllv    {rd; rs; rt} -> fmt3 "sllv\t%s, %s, %s" rd rs rt
    | Sra {rd; rs; shift5} -> fmt3 "sra\t%s, %s, %s" rd rs shift5
    | Srav   {rd; rs; rt}  -> fmt3 "srav\t%s, %s, %s" rd rs rt
    | Srl {rd; rs; shift5} -> fmt3 "srl\t%s, %s, %s" rd rs shift5
    | Srlv    {rd; rs; rt} -> fmt3 "srlv\t%s, %s, %s" rd rs rt

    (* Logical and Bitfield Operations
     *)
    | And  {rd; rs; rt}      -> fmt3 "and\t%s, %s, %s" rd rs rt
    | Andi {rd; rs; const16} -> fmt3 "and\t%s, %s, %s" rd rs const16
    | Nop                    -> "nop"
    | Nor   {rd; rs; rt}     -> fmt3 "nor\t%s, %s, %s" rd rs rt
    | Not       {rd; rs}     -> fmt2 "not\t%s, %s" rd rs
    | Or    {rd; rs; rt}     -> fmt3 "or\t%s, %s, %s" rd rs rt
    | Ori  {rd; rs; const16} -> fmt3 "ori\t%s, %s, %s" rd rs const16
    | Xor   {rd; rs; rt}     -> fmt3 "xor\t%s, %s, %s" rd rs rt
    | Xori {rd; rs; const16} -> fmt3 "xori\t%s, %s, %s" rd rs const16

    (* Condition Testing and Conditional Move.
     *)
    | Movn       {rd; rs; rt} -> fmt3 "movn\t%s, %s, %s" rd rs rt
    | Movz       {rd; rs; rt} -> fmt3 "movz\t%s, %s, %s" rd rs rt
    | Slt        {rd; rs; rt} -> fmt3 "slt\t%s, %s, %s" rd rs rt
    | Slti  {rd; rs; const16} -> fmt3 "slti\t%s, %s, %s" rd rs const16
    | Sltiu {rd; rs; const16} -> fmt3 "sltiu\t%s, %s, %s" rd rs const16
    | Sltu       {rd; rs; rt} -> fmt3 "sltu\t%s, %s, %s" rd rs rt

    (* Multiply and Divide.
     *)
    | Div      {rs; rt} -> fmt2 "div\t%s, %s" rs rt
    | Divu     {rs; rt} -> fmt2 "divu\t%s, %s" rs rt
    | Madd     {rs; rt} -> fmt2 "madd\t%s, %s" rs rt
    | Maddu    {rs; rt} -> fmt2 "maddu\t%s, %s" rs rt
    | Msub     {rs; rt} -> fmt2 "msub\t%s, %s" rs rt
    | Msubu    {rs; rt} -> fmt2 "msubu\t%s, %s" rs rt
    | Mul  {rd; rs; rt} -> fmt3 "mul\t%s, %s, %s" rd rs rt
    | Mult     {rs; rt} -> fmt2 "mult\t%s, %s" rs rt
    | Multu    {rs; rt} -> fmt2 "multu\t%s, %s" rs rt

    (* Accumulator Access Operations.
     *)
    | Mfhi rd -> fmt1 "mfhi\t%s" rd
    | Mflo rd -> fmt1 "mflo\t%s" rd
    | Mthi rs -> fmt1 "mthi\t%s" rs
    | Mtlo rs -> fmt1 "mtlo\t%s" rs

    (* Jumps and Branches
     *)
    | B      off18   ->         fmt1 "b\t%s" off18
    | Bal    off18 ->           fmt1 "bal\t%s" off18
    | Beq    {rs; rt; off18} -> fmt3 "beq\t%s, %s, %s" rs rt off18
    | Beqz   {rs; off18} ->     fmt2 "beqz\t%s, %s" rs off18
    | Bgez   {rs; off18} ->     fmt2 "bgez\t%s, %s" rs off18
    | Bgezal {rs; off18} ->     fmt2 "bgezal\t%s, %s" rs off18
    | Bgtz   {rs; off18} ->     fmt2 "bgtz\t%s, %s" rs off18
    | Blez   {rs; off18} ->     fmt2 "blez\t%s, %s" rs off18
    | Bltz   {rs; off18} ->     fmt2 "bltz\t%s, %s" rs off18
    | Bltzal {rs; off18} ->     fmt2 "bltzal\t%s, %s" rs off18
    | Bne    {rs; rt; off18} -> fmt3 "bne\t%s, %s, %s" rs rt off18
    | Bnez   {rs; off18} ->     fmt2 "bnez\t%s, %s" rs off18
    | J      addr28 ->          fmt1 "j\t%s" addr28
    | Jal    addr28 ->          fmt1 "jal\t%s" addr28
    | Jalr   {rd; rs} ->        fmt2 "jalr\t%s, %s" rd rs
    | Jr     off18 ->           fmt1 "jr\t%s" off18

    (* Load and Store Operations
     *)
    | Lb  {rd; rs} -> fmt2 "lb\t%s, %s" rd rs
    | Lbu {rd; rs} -> fmt2 "lbu\t%s, %s" rd rs
    | Lh  {rd; rs} -> fmt2 "lh\t%s, %s" rd rs
    | Lhu {rd; rs} -> fmt2 "lhu\t%s, %s" rd rs
    | Lw  {rd; rs} -> fmt2 "lw\t%s, %s" rd rs
    | Lwl {rd; rs} -> fmt2 "lwl\t%s, %s" rd rs
    | Lwr {rd; rs} -> fmt2 "lwr\t%s, %s" rd rs

    | Sb  {rs; rt} -> fmt2 "sb\t%s, %s"  rs rt
    | Sh  {rs; rt} -> fmt2 "sh\t%s, %s"  rs rt
    | Sw  {rs; rt} -> fmt2 "sw\t%s, %s"  rs rt
    | Swl {rs; rt} -> fmt2 "Swl\t%s, %s" rs rt
    | Swr {rs; rt} -> fmt2 "Swr\t%s, %s" rs rt
    | Ulw {rd; rs} -> fmt2 "Ulw\t%s, %s" rd rs
    | Usw {rs; rt} -> fmt2 "Usw\t%s, %s" rs rt

    (* System Calls.
    *)
    | Syscall -> "syscall"

    (* Pseudo-ops
     *)
    | Byte x   -> fmt1 ".byte\t%s" x
    | Word x   -> fmt1 ".word\t%s" x
    | Space x  -> fmt1 ".space\t%s" x
    | Text     -> ".text"
    | Data     -> ".data"
    | Asciiz s -> fmt1 ".asciiz\t%s" s
end

module Instruction =
struct
  module Opn = Operation

  type t = { label : Label.t option
           ; operation : Opn.t
           ; comment : string option
           }

  let toInstruction(lab, opn, comment) : t = { label = lab
                                             ; operation = opn
                                             ; comment = comment
                                             }

  let instructionLabel(i : t)     = i.label
  let instructionOperation(i : t) = i.operation
  let instructionComment(i : t)   = i.comment

  let alignComment = function
      Some y when y < 9 -> "\t\t\t\t"
    | Some y when y < 12 -> "\t\t\t"
    | _ -> "\t\t";;

  let format instr =
    let opnfmt = Opn.format instr.operation
    in
    match instr with
    | {label = None; operation=_; comment = None} -> fmt "\t%s" opnfmt

    | {label = None; operation=_; comment = Some comment} ->
      let tabs = alignComment (Some(String.length opnfmt))
      in
      fmt "\t%s%s# %s" opnfmt tabs comment

    | {label = Some l; operation=_; comment = None} ->
      let labfmt = Label.format l
      in
      fmt "%s:\t%s" labfmt opnfmt

    | {label = Some l; operation=_; comment = Some comment} ->
      let tabs = alignComment(Some (String.length opnfmt)) in
      let labfmt = Label.format l
      in
      fmt "%s:\t%s%s# %s" labfmt opnfmt tabs comment
end

module Codestream =
struct
  type t = Instruction.t list

  let concat : t -> t -> t = (@)
  let fromInstructionList : Instruction.t list -> t = (fun lst -> lst)
  let empty : t = []

  let emit (filename, codestream) =
    let objOut = Util.makeFile (filename, "asm") in
    let date = Util.dateAndTime () in
    let signature = fmt "# Generated by %s: %s.\n#\n" Util.compilerName date in
    let rec loop instructions =
      match instructions with
      | [] -> ()
      | instruction :: instructions ->
        (
          output_string objOut ((Instruction.format instruction) ^ "\n");
          loop instructions
        )
    in
    (
      output_string objOut signature;
      loop codestream;
      close_out(objOut)
    )
end
