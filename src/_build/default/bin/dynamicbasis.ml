(* file: dynamicbasis.ml
 * author: Bob Muller
 * revised as of 2/2019
 *)

module Opn = Mips.Operation
module Opnd = Mips.Operand
module Instr = Mips.Instruction
module CS = Mips.Codestream

let toInstr = Instr.toInstruction
let fIL = CS.fromInstructionList

(* The code generator maintains an environment for looking up
   information about identifiers.

   + When the identifier is a primitive operator, the code generator
     would like to retrieve a codegenerator for that operator.

   + When the identifier is a variable (or temporary), the code generator
     would like to retrieve the storage offset for that variable.  
*)
type t = CodeGenerator of (Opnd.t list -> Mips.Codestream.t)
       | Offset of int

(* NB: The order of these code generators must agree with the ordering 
   of the primitive operations in the Basis module and with the types
   in the Staticbasis module.
*)
let operatorCodeGenerators =
  [
    (* + *)
    (function
      | [rd; rs; rt] -> fIL [toInstr(None, Opn.Add {rd; rs; rt}, None)]
      | _ -> failwith "Dynamicbasis: bad call of the + codegen.")
  
  (* - *)
  ; (function
    | [rd; rs; rt] -> fIL [toInstr(None, Opn.Sub {rd; rs; rt}, None)]
    | _ -> failwith "Dynamicbasis: bad call of the - codegen.");
  
  (* * *)
  (function
    | [rd; rs; rt] -> fIL [toInstr(None, Opn.Mul {rd; rs; rt}, None)]
    | _ -> failwith "Dynamicbasis: bad call of the * codegen.");
  
  (* / *)
  (function
    | [rd; rs; rt] -> fIL [toInstr(None, Opn.Div {rs; rt}, None);
                           toInstr(None, Opn.Mflo rd, None)]
    | _ -> failwith "Dynamicbasis: bad call of the / codegen.");
  
  (* % *)
  (function
    | [rd; rs; rt] -> fIL [toInstr(None, Opn.Div {rs; rt}, None);
                           toInstr(None, Opn.Mfhi rd, None)]
    | _ -> failwith "Dynamicbasis: bad call of the % codegen.");
  
  (* **  EXP OPERATOR NOT IMPLEMENTED *)
  (function
    | [] -> fIL [toInstr(None, Opn.Nop, Some "**: NOT IMPLEMENTED")]
    | _ -> failwith "Dynamicbasis: bad call of the ** codegen.");
  
  (* < *)
  (function
    | [rd; rs; rt] -> fIL [toInstr(None, Opn.Slt {rd; rs; rt}, None)]
    | _ -> failwith "Dynamicbasis: bad call of the < codegen.");
  
  (* <= *)
  (function
    | [rd; rs; rt] ->
      fIL [ toInstr(None, Opn.Sub  {rd; rs; rt}, Some "<=")
          ; toInstr(None, Opn.Slti {rd; rs = rd; const16 = Opnd.Const16 1}, None)
          ]
    | _ -> failwith "Dynamicbasis: bad call of the <= codegens.");
  
  (* == *)
  (function
    | [rd; rs; rt] ->
      fIL [ toInstr(None, Opn.Li   {rd; imm32 = Opnd.Const16 0}, None)
          ; toInstr(None, Opn.Sub  {rd = rs; rs; rt}, Some "==")
          ; toInstr(None, Opn.Li   {rd = rt; imm32 = Opnd.Const16 1}, None)
          ; toInstr(None, Opn.Movz {rd; rs = rt; rt = rs}, None)
          ]
    | _ -> failwith "Dynamicbasis: bad call of the == codegen.");
  
  (* <> *)
  (function
    | [rd; rs; rt] ->
      fIL [ toInstr(None, Opn.Li   {rd; imm32 = Opnd.Const16 0}, None)
          ; toInstr(None, Opn.Sub  {rd = rs; rs; rt}, Some "<>")
          ; toInstr(None, Opn.Li   {rd = rt; imm32 = Opnd.Const16 1}, None)
          ; toInstr(None, Opn.Movn {rd = rd; rs = rt; rt = rs}, None)
          ]
    | _ -> failwith "Dynamicbasis: bad call of the <> codegen.");
  
  (* >= *)
  (function
    | [rd; rs; rt] ->
      fIL [toInstr(None, Opn.Sub  {rd = rd; rs = rt; rt = rs}, Some ">=");
           toInstr(None, Opn.Slti {rd = rd; rs = rd; const16 = Opnd.Const16 1}, None)]
    | _ -> failwith "Dynamicbasis: bad call of the >= codegen.");
  
  (* > *)
  (function
    | [rd; rs; rt] ->
      fIL [toInstr(None, Opn.Slt {rd; rs = rt; rt = rs}, Some ">")]
    | _ -> failwith "Dynamicbasis: bad call of the > codegen.");
  
  (* not
  *)
  (* not right *)
      (*
      (function
        | [rd; rs] ->fIL [toInstr(None, Opn.Not {rd; rs}, Some "not")]
        | other -> failwith "Dynamicbasis: bad call of the - codegen.")
       *)
  (* follows the true = 1, false = 0 notion *)
  (function
    | [rd; rs] ->
      fIL [ toInstr(None,
                    Opn.Slti {rd; rs; const16 = Opnd.Const16 1},
                    Some "not")
          ]
    | _ -> failwith "Dynamicbasis: bad call of the not codegen.")
]

let codeGenerators =
  List.map (fun cg -> CodeGenerator cg) operatorCodeGenerators
