(* file: codegen.ml
 * author: Bob Muller
 * revised: March/April 2017, by Art Zhu
 * revised: February, 2019 by Bob Muller
 *
 * This file contains code for inserting control in the compilation
 * of mini-C. This code translates from the language of quads to
 * MIPS assembly code. The calling protocols is as follows:
 *
 * Register Conventions:
 *  t0 - t9 : caller-save regs
 *  s0 - s7 : callee-save regs
 *  fp : frame pointer
 *  ra : return address
 *  a0 - a3 : argument registers for first 4 arguments
 *  v0 - v1 : return values
 *
 * 1. Stack Frame Layout:

       +----------------------------+
       |      Caller-Save Regs      |                 higher addresses
       +----------------------------+
       |       Value of Arg 1       |
       +--           :            --+
       |             :              |
       +--                        --+
       |       Value of Arg n       |
       +----------------------------+
       |   Caller's Return Address  |
       +----------------------------+
       |   Caller's Frame Pointer   | <-- fp
       +----------------------------+
       |      Local Variable 1      |
       +--            :           --+
       |              :             |
       +--                        --+
       |      Local Variable k      |
       +----------------------------+
       |   Callee Save Registers    |                 lower addresses
       +----------------------------+
                                      <-- sp
 *)

(* Some abbreviations for frequently used modules. *)

module Q = Quads
module Opn = Mips.Operation
module Opnd = Mips.Operand
module I = Mips.Instruction
module CS = Mips.Codestream

let fIL = CS.fromInstructionList
let toIn = I.toInstruction

let offsetsOf cgenv =
  let bindings = Env.bindings cgenv in
  let rec loop bindings =
    match bindings with
    | [] -> []
    | (key, Dynamicbasis.Offset n) :: bindings ->
      (key, n) :: loop bindings
    | _ :: bindings -> loop bindings
  in
  loop bindings

(* The dynamicBasis is an environment mapping built-in operators
   to code generator functions. This environment will also be used
   to store storage offsets for identifiers, including formal
   parameters as well as local and temporary variables.
*)
let dynamicBasis = Env.make Dynamicbasis.codeGenerators

(* Conventions
 *
 *   This section binds names to enforce some code generator conventions.
 *
 * Word size is 4 bytes.
 *)
let wordSize = 4

(* Conventions for system calls on MARS. These codes go in $v0.
 *)
let syscallPrintInt = 1
(* let syscallReturn = 10 *)
let syscallReturnWithVal = 17

(* accumulator register used to accumulate results.
 *)
let accumulator =  Opnd.Value 0
let operand1Reg =  Opnd.Temp 1
let operand2Reg =  Opnd.Temp 2

(* dataBaseAddr is the register that is used to to hold the base
 * address of the data area. All variables accessed via indirect
 * addressing off of this register.
 *)
(* let dataBaseAddr = Opnd.Temp 1 *)

(* targetLabelReg is the number of the register that is used to
 * hold the destination of a branch.
 *)
(* let targetLabelReg = Opnd.Temp 2 *)

let isMain label = label = Label.fromString "main"

let push reg maybeLabel comment =
  let i1 = toIn (maybeLabel,
                 Opn.Addi { rd = Opnd.Reg Opnd.StackPointer
                          ; rs = Opnd.Reg Opnd.StackPointer
                          ; const16 = Opnd.Const16 (-4)
                          },
			           Some ("push " ^ comment)) in
  let i2 = toIn (None,
                 Opn.Sw { rs = reg
                        ; rt = Opnd.Indirect { offset = Some 0
                                             ; reg = Opnd.StackPointer
                                             }
                        },
                 None)
  in
  fIL [i1; i2]

let pushRA maybeLabel = push (Opnd.Reg Opnd.ReturnAddress) maybeLabel ""

let pop dstreg comment =
  let i1 = toIn (None,
                 Opn.Lw { rd = dstreg
                        ; rs = Opnd.Indirect { offset = Some 0
                                             ; reg = Opnd.StackPointer
                                             }
                        },
			           Some ("pop " ^ comment)) in
  let i2 = toIn (None,
                 Opn.Addi { rd = Opnd.Reg Opnd.StackPointer
                          ; rs = Opnd.Reg Opnd.StackPointer
                          ; const16 = Opnd.Const16 4
                          },
                 None)
  in
  fIL [i1; i2]

let calleePrologue name nLocals =
  let pushFP = push (Opnd.Reg Opnd.FramePointer) (Some name) "fp" in
  let sp2fp = toIn (None,
                    Opn.Move { rd = Opnd.Reg Opnd.FramePointer
                             ; rs = Opnd.Reg Opnd.StackPointer
                             },
                    Some "fp <- sp") in
  let allocate =
    toIn (None,
          Opn.Addi { rd = Opnd.Reg Opnd.StackPointer
                   ; rs = Opnd.Reg Opnd.StackPointer
                   ; const16 = Opnd.Const16 (-wordSize * nLocals)
                   },
          Some "allocate locals")
  in
  CS.concat pushFP (fIL [sp2fp; allocate])

let calleeEpilogue entry =
  let restoreSP = toIn (None,
                        Opn.Move { rd = Opnd.Reg Opnd.StackPointer
                                 ; rs = Opnd.Reg Opnd.FramePointer
                                 },
                        None) in
  let restoreFP = pop (Opnd.Reg Opnd.FramePointer) "restore fp" in
  let return =
    match isMain entry with
    | true  -> [ toIn (None,
                       Opn.Li { rd = Opnd.Reg (Opnd.Value 0)
                              ; imm32 = Opnd.Const32 syscallReturnWithVal
                              },
                       Some "$v0 gets exit code for syscall")
               ; toIn (None, Opn.Syscall, Some "Exit here")
               ]
    | false -> [ toIn(None,
                      Opn.Jr (Opnd.Reg Opnd.ReturnAddress),
                      Some "return")
               ]
  in
  CS.concat (fIL [restoreSP]) (CS.concat restoreFP (fIL return))

(* makeEnv constructs an environment for a given procedure. The
 * environment maps each identifier to its storage offset as suggested
 * by the picture at the top of the file. In particular, all variables
 * are accessed via indirect addressing using the frame pointer (fp) as
 * the base address. Formal parameters will have positive offsets while
 * local variables will have negative offsets.
*)

let getLocalFromInstruction (Q.Instruction {label=_; op}) =
  match op with
  | Q.Gets {dst; src=_} ->
    (match dst with
     | Q.Id id -> Some id
     | _ -> failwith "Unexpected Quads.Word on lhs of Quads.Gets")
  | _ -> None

let rec extractLocals maybeLocals =
  match maybeLocals with
  | (Some local) :: maybeLocals ->
    local :: extractLocals maybeLocals
  | None :: maybeLocals ->
    extractLocals maybeLocals
  | _ -> []

let removeDuplicates xs ys =
  let uniqueCons x xs =
    if (List.mem x xs) || (List.mem x ys)
    then xs
    else x :: xs
  in
  List.fold_right uniqueCons xs []

let getLocals instructions formals =
  let maybeLocals = List.map getLocalFromInstruction instructions in
  let locals = extractLocals maybeLocals
  in
  removeDuplicates locals formals

(* let makeEnv formals instructions = Env.empty *)
let makeEnv formals instructions =
  let rec makeFormalsEnv formals offset =
    match formals with
    | f :: fs ->
      let almostEnv = makeFormalsEnv fs (offset + 1) in
      let offset = Dynamicbasis.Offset offset in
      Env.add f offset almostEnv
    | _ -> dynamicBasis in

  let formalsEnv = makeFormalsEnv (List.rev formals) 2 in

  let rec makeLocalsEnv locals offset =
    match locals with
    | l :: ls ->
      let (numLocals, almostEnv) = makeLocalsEnv ls (offset - 1) in
      let offset = Dynamicbasis.Offset offset in
      (numLocals + 1, Env.add l offset almostEnv)
    | _ -> (0, formalsEnv) in

  let locals = getLocals instructions formals
  in
  makeLocalsEnv locals (-1)

let lookupOffset name env =
  match Env.find name env with
  | Dynamicbasis.Offset i -> i
  | _ -> failwith "lookupOffset: something is wrong with codegen env"

let lookupCodeGenerator name env =
  match Env.find name env with
  | Dynamicbasis.CodeGenerator cg -> cg
  | _ -> failwith "lookupCodeGenerator: something is wrong with codegen env"

let loadRegister reg env opnd maybeLabel =
  match opnd with
  | Q.Id name ->
    let index = lookupOffset name env in
    let offset = wordSize * index
    in
    toIn(maybeLabel,
         Opn.Lw { rd = Opnd.Reg reg
                ; rs = Opnd.Indirect { offset = Some offset
                                     ; reg = Opnd.FramePointer
                                     }
                },
         None)

  | Q.Word {typ = _; bits} ->
    toIn(maybeLabel,
         Opn.Li { rd = Opnd.Reg reg
                ; imm32 = Opnd.Const32 bits
                },
         None)

let storeRegister reg env opnd maybeLabel =
  match opnd with
  | Q.Id name ->
    let index = lookupOffset name env in
    let offset = wordSize * index
    in
    fIL [ toIn(maybeLabel,
               Opn.Sw { rs = Opnd.Reg reg
                      ; rt = Opnd.Indirect { offset = Some offset
                                           ; reg = Opnd.FramePointer
                                           }
                      },
               None)
        ]
  | _ -> failwith "storeRegister: bad store operand"


let callerPrologue opnds maybeLabel env =
  let pushArgs =
    let pushArg maybeLabel opnd =
      CS.concat
        (fIL [loadRegister operand1Reg env opnd maybeLabel])
        (push (Opnd.Reg operand1Reg) None "")
    in
    match opnds with
    | [] -> pushRA maybeLabel
    | opnd :: opnds ->
      CS.concat
        (let cs1 = pushArg maybeLabel opnd
         in
         List.fold_left CS.concat cs1 (List.map (pushArg None) opnds))
        (pushRA None)
  in
  pushArgs

let callerEpilogue opnds =
  CS.concat
    (pop (Opnd.Reg Opnd.ReturnAddress) "ra")
    (fIL [
        toIn (
          None,
          Opn.Addi { rd = Opnd.Reg Opnd.StackPointer
                   ; rs = Opnd.Reg Opnd.StackPointer
                   ; const16 = Opnd.Const16 (wordSize * (List.length opnds))
                   },
          Some "deallocate args")
      ])


      
let concatCodestreams codestreams =
  List.fold_left CS.concat (List.hd codestreams) (List.tl codestreams)

let concatProcedureCodestreams procedures =
  let isMainProcedure (name, _) = isMain name in
  let isNotMainProcedure (name, _) = not (isMain name) in
  let (_, main) = List.find isMainProcedure procedures in
  let procedures = List.filter isNotMainProcedure procedures in
  let procedures = List.map (fun (_, p) -> p) procedures
  in
  concatCodestreams (main :: procedures)

let translateFunctionCall env callLabel opnds maybeLabel =
  let prologue = callerPrologue opnds maybeLabel env in
  let jal = fIL [toIn (None, Opn.Jal (Opnd.Label callLabel), None)] in
  let epilogue = callerEpilogue opnds
  in
  concatCodestreams [prologue; jal; epilogue]
let translateRHS env rhs maybeLabel reg =
  match rhs with
  | Q.Operand opnd ->
    fIL [translateOperand reg env opnd maybeLabel]

  | Q.BinPrimOp {op; opnds = {src1; src2}} ->
    let opnd1Instruction = translateOperand operand1Reg env src1 maybeLabel in
    let opnd2Instruction = translateOperand operand2Reg env src2 maybeLabel in
    let opndsCodestream = fIL [opnd1Instruction; opnd2Instruction] in
    let codeGenerator = lookupCodeGenerator op env in
    let opCodestream = codeGenerator [ Opnd.Reg reg
                                     ; Opnd.Reg operand1Reg
                                     ; Opnd.Reg operand2Reg
                                     ]
    in
    concatCodestreams [opndsCodestream; opCodestream]

  | Q.UnPrimOp {op; opnd} ->
    let opndInstruction = translateOperand operand1Reg env opnd maybeLabel in
    let opndCodestream = fIL [opndInstruction] in
    let codeGenerator = lookupCodeGenerator op env in
    let opCodestream = codeGenerator [Opnd.Reg reg; Opnd.Reg operand1Reg]
    in
    concatCodestreams [opndCodestream; opCodestream]

  | Q.FunCall {label; opnds} ->
    translateFunctionCall env label opnds maybeLabel

let translateOperation env opn maybeLabel procedureLabel =
  match opn with
  | Q.Gets {dst; src} ->
    let rhs = translateRHS env src maybeLabel accumulator in
    let sw = storeRegister accumulator env dst None
    in
    CS.concat rhs sw

  | Q.Jmp label ->
    fIL [toIn (maybeLabel, Opn.J (Opnd.Label label), Some "Jmp")]

  | Q.JmpZero {cond; dest} ->
    let cond = translateRHS env cond maybeLabel accumulator in
    let rs = Opnd.Reg accumulator in
    let off18 = Opnd.Label dest in
    let comment = Some "JmpZero" in
    let beqz = fIL [toIn (maybeLabel, Opn.Beqz {rs; off18}, comment)]
    in
    CS.concat cond beqz

  | Q.Call {label; opnds} ->
    translateFunctionCall env label opnds maybeLabel

  | Q.Ret opnd ->
    let reg = if isMain procedureLabel then Opnd.Arg 0 else accumulator
    in
    fIL [translateOperand reg env opnd maybeLabel]

  | Q.Print rhs ->
    let rhs = translateRHS env rhs None (Opnd.Arg 0) in
    let rd = Opnd.Reg accumulator in
    let imm32 = Opnd.Const32 syscallPrintInt in
    let print =
      fIL [ toIn (None, Opn.Li {rd; imm32}, None)
          ; toIn (None, Opn.Syscall, Some "Print")
          ]
    in
    CS.concat rhs print

  | Q.Noop -> fIL [toIn (maybeLabel, Opn.Nop, None)]

let translateInstruction env procedureLabel (Q.Instruction {label; op}) =
  translateOperation env op label procedureLabel

let translateProcedure (Q.Procedure {entry; formals; code}) =
  let (numLocals, env) = makeEnv formals code in
  let prologue = calleePrologue entry numLocals in
  let instructions = List.map (translateInstruction env entry) code in
  let instructions = concatCodestreams instructions in
  let epilogue = calleeEpilogue entry
  in
  Debug.dumpCGEnv entry (offsetsOf env);
  (entry, concatCodestreams [prologue; instructions; epilogue])
let translate (Q.Program procedures) =
  let procedures = List.map translateProcedure procedures in
  let procedures = concatProcedureCodestreams procedures in
  let initialInstructions = fIL ([ toIn (None, Opn.Data, None)
                                 ; toIn (None, Opn.Text, None)
                                 ])
  in
  CS.concat initialInstructions procedures
