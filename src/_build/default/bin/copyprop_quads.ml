(* file: copyprop_quads.ml
 * author: Art Zhu (?)
 * revised: February 2019 (by R. Muller)
 *
   NB: THIS CODE ISN'T WORKING!
*)

module Q = Quads

let substOperand opnd oldId newId =
  match opnd with
  | Q.Id sym ->
    (match Symbol.compare sym oldId = 0 with
     | true  -> Q.Id newId
     | false -> opnd)
  | Q.Word _ -> opnd

let substRHS rhs oldId newId =
  match rhs with
  | Q.Operand opnd -> Q.Operand (substOperand opnd oldId newId)

  | Q.BinPrimOp {op; opnds = {src1; src2}} ->
    Q.BinPrimOp { op
                ; opnds = { src1 = substOperand src1 oldId newId
                          ; src2 = substOperand src2 oldId newId
                          }
                }
  | Q.UnPrimOp {op; opnd} ->
    Q.UnPrimOp { op
               ; opnd = substOperand opnd oldId newId
               }
  | Q.FunCall {label; opnds} ->
    let substOperand opnd = substOperand opnd oldId newId
    in
    Q.FunCall { label
              ; opnds = List.map substOperand opnds
              }

let substInstruction (Q.Instruction {label; op} as instr) oldId newId =
  match op with
  | Q.Gets{dst; src} ->
    let dst = substOperand dst oldId newId in
    let src = substRHS src oldId newId
    in
    Q.Instruction { label
                  ; op = Q.Gets {dst; src}
                  }
  | Q.Jmp _ -> instr

  | Q.JmpZero {cond; dest} ->
    let cond = substRHS cond oldId newId
    in
    Q.Instruction { label
                  ; op = Q.JmpZero {cond; dest}
                  }
  | Q.Call {label = dstLabel; opnds} ->
    let opnds = List.map (fun opnd -> substOperand opnd oldId newId) opnds
    in
    Q.Instruction { label
                  ; op = Q.Call {label = dstLabel; opnds}
                  }
  | Q.Ret operand ->
    Q.Instruction { label
                  ; op = Q.Ret (substOperand operand oldId newId)
                  }
  | Print rhs ->
    Q.Instruction { label
                  ; op = Q.Print (substRHS rhs oldId newId)
                  }
  | Q.Noop -> instr

let substitute instructions oldId newId =
  List.map (fun instr -> substInstruction instr oldId newId) instructions

let rec translateInstructions instructions =
  match instructions with
  | [] -> []
  | Q.Instruction { label
                  ; op = Q.Gets { dst = Q.Id oldId
                                ; src = Q.Operand (Q.Id newId)
                                }
                  } :: instructions ->
    let instructions = substitute instructions oldId newId
    in
    translateInstructions instructions
  | instruction :: instructions ->
    instruction :: translateInstructions instructions

let translateProcedure (Q.Procedure {entry; formals; code}) =
  Q.Procedure { entry
              ; formals
              ; code = translateInstructions code
              }

let translate procedures = List.map translateProcedure procedures

(*****************************************************************)

let rmNoopProcedure (Q.Procedure {entry; formals; code}) =
  let noopFilter (Q.Instruction {label; op}) =
    match (label, op) with
    | (None, Q.Noop) -> false
    | _ -> true
  in
  Q.Procedure { entry
                  ; formals
                  ; code = List.filter noopFilter code
                  }

let rmNoop instructionstream = List.map rmNoopProcedure instructionstream

let rec translate instructionstream =
  let translate_and_concat translated_stream procedure =
    let t = translateProcedure procedure
    in
    List.append translated_stream [t]
  in
  List.fold_left translate_and_concat [] instructionstream

and
  translateProcedure (Q.Procedure {entry; formals; code}) =
  Q.Procedure { entry
                  ; formals
                  ; code = translateInstructionList Env.empty [] code
                  }
and
  translateInstructionList env translated_code code =
  match code with
  | [] -> translated_code
  | instruction :: rest ->
    let (ret_env, ret_instr) = translateInstruction env instruction
    in
    translateInstructionList ret_env (translated_code @ [ret_instr]) rest
and
  translateInstruction env (Q.Instruction {label; op}) =
  let (ret_env, op) = translateOperation env op
  in
  (ret_env, Q.Instruction {label; op})
and
  translateOperation env op =
  match op with
  | Q.Gets {dst; src} ->
    (match dst with
     | Q.Word _ -> assert false
     | Q.Id lhs ->
       let (ret_env ,translated_src) =
         translateRHS env (Some lhs) src
       in
       match translated_src with
       | None -> (ret_env, Q.Noop)
       | Some src -> (ret_env, Q.Gets{dst; src}))

  | Q.Jmp lbl -> (env, op)

  | Q.JmpZero {cond; dest} ->
    let cond =
      (match cond with
       | Q.Operand operand ->
         Q.Operand (translateOperand env operand)
       | _ ->
         Lib.pfmt "JmpZero (cond : Q.rhs) should be operand only.\n" ;
         assert false)
    in
    (env, Q.JmpZero {cond; dest})

  | Q.Call {label; opnds} ->
    (env, Q.Call { label
                     ; opnds = List.map (translateOperand env) opnds})

  | Q.Ret operand ->
    (env, Q.Ret (translateOperand env operand))
  | Q.Print rhs ->
    (match (translateRHS env None rhs) with
     | (_, None) -> assert false
     | (_, Some trhs) ->
       (env, Q.Print trhs))
  | Q.Noop -> (env, op)

and
  translateRHS env (maybeLHS : Symbol.t option) (rhs : Q.rhs) =
    let (ret_env, translated_src) = (
      match rhs with
      | Q.Operand ((Q.Id rhs_sym) as r) ->
        (match maybeLHS with
         | None -> (env, Some (Q.Operand (translateOperand env r)))
         | Some lhs_sym ->
           let left = Symbol.format lhs_sym in
           let right = Symbol.format rhs_sym
           in
           Lib.pfmt "replace %s as %s\n" left right ;
           (union lhs_sym rhs_sym env, None))

      | Q.Operand (Q.Word w) -> (env, Some rhs)

      | Q.BinPrimOp {op; opnds = Q.{src1; src2}} ->
        let opnds = Q.{ src1 = translateOperand env src1
                          ; src2 = translateOperand env src2
                          }
        in
        (env, Some (Q.BinPrimOp {op; opnds}))

      | Q.UnPrimOp {op; opnd} ->
        (env, Some (Q.UnPrimOp { op
                                   ; opnd = (translateOperand env opnd)
                                   }))
      | Q.FunCall {label; opnds} ->
        let opnds = List.map (translateOperand env) opnds
        in
        (env, Some (Q.FunCall {label; opnds}))
    ) in
    (ret_env, translated_src)
and
  translateOperand env (operand : Q.operand) =
  match operand with
  | Q.Word _ -> operand
  | Q.Id id ->
    (match Env.mem id env with
     | false -> operand
     | true -> Q.Id (Env.find id env))
and
  union key value env =
    let v = (match Env.mem value env with
             | true  -> Env.find value env
             | false -> value)
    in
    Env.add key v env
