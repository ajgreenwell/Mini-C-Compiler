module A = Ast
module Q = Quads
(* name: operation and function calls only on variables *)
(* lift: no nested lets *)

type transExprOut = { instructions : Q.instruction list
                    ; result : Q.rhs
                    }

let mark label = [Q.Instruction {label = Some label; op = Q.Noop}]
let jump label = [Q.Instruction {label = None; op = Q.Jmp label}]

let terms2opnds terms =
  List.map
    (fun term ->
       match term with
       | A.Id sym -> Q.Id sym
       | _ -> failwith "Control.terms2opnds: Should not happen"
    )
    terms

let invoke rator terms =
  let name = Label.fromString (Symbol.format rator)
  in
  { instructions = []
  ; result = Q.FunCall { label = name
                       ; opnds = terms2opnds terms
                       }
  }

let initialize decls =
  List.map
    (fun A.{id; typ} ->
       match typ with
       | Typ.Int ->
         [ Q.Instruction { label = None
                         ; op = Q.Gets { dst = Q.Id id
                                       ; src = Q.Operand (Q.Word { typ
                                                                 ; bits = 0
                                                                 })
                                       }
                         }
         ]
       | Typ.Bool ->
         [ Q.Instruction { label = None
                         ; op = Q.Gets { dst = Q.Id id
                                       ; src = Q.Operand (Q.Word { typ
                                                                 ; bits = 0
                                                                 })
                                       }
                         }
         ]
       | _ -> [])
    decls

let rec translate (A.Program procedures) =
  Q.Program (List.map translateProcedure procedures)

and translateProcedure (A.Procedure {id; typ=_; formals; body}) =
  let entry = Label.fromString (Symbol.format id) in
  let formals = List.map A.(fun bv -> bv.id) formals in
  let code = translateStatement body
  in
  Q.Procedure {entry; formals; code}

and translateStatement ast =
  match ast with
  | A.Block {decls; statements} ->
    List.concat (List.concat ( (initialize decls)
                               :: [List.map translateStatement statements]
                             ))
  | A.Assign {id; expr} ->
    let {instructions; result} = translateExpression expr
    in
    instructions @ [ Q.Instruction { label = None
                                   ; op = Q.Gets { dst = Q.Id id
                                                 ; src = result
                                                 }
                                   }
                   ]
  | A.While {expr; statement} ->
    let {instructions; result} = translateExpression expr in
    let startL = Label.fresh() in
    let endL = Label.fresh() in
    let switch = [ Q.Instruction { label = None
                                 ; op = Q.JmpZero { cond = result
                                                  ; dest = endL
                                                  }
                                 }
                 ] in
    List.concat ( (mark startL)
                  :: instructions
                  :: switch
                  :: (translateStatement statement)
                  :: (jump startL)
                  :: [mark endL]
                )

  | A.IfS {expr; thn; els} ->
    let {instructions; result} = translateExpression expr in
    let switchL = Label.fresh() in
    let endL = Label.fresh() in
    let switch = [ Q.Instruction { label = None
                                 ; op = Q.JmpZero { cond = result
                                                  ; dest = switchL
                                                  }
                                 }
                 ] in
    List.concat ( instructions
                  :: switch
                  :: (translateStatement thn)
                  :: (jump endL)
                  :: (mark switchL)
                  :: (translateStatement els)
                  :: [mark endL]
                )

  | A.Call {rator; rands} ->
    let rands = List.map translateExpression rands in
    let instructions =
      List.concat (List.map (fun x -> x.instructions) rands) in
    let oList = List.map (fun _ -> Q.Id (Symbol.fresh())) rands in
    let passId =
      List.map2 (fun x y -> Q.Instruction { label = None
                                          ; op = Q.Gets{ dst = x
                                                       ; src = y.result
                                                       }
                                          }) oList rands in
    let name = Label.fromString (Symbol.format rator) in
    List.concat ( instructions
                  :: passId
                  :: [[Q.Instruction { label = None
                                     ; op = Q.Call { label = name
                                                   ; opnds = oList
                                                   }
                                     }
                      ]])

  | A.Print expr ->
    let expr = translateExpression expr in
    let resultId = Q.Id (Symbol.fresh()) in
    let passId = [ Q.Instruction { label = None
                                 ; op = Q.Gets { dst = resultId
                                               ; src = expr.result
                                               }
                                 }
                 ] in
    let print = [ Q.Instruction { label = None
                                ; op = Q.Print (Q.Operand resultId)
                                }
                ] in
    List.concat ( expr.instructions
                  :: passId
                  :: [print]
                )

  | A.Return expr ->
    let expr = translateExpression expr in
    let resultId = Q.Id (Symbol.fresh()) in
    let passId = [ Q.Instruction { label = None
                                 ; op = Q.Gets { dst = resultId
                                               ; src = expr.result
                                               }
                                 }
                 ] in
    let return = [Q.Instruction {label = None; op = Q.Ret resultId}] in
    List.concat (expr.instructions :: passId :: [return])

and translateExpression expr =
  match expr with
  | A.Id sym -> { instructions = []
                ; result = Q.Operand (Q.Id sym)
                }
  | A.Literal {typ; bits} ->
    { instructions = []
    ; result = Q.Operand (Q.Word {typ; bits})
    }
  | A.App {rator; rands = (A.Id sym1)::[A.Id sym2] as terms} ->
    (match Basis.isPrim rator with
     | true -> { instructions = []
               ; result = Q.BinPrimOp { op = rator
                                      ; opnds = { Q.src1 = Q.Id sym1
                                                ; Q.src2 = Q.Id sym2
                                                }
                                      }
               }
     | false -> invoke rator terms)
  | A.App {rator; rands = [A.Id sym] as terms} ->
    (match Basis.isPrim rator with
     | true -> { instructions = []
               ; result = Q.UnPrimOp { op = rator
                                     ; opnd = Q.Id sym
                                     }
               }
     | false -> invoke rator terms)
  | A.App {rator; rands} -> invoke rator rands

  | A.If {expr; thn; els} ->
    let expr = translateExpression expr in
    let thn  = translateExpression thn in
    let els  = translateExpression els in
    let resultId = Q.Id (Symbol.fresh()) in
    let switchL = Label.fresh() in
    let endL = Label.fresh() in
    let switch = [ Q.Instruction { label = None
                                 ; op = Q.JmpZero { cond = expr.result
                                                  ; dest = switchL
                                                  }
                                 }
                 ] in
    let passId texpr = [ Q.Instruction { label = None
                                       ; op = Q.Gets { dst = resultId
                                                     ; src = texpr.result
                                                     }
                                       }
                       ] in
    { instructions = List.concat ( expr.instructions
                                   :: switch
                                   :: thn.instructions
                                   :: (passId thn)
                                   :: (jump endL)
                                   :: (mark switchL)
                                   :: els.instructions
                                   :: (passId els)
                                   :: [mark endL]
                                 )
    ; result = Q.Operand resultId
    }

  | A.And _ ->
    failwith "Control.translateExpr: Ast.And eliminated by Name"
  | A.Or _ ->
    failwith "Control.translateExpr: Ast.Or eliminated by Name"

  | A.Let {decl = A.ValBind {bv; defn}; body} ->
    let defn = translateExpression defn in
    let body = translateExpression body in
    let passId = [ Q.Instruction { label = None
                                 ; op = Q.Gets { dst = Q.Id bv.id
                                               ; src = defn.result
                                               }
                                 }
                 ] in
    { instructions = List.concat ( defn.instructions
                                   :: passId
                                   :: [body.instructions]
                                 )
    ; result = body.result
    }
