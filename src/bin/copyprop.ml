(*
 * file: copyprop.sml
 * author: Bob Muller
 * date: 4-12-2015.
 *
 * The copyprop transformation does the following:
 *
 * let x = e1 in let y = x in e2 =copyprop=> let x = e1 in e2[y:=x]
 *)
let substitute x (*for*) y term =
  let rec translateTerm term =
    match term with
    | (Ast.Id z as i) ->
      (match Symbol.compare y z with
       | 0 -> Ast.Id x
       | _ -> i)
    | Ast.Literal _ as w -> w

    | Ast.If {expr; thn; els} ->
      Ast.If { expr = translateTerm expr
             ; thn  = translateTerm thn
             ; els  = translateTerm els
             }

(* The following two cases can't happen because Or and And were eliminated in
   the naming phase.
*)
    | Ast.Or _  -> raise (Failure "Cannot happen.")
    | Ast.And _ -> raise (Failure "Cannot happen.")

    | Ast.App {rator; rands} ->
      Ast.App {rator = rator; rands = List.map translateTerm rands}

    | Ast.Let {decl = Ast.ValBind {bv; defn}; body} ->
      Ast.Let {decl = Ast.ValBind {bv = bv; defn = translateTerm defn};
               body = translateTerm body}
  in
  translateTerm term

let rec translate (Ast.Program procedures) =
  Ast.Program (List.map translateProcedure procedures)

and
  translateProcedure (Ast.Procedure {id; formals; typ; body}) =
  Ast.Procedure { id
                ; formals
                ; typ
                ; body = translateStatement body
                }
and
  translateStatement statement = match statement with
  | Ast.Block {decls; statements} ->
    Ast.Block { decls
              ; statements = List.map translateStatement statements
              }
  | Ast.Assign {id; expr} ->
    Ast.Assign { id
               ; expr = translateTerm expr
               }
  | Ast.While {expr; statement} ->
    Ast.While { expr = translateTerm expr
              ; statement = translateStatement statement
              }
  | Ast.IfS {expr; thn; els} ->
    Ast.IfS { expr = translateTerm expr
            ; thn = translateStatement thn
            ; els = translateStatement els
            }
  | Ast.Call {rator; rands} ->
    Ast.Call { rator
             ; rands = List.map translateTerm rands
             }
  | Ast.Print term -> Ast.Print (translateTerm term)
  | Ast.Return term -> Ast.Return (translateTerm term)

and
  translateTerm term =
  match term with
  | Ast.Id _ -> term
  | Ast.Literal _ -> term
  | Ast.App {rator; rands} ->
    Ast.App {rator; rands = List.map translateTerm rands}
  | Ast.If {expr; thn; els} ->
    Ast.If { expr = translateTerm expr
           ; thn = translateTerm thn
           ; els = translateTerm els
           }
  | Ast.And _ -> failwith "There are no And nodes"
  | Ast.Or  _ -> failwith "There are no Or nodes"

  | Ast.Let {decl; body} ->
    (match decl with
     | Ast.ValBind {bv = Ast.{id; typ=_}; defn = Ast.Id keep} ->
       translateTerm (substitute keep id body)
     | Ast.ValBind _ ->
       Ast.Let {decl; body = translateTerm body})
