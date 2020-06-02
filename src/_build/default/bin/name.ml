(* file: name.ml
 * author: Bob Muller
 * date: January 5, 2009
 * revised: March, 2017
 *
 * The Name module implements a source-to-source transformation for
 * naming the values of subterms in miniC. In addition to naming values,
 * the Name module translates "or" and "and" forms into special-cases
 * of the conditional form nested within a let-form.
*)
let createValBinds bvs defns =
  let bvsAndDefns = Util.zip (bvs, defns)
  in
  List.map (fun (bv, defn) -> Ast.ValBind {bv; defn}) bvsAndDefns

let createLetSequence valbinds finalLet =
  let rec loop decls =
    match decls with
    | [] -> finalLet
    | decl :: decls -> Ast.Let {decl; body = loop decls}
  in
  loop valbinds

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
  translateStatement statement = (* THIS IS A STUB, YOUR CODE HERE *)
  match statement with
  | Ast.Block {decls; statements} ->
    let statements = List.map translateStatement statements
    in
    Ast.Block {decls; statements}

  | Ast.Assign {id; expr} ->
    let expr = translateTerm expr
    in
    Ast.Assign {id; expr}

  | Ast.While {expr; statement} ->
    let expr = translateTerm expr in
    let statement = translateStatement statement
    in
    Ast.While {expr; statement}

  | Ast.IfS {expr; thn; els} ->
    let expr = translateTerm expr in
    let thn = translateStatement thn in
    let els = translateStatement els
    in
    Ast.IfS {expr; thn; els}

  | Call {rator; rands} ->
    let rands = List.map translateTerm rands
    in
    Call {rator; rands}

  | Print expr ->
    let expr = translateTerm expr
    in
    Print expr

  | Return expr ->
    let expr = translateTerm expr
    in
    Return expr

and
  translateTerm term =
  match term with
  | Ast.Id _ as id -> id

  | Ast.Literal _ as literal -> literal

  | Ast.App {rator; rands} ->
    let ids = List.map (fun _ -> Symbol.fresh ()) rands in
    let bvs = List.map (fun id -> Ast.{id; typ = Typ.Int}) ids in
    let rands = List.map translateTerm rands in
    let valbinds = createValBinds bvs rands in
    let id = Symbol.fresh () in
    let bv = Ast.{id; typ = Typ.Int} in
    let ids = List.map (fun id -> Ast.Id id) ids in
    let finalLet =
      Ast.Let { decl = Ast.ValBind { bv
                                   ; defn = Ast.App { rator
                                                    ; rands = ids}
                                   }
              ; body = Ast.Id id
              }
    in
    createLetSequence valbinds finalLet

  | Ast.If {expr; thn; els} ->
    let defn = translateTerm expr in
    let thn = translateTerm thn in
    let els = translateTerm els in
    let id = Symbol.fresh() in
    let bv = Ast.{id; typ = Typ.Int} in
    let expr = Ast.Id id
    in
    Ast.Let { decl = Ast.ValBind { bv
                                 ; defn
                                 }
            ; body = Ast.If { expr
                            ; thn
                            ; els
                            }
            }

  | Ast.Or {left; right} ->         (* FREE CODE Removes OR *)
    let x = Symbol.fresh () in
    let expr = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Bool}
    in
    Ast.Let { decl = Ast.ValBind { bv
                                 ; defn = translateTerm left
                                 }
            ; body = Ast.If { expr
                            ; thn = expr
                            ; els = translateTerm right
                            }
            }

  | Ast.And {left; right} ->      (* FREE CODE Removes AND *)
    let x = Symbol.fresh () in
    let expr = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Bool}
    in
    Ast.Let { decl = Ast.ValBind { bv
                                 ; defn = translateTerm left
                                 }
            ; body = Ast.If { expr
                            ; thn = translateTerm right
                            ; els = expr
                            }
            }

  | Ast.Let _ ->
    failwith "Let forms aren't supported in the abstract syntax of miniC"
