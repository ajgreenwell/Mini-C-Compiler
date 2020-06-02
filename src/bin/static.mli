val typeCheck : Typ.t Env.t -> Ast.program -> unit

exception TypeError of string
