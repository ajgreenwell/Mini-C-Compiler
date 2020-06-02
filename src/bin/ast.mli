(* file: ast.mli
   author: Bob Muller

   CSCI 3366 Principles of Programming Languages
*)
type binding = { id : Symbol.t
               ; typ : Typ.t
               }

type program = Program of procedure list

and procedure = Procedure of { id : Symbol.t
                             ; formals : binding list
                             ; typ : Typ.t
                             ; body : statement
                             }

and statement = Block of { decls : binding list
                         ; statements : statement list
                         }
              | Assign of { id : Symbol.t
                          ; expr : term
                          }
              | While of { expr : term
                         ; statement : statement
                         }
              | IfS of { expr : term
                       ; thn : statement
                       ; els : statement
                       }
              | Call of { rator : Symbol.t
                        ; rands : term list
                        }
              | Print of term
              | Return of term

and term = Id of Symbol.t
         | Literal of { typ : Typ.t
                      ; bits : int
                      }
         | App of { rator : Symbol.t
                  ; rands : term list
                  }    (* term is an identifier *)
         | If of { expr : term
                 ; thn : term
                 ; els : term
                 }
         | And of { left : term
                  ; right : term
                  }
         | Or of { left : term
                 ; right : term
                 }
         | Let of { decl : declaration
                  ; body : term
                  }
and
  declaration = ValBind of { bv : binding
                           ; defn : term
                           }

and
  value = LiteralValue of { typ : Typ.t
                          ; bits : int
                          }
        | BinaryOp of (value * value -> value)
        | UnaryOp of (value -> value)

val toStringList : 'a list -> ('a -> string) -> string
val toString : term -> string
val toStringValue : value -> string

val pp : program -> unit
val ppv : value -> unit
val ppStream : out_channel -> program -> unit
val ppFile : string -> program -> unit
val setIndentLevel : int -> unit
