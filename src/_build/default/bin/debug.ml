(* file: debug.ml
   author: Bob Muller
   date: 3-2-2009

   Various debugging stuff.

 * When !debug, debugging information is printed to file.dbg.
 *)
let debug = ref true

let dbgout = ref stdout

let debugLexer = ref false

let debugOut(out, printer, message, term) =
  ( output_string out (Lib.fmt "\n%s\n\n" message)
  ; printer out term
  )

let debugInfo(ouch, str, term) =
  (if !debug then
     debugOut(ouch, Ast.ppStream, str, term)
   else ())

let debugSetup fname =
  if !debug then
    let dbgOut = Util.makeFile(fname,"dbg") in
    let _ = dbgout := dbgOut in
    let dbgName = Util.makeFileName(fname, "dbg") in
    let message = Lib.fmt "Compiling %s in debug mode." fname in
    let _ = Lib.pfmt "\n%s See file %s.\n" message dbgName in
    let date = Util.dateAndTime() in
    let prefix = Lib.fmt "%s: %s. " Util.compilerName date in
    let _ = output_string dbgOut (Lib.fmt "%s%s\n" prefix message)
    in
    dbgOut
  else
    stdout

(* The following is for debugging the lexer. *)

let format = function
    Parser.NUM n -> string_of_int n
  | Parser.ID s -> s
  | Parser.TYPE t -> t
  | Parser.SEMI     -> ";"
  | Parser.COLON -> ":"
  | Parser.QUESTION -> "?"
  | Parser.COMMA     -> ","
  | Parser.LPAREN     -> "("
  | Parser.RPAREN     -> ")"
  | Parser.LBRACE     -> "{"
  | Parser.RBRACE     -> "}"
  | Parser.TRUE     -> "True"
  | Parser.FALSE     -> "False"
  | Parser.IF     -> "if"
  | Parser.THEN     -> "then"
  | Parser.ELSE     -> "else"
  | Parser.AND     -> "and"
  | Parser.OR     -> "or"
  | Parser.NOT     -> "not"
  | Parser.PRINT     -> "print"
  | Parser.WHILE     -> "while"
  | Parser.RETURN     -> "return"
  | Parser.PLUS     -> "+"
  | Parser.MINUS     -> "-"
  | Parser.MULTIPLY     -> "*"
  | Parser.DIVIDE     -> "/"
  | Parser.UNIT     -> "()"
  | Parser.MOD     -> "%"
  | Parser.CARET     -> "**"
  | Parser.LT     -> "<"
  | Parser.LE     -> "<="
  | Parser.EQ     -> "=="
  | Parser.NE     -> "!="
  | Parser.NEALT     -> "<>"
  | Parser.GE     -> ">="
  | Parser.GT     -> ">"
  | Parser.GETS     -> "="
  | Parser.EOF     -> "EOF"

let rec dumpTokens lexer lexbuf =
  match lexer lexbuf with
  | Parser.EOF -> print_string "EOF\n"
  | other ->
    print_string ((format other) ^ "\n") ;
	  dumpTokens lexer lexbuf

let dumpEnv tenv =
  let printer id typ =
    let ids  = Env.keyFormat id in
    let typs = Typ.format typ
    in
    output_string !dbgout (Lib.fmt "%s : %s\n" ids typs)
  in
  output_string !dbgout "\nType environment is:\n" ;
  Env.iter printer tenv

let dumpCGEnv procedure bindings =
  let procedure = Label.format procedure in
  let printer (id, offset) =
    let id = Env.keyFormat id in
    let offset = string_of_int offset
    in
    output_string !dbgout (Lib.fmt "%s : %s\n" id offset)
  in
  output_string !dbgout (Lib.fmt "\nCG environment for %s is:\n" procedure);
  List.iter printer bindings
