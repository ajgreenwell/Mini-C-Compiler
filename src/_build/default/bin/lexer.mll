(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of integer,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET),
   or EOF.  It skips all blanks and tabs, unknown characters. *)
{
  open Parser (* Assumes the parser file is "parser.mly". *)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl;;

  let keyword_table =
    create_hashtable 11 [
      ("print", PRINT);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("and", AND);
      ("or", OR);
      ("not", NOT);
      ("True", TRUE);
      ("False", FALSE);
      ("while", WHILE);
      ("return", RETURN)
    ];;
}

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase
rule token = parse
  | (' ' | '\t' | '\n')*   { token lexbuf }
  | '#' [^ '\n']* '\n'     { token lexbuf }
  | digit+ as n            { NUM (int_of_string n) }
  | ("int" | "bool" | "void") as typ { TYPE typ }
  | (letter | '_') (letter | digit | '_')* as word
                           { try
			       let token = Hashtbl.find keyword_table word in
				 token
			     with Not_found -> (ID word)
			   }
  | "()"             { UNIT }
  | ','              { COMMA }
  | ';'              { SEMI }
  | ':'              { COLON }
  | '?'              { QUESTION }
  | '='              { GETS }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '{'              { LBRACE }
  | '}'              { RBRACE }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '*'              { MULTIPLY }
  | '/'              { DIVIDE }
  | '%'              { MOD }
  | "**"             { CARET }
  | "<"              { LT }
  | "<="             { LE }
  | "=="             { EQ }
  | "!="             { NE }
  | "<>"             { NEALT }
  | ">="             { GE }
  | ">"              { GT }
  | eof              { EOF }
