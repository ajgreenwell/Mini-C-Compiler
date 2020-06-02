# 6 "bin/lexer.mll"
 
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

# 26 "bin/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\227\255\003\000\030\000\031\000\236\255\237\255\004\000\
    \239\255\240\255\241\255\242\255\243\255\033\000\246\255\247\255\
    \248\255\249\255\055\000\078\000\160\000\235\000\054\001\129\001\
    \114\000\002\000\254\255\139\001\214\001\033\002\108\002\183\002\
    \002\003\250\255\232\255\235\255\230\255\233\255\231\255\229\255\
    ";
  Lexing.lex_backtrk =
   "\000\000\255\255\027\000\255\255\021\000\255\255\255\255\017\000\
    \255\255\255\255\255\255\255\255\255\255\010\000\255\255\255\255\
    \255\255\255\255\011\000\004\000\004\000\004\000\004\000\002\000\
    \255\255\000\000\255\255\004\000\003\000\004\000\004\000\004\000\
    \004\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default =
   "\255\255\000\000\255\255\255\255\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \024\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\025\000\025\000\025\000\025\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \025\000\003\000\025\000\024\000\000\000\005\000\000\000\000\000\
    \018\000\012\000\007\000\009\000\017\000\008\000\035\000\006\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\015\000\016\000\004\000\013\000\002\000\014\000\
    \039\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\038\000\037\000\036\000\034\000\019\000\
    \033\000\019\000\021\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\022\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\020\000\019\000\
    \019\000\019\000\019\000\011\000\026\000\010\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\019\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\000\000\000\000\000\000\000\000\019\000\
    \001\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\031\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\000\000\000\000\
    \000\000\000\000\019\000\000\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\029\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\019\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\027\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\000\000\000\000\
    \000\000\000\000\019\000\000\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\028\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\019\000\000\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \030\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000\
    \000\000\000\000\000\000\019\000\000\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \028\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\000\000\000\000\000\000\000\000\019\000\000\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \032\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
    \000\000\019\000\000\000\019\000\019\000\019\000\028\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\025\000\025\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\025\000\000\000\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\004\000\004\000\013\000\000\000\
    \018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\024\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\255\255\255\255\255\255\255\255\019\000\255\255\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\255\255\255\255\255\255\255\255\020\000\
    \000\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\255\255\255\255\
    \255\255\255\255\021\000\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \255\255\255\255\024\000\255\255\255\255\255\255\255\255\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\255\255\255\255\255\255\255\255\022\000\255\255\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\255\255\255\255\
    \255\255\255\255\027\000\255\255\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\255\255\255\255\255\255\255\255\028\000\255\255\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\255\255\255\255\255\255\255\255\
    \029\000\255\255\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\255\255\
    \255\255\255\255\255\255\030\000\255\255\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\255\255\255\255\255\255\255\255\031\000\255\255\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\255\255\255\255\255\255\
    \255\255\032\000\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 35 "bin/lexer.mll"
                           ( token lexbuf )
# 330 "bin/lexer.ml"

  | 1 ->
# 36 "bin/lexer.mll"
                           ( token lexbuf )
# 335 "bin/lexer.ml"

  | 2 ->
let
# 37 "bin/lexer.mll"
              n
# 341 "bin/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 37 "bin/lexer.mll"
                           ( NUM (int_of_string n) )
# 345 "bin/lexer.ml"

  | 3 ->
let
# 38 "bin/lexer.mll"
                                 typ
# 351 "bin/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 38 "bin/lexer.mll"
                                     ( TYPE typ )
# 355 "bin/lexer.ml"

  | 4 ->
let
# 39 "bin/lexer.mll"
                                              word
# 361 "bin/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 40 "bin/lexer.mll"
                           ( try
			       let token = Hashtbl.find keyword_table word in
				 token
			     with Not_found -> (ID word)
			   )
# 369 "bin/lexer.ml"

  | 5 ->
# 45 "bin/lexer.mll"
                     ( UNIT )
# 374 "bin/lexer.ml"

  | 6 ->
# 46 "bin/lexer.mll"
                     ( COMMA )
# 379 "bin/lexer.ml"

  | 7 ->
# 47 "bin/lexer.mll"
                     ( SEMI )
# 384 "bin/lexer.ml"

  | 8 ->
# 48 "bin/lexer.mll"
                     ( COLON )
# 389 "bin/lexer.ml"

  | 9 ->
# 49 "bin/lexer.mll"
                     ( QUESTION )
# 394 "bin/lexer.ml"

  | 10 ->
# 50 "bin/lexer.mll"
                     ( GETS )
# 399 "bin/lexer.ml"

  | 11 ->
# 51 "bin/lexer.mll"
                     ( LPAREN )
# 404 "bin/lexer.ml"

  | 12 ->
# 52 "bin/lexer.mll"
                     ( RPAREN )
# 409 "bin/lexer.ml"

  | 13 ->
# 53 "bin/lexer.mll"
                     ( LBRACE )
# 414 "bin/lexer.ml"

  | 14 ->
# 54 "bin/lexer.mll"
                     ( RBRACE )
# 419 "bin/lexer.ml"

  | 15 ->
# 55 "bin/lexer.mll"
                     ( PLUS )
# 424 "bin/lexer.ml"

  | 16 ->
# 56 "bin/lexer.mll"
                     ( MINUS )
# 429 "bin/lexer.ml"

  | 17 ->
# 57 "bin/lexer.mll"
                     ( MULTIPLY )
# 434 "bin/lexer.ml"

  | 18 ->
# 58 "bin/lexer.mll"
                     ( DIVIDE )
# 439 "bin/lexer.ml"

  | 19 ->
# 59 "bin/lexer.mll"
                     ( MOD )
# 444 "bin/lexer.ml"

  | 20 ->
# 60 "bin/lexer.mll"
                     ( CARET )
# 449 "bin/lexer.ml"

  | 21 ->
# 61 "bin/lexer.mll"
                     ( LT )
# 454 "bin/lexer.ml"

  | 22 ->
# 62 "bin/lexer.mll"
                     ( LE )
# 459 "bin/lexer.ml"

  | 23 ->
# 63 "bin/lexer.mll"
                     ( EQ )
# 464 "bin/lexer.ml"

  | 24 ->
# 64 "bin/lexer.mll"
                     ( NE )
# 469 "bin/lexer.ml"

  | 25 ->
# 65 "bin/lexer.mll"
                     ( NEALT )
# 474 "bin/lexer.ml"

  | 26 ->
# 66 "bin/lexer.mll"
                     ( GE )
# 479 "bin/lexer.ml"

  | 27 ->
# 67 "bin/lexer.mll"
                     ( GT )
# 484 "bin/lexer.ml"

  | 28 ->
# 68 "bin/lexer.mll"
                     ( EOF )
# 489 "bin/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;
