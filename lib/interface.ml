
let parse s =
  Lexer.clear_macros ();
  let lexbuf = Lexing.from_string s in
  try
    Parser.programa Lexer.token lexbuf
  with
    | Parser.Error ->
    let pos = lexbuf.lex_curr_p in
    failwith @@
    Printf.sprintf "Erro de sintaxe na linha %d, coluna %d"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
;;

let string_of_token = function
  | Parser.CONSTANTE_INT i    -> Printf.sprintf "CONSTANTE_INT(%d)" i
  | Parser.CONSTANTE_FLOAT f  -> Printf.sprintf "CONSTANTE_FLOAT(%g)" f
  | Parser.CONSTANTE_CHAR c   -> Printf.sprintf "CONSTANTE_CHAR('%c')" c
  | Parser.CONSTANTE_STR s    -> Printf.sprintf "CONSTANTE_STR(%S)" s
  | Parser.PALAVRA s          -> Printf.sprintf "PALAVRA(%s)" s
  | Parser.ASSIGN             -> "ASSIGN"
  | Parser.PLUS_ASSIGN        -> "PLUS_ASSIGN"
  | Parser.MINUS_ASSIGN       -> "MINUS_ASSIGN"
  | Parser.MULT_ASSIGN        -> "MULT_ASSIGN"
  | Parser.DIV_ASSIGN         -> "DIV_ASSIGN"
  | Parser.MOD_ASSIGN         -> "MOD_ASSIGN"
  | Parser.AND_ASSIGN         -> "AND_ASSIGN"
  | Parser.OR_ASSIGN          -> "OR_ASSIGN"
  | Parser.XOR_ASSIGN         -> "XOR_ASSIGN"
  | Parser.SHL_ASSIGN         -> "SHL_ASSIGN"
  | Parser.SHR_ASSIGN         -> "SHR_ASSIGN"
  | Parser.EQ                 -> "EQ"
  | Parser.NEQ                -> "NEQ"
  | Parser.LT                 -> "LT"
  | Parser.GT                 -> "GT"
  | Parser.LE                 -> "LE"
  | Parser.GE                 -> "GE"
  | Parser.AND                -> "AND"
  | Parser.OR                 -> "OR"
  | Parser.PLUS               -> "PLUS"
  | Parser.MINUS              -> "MINUS"
  | Parser.ASTERISCO          -> "ASTERISCO"
  | Parser.BARRA              -> "BARRA"
  | Parser.PERCENT            -> "PERCENT"
  | Parser.AMPERSAND          -> "AMPERSAND"
  | Parser.PIPE               -> "PIPE"
  | Parser.HAT                -> "HAT"
  | Parser.TILDE              -> "TILDE"
  | Parser.SHL                -> "SHL"
  | Parser.SHR                -> "SHR"
  | Parser.BANG               -> "BANG"
  | Parser.INC                -> "INC"
  | Parser.DEC                -> "DEC"
  | Parser.ARROW              -> "ARROW"
  | Parser.PONTO              -> "PONTO"
  | Parser.INTERROGACAO       -> "INTERROGACAO"
  | Parser.COLON              -> "COLON"
  | Parser.PONTO_VIRGULA      -> "PONTO_VIRGULA"
  | Parser.VIRGULA            -> "VIRGULA"
  | Parser.IDENT_INICIO       -> "IDENT_INICIO"
  | Parser.IDENT_FIM          -> "IDENT_FIM"
  | Parser.PAREN_INICIO       -> "PAREN_INICIO"
  | Parser.PAREN_FIM          -> "PAREN_FIM"
  | Parser.COL_INICIO         -> "COL_INICIO"
  | Parser.COL_FIM            -> "COL_FIM"
  | Parser.AUTO               -> "AUTO"
  | Parser.BREAK              -> "BREAK"
  | Parser.CASE               -> "CASE"
  | Parser.CHAR               -> "CHAR"
  | Parser.CONST              -> "CONST"
  | Parser.CONTINUE           -> "CONTINUE"
  | Parser.DEFAULT            -> "DEFAULT"
  | Parser.DO                 -> "DO"
  | Parser.DOUBLE             -> "DOUBLE"
  | Parser.ELSE               -> "ELSE"
  | Parser.ENUM               -> "ENUM"
  | Parser.EXTERN             -> "EXTERN"
  | Parser.FLOAT              -> "FLOAT"
  | Parser.FOR                -> "FOR"
  | Parser.GOTO               -> "GOTO"
  | Parser.IF                 -> "IF"
  | Parser.INT                -> "INT"
  | Parser.LONG               -> "LONG"
  | Parser.REGISTER           -> "REGISTER"
  | Parser.RETURN             -> "RETURN"
  | Parser.SHORT              -> "SHORT"
  | Parser.SIGNED             -> "SIGNED"
  | Parser.SIZEOF             -> "SIZEOF"
  | Parser.STATIC             -> "STATIC"
  | Parser.STRUCT             -> "STRUCT"
  | Parser.SWITCH             -> "SWITCH"
  | Parser.TYPEDEF            -> "TYPEDEF"
  | Parser.UNION              -> "UNION"
  | Parser.UNSIGNED           -> "UNSIGNED"
  | Parser.VOID               -> "VOID"
  | Parser.VOLATILE           -> "VOLATILE"
  | Parser.WHILE              -> "WHILE"
  | Parser.NULL_KW            -> "NULL_KW"
  | Parser.EOF                -> "EOF"
;;

let list_tokens s =
  Lexer.clear_macros ();
  let lexbuf = Lexing.from_string s in
  try while true do
    let tok = Lexer.token lexbuf in
    if tok = Parser.EOF then raise Exit;
    print_string (string_of_token tok);
    print_char '\n'
  done with Exit | End_of_file -> ()
