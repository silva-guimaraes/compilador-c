{
  open Parser

  let unescape = function
    | 'n'  -> '\n'  | 't'  -> '\t'  | 'r'  -> '\r'
    | '\\' -> '\\'  | '\'' -> '\''  | '"'  -> '"'
    | '0'  -> '\000'| c    -> c
}

let digito   = ['0'-'9']
let hexdig   = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha    = ['a'-'z' 'A'-'Z' '_']
let alnum    = alpha | digito

let macro    = "#" [^ '\n']*
let int_dec  = digito+
let int_hex  = "0x" hexdig+
let int_oct  = '0' ['0'-'7']+

let exp      = ['e' 'E'] ['+' '-']? digito+
let float1   = digito+ '.' digito* exp?     (* 1.0  1.  1.0e3 *)
let float2   = digito* '.' digito+ exp?     (* .5   .5e3      *)
let float3   = digito+ exp                  (* 1e3            *)
let float_lit = (float1 | float2 | float3) ['f' 'F' 'l' 'L']?

let int_lit  = (int_hex | int_oct | int_dec) ['u' 'U' 'l' 'L']*

let id       = alpha alnum*
let espaco   = [' ' '\t' '\r']+
let newline  = '\n'

(* ==================== *)
rule token = parse
    (* Espaço e novas linhas *)
    | espaco   { token lexbuf }
    | newline  { token lexbuf }
    (* Macros e comentários de linha – descartados *)
    | macro    { token lexbuf }
    (* Comentário de bloco *)
    | "/*"     { block_comment lexbuf }
    (* Comentário de linha *)
    | "//"     { line_comment lexbuf }

    (* ---- Literais ---- *)
    | float_lit  { CONSTANTE_FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | int_lit    { CONSTANTE_INT   (int_of_string   (Lexing.lexeme lexbuf)) }

    (* Char literal *)
    | '\'' '\\' (_ as c) '\''  { CONSTANTE_CHAR (unescape c) }
    | '\'' ([^ '\'' '\\'] as c) '\'' { CONSTANTE_CHAR c }

    (* String literal *)
    | '"'  { let buf = Buffer.create 32 in str_lit buf lexbuf }

    (* ---- Operadores multi-char ---- *)
    | "=="  { EQ }   | "!="  { NEQ }
    | "<="  { LE }   | ">="  { GE }
    | "&&"  { AND }  | "||"  { OR }
    | "++"  { INC }  | "--"  { DEC }
    | "<<"  { SHL }  | ">>"  { SHR }
    | "+="  { PLUS_ASSIGN }  | "-="  { MINUS_ASSIGN }
    | "*="  { MULT_ASSIGN  }  | "/="  { DIV_ASSIGN   }
    | "%="  { MOD_ASSIGN   }  | "&="  { AND_ASSIGN   }
    | "|="  { OR_ASSIGN    }  | "^="  { XOR_ASSIGN   }
    | "<<=" { SHL_ASSIGN   }  | ">>=" { SHR_ASSIGN   }
    | "->"  { ARROW }

    (* ---- Operadores simples ---- *)
    | '='   { ASSIGN }
    | '+'   { PLUS }   | '-'   { MINUS }
    | '*'   { ASTERISCO } | '/'   { BARRA }  | '%'   { PERCENT }
    | '<'   { LT }     | '>'   { GT }
    | '!'   { BANG }
    | '&'   { AMPERSAND } | '|'   { PIPE }   | '^'   { HAT }
    | '~'   { TILDE }
    | '.'   { PONTO }
    | '?'   { INTERROGACAO } | ':'   { COLON }

    (* ---- Delimitadores ---- *)
    | '{'   { IDENT_INICIO } | '}'   { IDENT_FIM }
    | '('   { PAREN_INICIO } | ')'   { PAREN_FIM }
    | '['   { COL_INICIO   } | ']'   { COL_FIM   }
    | ';'+  { PONTO_VIRGULA }
    | ','   { VIRGULA }

    (* ---- Palavras reservadas ---- *)
    | "auto"     { AUTO }
    | "break"    { BREAK }
    | "case"     { CASE }
    | "char"     { CHAR }
    | "const"    { CONST }
    | "continue" { CONTINUE }
    | "default"  { DEFAULT }
    | "do"       { DO }
    | "double"   { DOUBLE }
    | "else"     { ELSE }
    | "enum"     { ENUM }
    | "extern"   { EXTERN }
    | "float"    { FLOAT }
    | "for"      { FOR }
    | "goto"     { GOTO }
    | "if"       { IF }
    | "int"      { INT }
    | "long"     { LONG }
    | "register" { REGISTER }
    | "return"   { RETURN }
    | "short"    { SHORT }
    | "signed"   { SIGNED }
    | "sizeof"   { SIZEOF }
    | "static"   { STATIC }
    | "struct"   { STRUCT }
    | "switch"   { SWITCH }
    | "typedef"  { TYPEDEF }
    | "union"    { UNION }
    | "unsigned" { UNSIGNED }
    | "void"     { VOID }
    | "volatile" { VOLATILE }
    | "while"    { WHILE }
    | "NULL"     { NULL_KW }

    | id         { PALAVRA (Lexing.lexeme lexbuf) }

    | eof        { EOF }
    | _          { raise (Failure ("Caractere não identificado: '"
                           ^ Lexing.lexeme lexbuf ^ "'")) }

and block_comment = parse
    | "*/"  { token lexbuf }
    | '\n'  { block_comment lexbuf }
    | _     { block_comment lexbuf }
    | eof   { raise (Failure "Comentário de bloco não fechado") }

and line_comment = parse
    | '\n'  { token lexbuf }
    | _     { line_comment lexbuf }
    | eof   { EOF }

and str_lit buf = parse
    | '"'               { CONSTANTE_STR (Buffer.contents buf) }
    | '\\' (_ as c)     { Buffer.add_char buf (unescape c); str_lit buf lexbuf }
    | [^ '"' '\\' '\n']+ as s { Buffer.add_string buf s; str_lit buf lexbuf }
    | '\n'              { raise (Failure "String literal com quebra de linha") }
    | eof               { raise (Failure "String literal não fechada") }
