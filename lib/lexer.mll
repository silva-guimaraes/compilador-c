{
  open Parser
}

let digito = ['0'-'9']
let sinal = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

let macro = "#" [^ '\n']*

let constante_int = sinal? digito+

let exponente = ['e' 'E']

(* Exemplos: 1.0 1. 1.111 1.11e10 1.11e-10 1.11e+10*)
let constante_float = sinal? digito+ '.' digito+ (exponente sinal? digito+)?
let identificador = alpha (alpha | digito | '-')*

let espaco = [' ' '\t']+


(* =============== *)

(* Curiosamente com o ocamllex, tokens são definidos pelo parser. Veja o arquivo parser.mly.
   Em maiúsculo são os tokens que podem receber qualquer valor dado a tipagem correta. *)

rule token = parse
    (* *)
    | '=' { ASSIGN }
    | '{' { IDENT_INICIO }
    | '}' { IDENT_FIM }
    | ';'* { PONTO_VIRGULA } (* trata vários como um só *)
    | ',' { VIRGULA }
    | '(' { PAREN_INICIO }
    | ')' { PAREN_FIM }
    | '*' { ASTERISCO }
    | '[' { COL_INICIO }
    | ']' { COL_FIM }
    (* reservados *)
    | "auto" { AUTO }
    | "break" { BREAK }
    | "case" { CASE }
    | "char" { CHAR }
    | "const" { CONST }
    | "continue" { CONTINUE }
    | "default" { DEFAULT }
    | "do" { DO }
    | "double" { DOUBLE }
    | "else" { ELSE }
    | "enum" { ENUM }
    | "extern" { EXTERN }
    | "float" { FLOAT }
    | "for" { FOR }
    | "goto" { GOTO }
    | "if" { IF }
    | "int" { INT }
    | "long" { LONG }
    | "register" { REGISTER }
    | "return" { RETURN }
    | "short" { SHORT }
    | "signed" { SIGNED }
    | "sizeof" { SIZEOF }
    | "static" { STATIC }
    | "struct" { STRUCT }
    | "switch" { SWITCH }
    | "typedef" { TYPEDEF }
    | "union" { UNION }
    | "unsigned" { UNSIGNED }
    | "void" { VOID }
    | "volatile" { VOLATILE }
    | "while" { WHILE }
    (* =============== *)
    | identificador { PALAVRA (Lexing.lexeme lexbuf)}
    | constante_float { CONSTANTE_FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | constante_int { CONSTANTE_INT (int_of_string (Lexing.lexeme lexbuf)) }
    | macro | espaco { token lexbuf }
    (* obrigatório *)
    | eof { EOF }
    | _ {raise (Failure ("Caractere não identificado: '" ^ Lexing.lexeme lexbuf ^ "'"))}
