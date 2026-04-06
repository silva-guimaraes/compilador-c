{
  open Parser
}

let digito = ['0'-'9']
let sinal = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

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
    | constante_float { CONSTANTE_FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | constante_int { CONSTANTE_INT (int_of_string (Lexing.lexeme lexbuf)) }
    | identificador { PALAVRA (Lexing.lexeme lexbuf)}
    | espaco { token lexbuf }
    (* obrigatório *)
    | eof { EOF }
    | _ {raise (Failure ("Caractere não identificado: '" ^ Lexing.lexeme lexbuf ^ "'"))}
