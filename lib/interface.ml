
let parse s =
  let lexbuf = Lexing.from_string s in
  try
    Parser.programa Lexer.token lexbuf
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      failwith @@
        Printf.sprintf "Erro de sintaxe na linha %d, coluna %d"
          pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
