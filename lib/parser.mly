
%{
  open Ast
%}

%token <int> CONSTANTE_INT
%token <float> CONSTANTE_FLOAT
%token <string> PALAVRA
%token EOF


%type <Ast.programa> programa
%start programa

%%

programa:
    | EOF { Programa [] }
;
