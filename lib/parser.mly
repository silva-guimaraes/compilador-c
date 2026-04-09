
%{
  open Ast
%}

%token <int> CONSTANTE_INT
%token <float> CONSTANTE_FLOAT
%token <string> PALAVRA

%token ASSIGN
%token PONTO_VIRGULA
%token IDENT_INICIO
%token IDENT_FIM
%token VIRGULA

%token AUTO
%token BREAK
%token CASE
%token CHAR
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INT
%token LONG
%token REGISTER
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STATIC
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE

%token EOF

%type <Ast.programa> programa
%start programa

%%

let programa :=
  | a = decl*; EOF; { Programa a }

let id :=
  | a = PALAVRA; { Id a }

let decl :=
  | a = struct2; { Struct a }
  | a = union; { Union a }
  | a = enum; { Enum a }

let stmt :=
  | a = tipo; b = id; ASSIGN; c = expr; PONTO_VIRGULA; { VarDecl (a, b, c) }

let atributo :=
  | a = tipo; b = id; PONTO_VIRGULA; { { tipo = a; nome = b} }

let struct2 :=
  | STRUCT; a = id; IDENT_INICIO; b = atributo*; IDENT_FIM; PONTO_VIRGULA;
    { ({nome = a; atributos = b}: struct2)}

let union :=
  | UNION; a = id; IDENT_INICIO; b = atributo*; IDENT_FIM; PONTO_VIRGULA;
      { ({nome = a; atributos = b}: union )}

let enum_list :=
  | a = id; VIRGULA; b = enum_list; { a :: b }
  | a = id; { [a] }

let enum :=
  | ENUM; a = id; IDENT_INICIO; b = enum_list; IDENT_FIM; PONTO_VIRGULA;
    { ({nome = a; lista = b} : enum)}

let tipo :=
  | INT; { Id "int" }
  | FLOAT; { Id "float" }
  | a = id; { a }

let expr :=
  | a = id; { Var a }
