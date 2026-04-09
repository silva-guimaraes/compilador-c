
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
%token PAREN_INICIO
%token PAREN_FIM
%token COL_INICIO
%token COL_FIM
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
%token ASTERISCO

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
  | a = func_prototipo; { FuncProt a }

let var_decl :=
  | a = tipo; b = ASTERISCO*; c = id;
    { 
      let count =  List.length b in
      { tipo = a; nome = c; deref = count; array_len = None}
    }
  | a = tipo; b = ASTERISCO*; c = id; COL_INICIO; d = CONSTANTE_INT; COL_FIM;
    { 
      let count =  List.length b in
      { tipo = a; nome = c; deref = count; array_len = Some d}
    }

let stmt :=
  | a = tipo; b = id; ASSIGN; c = expr; PONTO_VIRGULA; { VarDecl (a, b, c) }

let var_decl_list :=
  | a = var_decl; PONTO_VIRGULA; b = var_decl_list; { a :: b }
  | a = var_decl; PONTO_VIRGULA?; { [a] }

let struct2 :=
  | STRUCT; a = id; IDENT_INICIO; b = var_decl_list; IDENT_FIM; PONTO_VIRGULA;
    { ({nome = a; atributos = b}: struct2)}

let union :=
  | UNION; a = id; IDENT_INICIO; b = var_decl_list; IDENT_FIM; PONTO_VIRGULA;
      { ({nome = a; atributos = b}: union )}

let enum_lista :=
  | a = id; VIRGULA; b = enum_lista; { a :: b }
  | a = id; { [a] }

let enum :=
  | ENUM; a = id; IDENT_INICIO; b = enum_lista; IDENT_FIM; PONTO_VIRGULA;
    { ({nome = a; lista = b} : enum)}

let param_lista :=
  | a = var_decl; VIRGULA; b = param_lista; { a :: b }
  | a = var_decl; { [a] }

let func_prototipo :=
  | a = tipo; b = id; PAREN_INICIO; c = param_lista; PAREN_FIM; PONTO_VIRGULA;
    { { tipo = a; nome = b; parametros = c } }

let tipo :=
  | INT; { Id "int" }
  | FLOAT; { Id "float" }
  | CHAR; { Id "char" }
  | VOID; { Id "void" }
  | a = id; { a }

let expr :=
  | a = id; { Var a }
