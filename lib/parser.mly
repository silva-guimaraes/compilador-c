%{
  open Ast
%}

(* ============================== Tokens ============================== *)

%token <int>    CONSTANTE_INT
%token <float>  CONSTANTE_FLOAT
%token <char>   CONSTANTE_CHAR
%token <string> CONSTANTE_STR
%token <string> PALAVRA

(* Operadores de atribuição *)
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN MULT_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AND_ASSIGN OR_ASSIGN XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN

(* Comparação / lógico *)
%token EQ NEQ LT GT LE GE AND OR

(* Aritméticos / bitwise *)
%token PLUS MINUS ASTERISCO BARRA PERCENT
%token AMPERSAND PIPE HAT TILDE SHL SHR

%token BANG
%token INC DEC
%token ARROW PONTO

(* Ternário *)
%token INTERROGACAO COLON

(* Delimitadores *)
%token PONTO_VIRGULA VIRGULA
%token IDENT_INICIO IDENT_FIM
%token PAREN_INICIO PAREN_FIM
%token COL_INICIO   COL_FIM

(* Palavras reservadas *)
%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM
%token EXTERN FLOAT FOR GOTO IF INT LONG REGISTER RETURN SHORT SIGNED
%token SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE
%token NULL_KW

%token EOF

(* ============================== Precedência ============================== *)
(* Usada apenas para resolver o conflito dangling-else *)

%nonassoc NO_ELSE
%nonassoc ELSE

(* ============================== Tipos de retorno ============================== *)

%type <Ast.programa> programa
%start programa

%%

(* ====================== Topo ====================== *)

let programa :=
  | ds = decl*; EOF; { Programa ds }

(* ====================== Identificador ====================== *)

let id :=
  | a = PALAVRA; { Id a }

(* ====================== Qualificadores e sinal ====================== *)

let qual_item :=
  | CONST;    { Const }
  | VOLATILE; { Volatile }
  | REGISTER; { Register }
  | STATIC;   { Static }
  | EXTERN;   { Extern }
  | AUTO;     { Auto }

let sinal_opt :=
  | UNSIGNED; { Some Unsigned }
  | SIGNED;   { Some Signed }
  |           { None }

let tamanho_opt :=
  | LONG;  { Some Long }
  | SHORT; { Some Short }
  |        { None }

(* ====================== Tipo ====================== *)

let tipo :=
  | qs = qual_item*; s = sinal_opt; sz = tamanho_opt; b = tipo_base_nome;
    { { base = b; sinal = s; tamanho = sz; quals = qs } }
  | qs = qual_item*; s = sinal_opt; sz = tamanho_opt;
    { { base = Id "int"; sinal = s; tamanho = sz; quals = qs } }

let tipo_base_nome :=
  | INT;    { Id "int"    }
  | FLOAT;  { Id "float"  }
  | CHAR;   { Id "char"   }
  | VOID;   { Id "void"   }
  | DOUBLE; { Id "double" }
  | LONG;   { Id "long"   }
  (* struct Foo, union Bar, enum Baz usados como tipo em declarações *)
  | STRUCT; a = id; { let (Id s) = a in Id ("struct " ^ s) }
  | UNION;  a = id; { let (Id s) = a in Id ("union "  ^ s) }
  | ENUM;   a = id; { let (Id s) = a in Id ("enum "   ^ s) }
  | a = id; { a }

(* Número de asteriscos (nível de ponteiro) *)
let deref_count :=
  | xs = ASTERISCO*; { List.length xs }

(* ====================== var_decl ====================== *)

let array_dim :=
  | COL_INICIO; e = expr?; COL_FIM; { e }

let var_decl :=
  | t = tipo; d = deref_count; n = id; dims = array_dim*;
    { { tipo = t; nome = n; deref = d; array_dims = dims } }

(* ====================== Listas auxiliares ====================== *)

let var_decl_list :=
  | a = var_decl; PONTO_VIRGULA; rest = var_decl_list; { a :: rest }
  | a = var_decl; PONTO_VIRGULA?;                       { [a] }

let param_lista :=
  | a = var_decl; VIRGULA; rest = param_lista; { a :: rest }
  | a = var_decl;                               { [a] }
  |                                             { [] }

let expr_lista :=
  | a = assign_expr; VIRGULA; rest = expr_lista; { a :: rest }
  | a = assign_expr;                              { [a] }
  |                                               { [] }

(* ====================== Struct / Union / Enum ====================== *)

let struct2 :=
  | STRUCT; a = id; IDENT_INICIO; b = var_decl_list; IDENT_FIM; PONTO_VIRGULA;
    { ({ nome = a; atributos = b }: struct2) }

let union :=
  | UNION; a = id; IDENT_INICIO; b = var_decl_list; IDENT_FIM; PONTO_VIRGULA;
    { ({ nome = a; atributos = b }: union) }

let enum_item :=
  | a = id; ASSIGN; v = expr; { (a, Some v) }
  | a = id;                   { (a, None)    }

let enum_lista :=
  | a = enum_item; VIRGULA; rest = enum_lista; { a :: rest }
  | a = enum_item;                              { [a] }

let enum :=
  | ENUM; a = id; IDENT_INICIO; b = enum_lista; IDENT_FIM; PONTO_VIRGULA;
    { { nome = a; lista = b } }

(* ====================== Protótipo e função ====================== *)

let func_prototipo :=
  | t = tipo; d = deref_count; n = id;
    PAREN_INICIO; ps = param_lista; PAREN_FIM;
    { { tipo = t; deref = d; nome = n; parametros = ps } }

let func :=
  | p = func_prototipo; IDENT_INICIO; body = stmt*; IDENT_FIM;
    { { prototipo = p; corpo = body } }

(* ====================== Declarações de topo de arquivo ====================== *)

(* ====================== Inicializador ====================== *)
(* Separa { expr, ... } de expr comum para evitar conflito com Block *)

let initializer_ :=
  | IDENT_INICIO; es = nonempty_init_list; IDENT_FIM; { CompoundLit es }
  | e = expr;                                          { e }

let decl :=
  | a = struct2;                                             { Struct a }
  | a = union;                                               { Union a }
  | a = enum;                                                { Enum a }
  | a = func;                                                { Func a }
  | p = func_prototipo; PONTO_VIRGULA;                       { FuncProt p }
  | v = var_decl; PONTO_VIRGULA;                             { GlobalVar v }
  | v = var_decl; ASSIGN; e = initializer_; PONTO_VIRGULA;   { GlobalVarInit (v, e) }
  | TYPEDEF; t = typedef_tipo; d = deref_count; n = id; PONTO_VIRGULA;
    { Typedef (t, d, n) }


(* ====================== Statements ====================== *)

let stmt :=
  | s = simple_stmt; PONTO_VIRGULA; { s }
  | s = compound_stmt;              { s }


let typedef_tipo :=
  | qs = qual_item*; s = sinal_opt; sz = tamanho_opt; b = typedef_base;
    { { base = b; sinal = s; tamanho = sz; quals = qs } }
  | qs = qual_item*; s = sinal_opt; sz = tamanho_opt;
    { { base = Id "int"; sinal = s; tamanho = sz; quals = qs } }

let typedef_base :=
  | INT;    { Id "int"    }
  | FLOAT;  { Id "float"  }
  | CHAR;   { Id "char"   }
  | VOID;   { Id "void"   }
  | DOUBLE; { Id "double" }
  | LONG;   { Id "long"   }
  | STRUCT; a = id; { let (Id s) = a in Id ("struct " ^ s) }
  | UNION;  a = id; { let (Id s) = a in Id ("union "  ^ s) }
  | ENUM;   a = id; { let (Id s) = a in Id ("enum "   ^ s) }
  (* sem | a = id — aqui não, para não engolir o nome do typedef *)

(* Statements que terminam com ';' *)
let simple_stmt :=
  | v = var_decl; ASSIGN; e = initializer_;      { VarDeclInit (v, e) }
  | v = var_decl;                                { VarDecl v }
  | RETURN; e = expr;                            { Return (Some e) }
  | RETURN;                                      { Return None }
  | BREAK;                                       { Break }
  | CONTINUE;                                    { Continue }
  | GOTO; n = id;                                { Goto n }
  | TYPEDEF; t = typedef_tipo; d = deref_count; n = id; { TypedefDecl (t, d, n) }
  | t_nome = PALAVRA; d = deref_count; n = id; ASSIGN; e = initializer_;
    { let t = { base = Id t_nome; sinal = None; tamanho = None; quals = [] }
      in VarDeclInit ({ tipo = t; nome = n; deref = d; array_dims = [] }, e) }
  | t_nome = PALAVRA; d = deref_count; n = id;
    { let t = { base = Id t_nome; sinal = None; tamanho = None; quals = [] }
    in VarDecl { tipo = t; nome = n; deref = d; array_dims = [] } }
  | e = expr;                                    { Expr e }

(* Statements compostos / de controle — sem ';' terminal *)
let compound_stmt :=
  | IDENT_INICIO; body = stmt*; IDENT_FIM;
    { Block body }

  | IF; PAREN_INICIO; c = expr; PAREN_FIM; t = stmt; %prec NO_ELSE
    { If (c, t, None) }

  | IF; PAREN_INICIO; c = expr; PAREN_FIM; t = stmt; ELSE; el = stmt;
    { If (c, t, Some el) }

  | WHILE; PAREN_INICIO; c = expr; PAREN_FIM; s = stmt;
    { While (c, s) }

  | DO; s = stmt; WHILE; PAREN_INICIO; c = expr; PAREN_FIM; PONTO_VIRGULA;
    { DoWhile (s, c) }

  | FOR; PAREN_INICIO; init = for_init; cond = expr?; PONTO_VIRGULA;
    step = expr?; PAREN_FIM; s = stmt;
    { For (init, cond, step, s) }

  | SWITCH; PAREN_INICIO; e = expr; PAREN_FIM;
    IDENT_INICIO; cases = switch_body*; IDENT_FIM;
    { Switch (e, Block cases) }

let for_init :=
  | v = var_decl; ASSIGN; e = expr; PONTO_VIRGULA; { ForInitDecl (v, Some e) }
  | v = var_decl;                   PONTO_VIRGULA;  { ForInitDecl (v, None) }
  | e = expr?;                      PONTO_VIRGULA;  { ForInitExpr e }

(* case / default dentro de switch *)
let switch_body :=
  | CASE; e = expr; COLON; body = switch_stmt*;  { Case (e, body) }
  | DEFAULT;        COLON; body = switch_stmt*;  { Default body }

(* statements dentro de case — estruturalmente idêntico a stmt *)
let switch_stmt :=
  | s = simple_stmt; PONTO_VIRGULA; { s }
  | s = compound_stmt;              { s }

(* ====================== Expressões ====================== *)
(*
   Hierarquia explícita — sem %left/%right para operadores binários.
   Isso elimina todos os warnings "precedence level never useful".

   expr → assign_expr → ternary_expr → lor → land → bor → bxor → band
        → eq → rel → shift → add → mul → cast → unary → postfix → primary
*)

let expr :=
  | e = assign_expr; { e }

let assign_expr :=
  | c = ternary_expr; ASSIGN;        r = assign_expr; { Bop (Assign, c, r) }
  | c = ternary_expr; PLUS_ASSIGN;   r = assign_expr; { Bop (Soma,   c, r) }
  | c = ternary_expr; MINUS_ASSIGN;  r = assign_expr; { Bop (Sub,    c, r) }
  | c = ternary_expr; MULT_ASSIGN;   r = assign_expr; { Bop (Mult,   c, r) }
  | c = ternary_expr; DIV_ASSIGN;    r = assign_expr; { Bop (Div,    c, r) }
  | c = ternary_expr; MOD_ASSIGN;    r = assign_expr; { Bop (Mod,    c, r) }
  | c = ternary_expr; AND_ASSIGN;    r = assign_expr; { Bop (BitAnd, c, r) }
  | c = ternary_expr; OR_ASSIGN;     r = assign_expr; { Bop (BitOr,  c, r) }
  | c = ternary_expr; XOR_ASSIGN;    r = assign_expr; { Bop (BitXor, c, r) }
  | c = ternary_expr; SHL_ASSIGN;    r = assign_expr; { Bop (Shl,    c, r) }
  | c = ternary_expr; SHR_ASSIGN;    r = assign_expr; { Bop (Shr,    c, r) }
  | e = ternary_expr;                                 { e }

let ternary_expr :=
  | c = lor_expr; INTERROGACAO; t = expr; COLON; f = ternary_expr;
    { Ternary (c, t, f) }
  | e = lor_expr; { e }

let lor_expr :=
  | a = lor_expr;  OR;  b = land_expr; { Bop (Or,  a, b) }
  | e = land_expr;                     { e }

let land_expr :=
  | a = land_expr; AND; b = bor_expr; { Bop (And, a, b) }
  | e = bor_expr;                     { e }

let bor_expr :=
  | a = bor_expr; PIPE; b = bxor_expr; { Bop (BitOr, a, b) }
  | e = bxor_expr;                      { e }

let bxor_expr :=
  | a = bxor_expr; HAT; b = band_expr; { Bop (BitXor, a, b) }
  | e = band_expr;                      { e }

let band_expr :=
  | a = band_expr; AMPERSAND; b = eq_expr; { Bop (BitAnd, a, b) }
  | e = eq_expr;                            { e }

let eq_expr :=
  | a = eq_expr; EQ;  b = rel_expr; { Bop (Eq,  a, b) }
  | a = eq_expr; NEQ; b = rel_expr; { Bop (Neq, a, b) }
  | e = rel_expr;                   { e }

let rel_expr :=
  | a = rel_expr; LT; b = shift_expr; { Bop (Lt, a, b) }
  | a = rel_expr; GT; b = shift_expr; { Bop (Gt, a, b) }
  | a = rel_expr; LE; b = shift_expr; { Bop (Le, a, b) }
  | a = rel_expr; GE; b = shift_expr; { Bop (Ge, a, b) }
  | e = shift_expr;                   { e }

let shift_expr :=
  | a = shift_expr; SHL; b = add_expr; { Bop (Shl, a, b) }
  | a = shift_expr; SHR; b = add_expr; { Bop (Shr, a, b) }
  | e = add_expr;                      { e }

let add_expr :=
  | a = add_expr; PLUS;  b = mul_expr; { Bop (Soma, a, b) }
  | a = add_expr; MINUS; b = mul_expr; { Bop (Sub,  a, b) }
  | e = mul_expr;                      { e }

let mul_expr :=
  | a = mul_expr; ASTERISCO; b = cast_expr; { Bop (Mult, a, b) }
  | a = mul_expr; BARRA;     b = cast_expr; { Bop (Div,  a, b) }
  | a = mul_expr; PERCENT;   b = cast_expr; { Bop (Mod,  a, b) }
  | e = cast_expr;                           { e }

let cast_expr :=
  | PAREN_INICIO; t = tipo; d = deref_count; PAREN_FIM; e = cast_expr;
    { Cast (t, d, e) }
  | e = unary_expr; { e }

let unary_expr :=
  | MINUS;     e = cast_expr; { Uop (Neg,    e) }
  | PLUS;      e = cast_expr; { Uop (Neg,    e) }
  | BANG;      e = cast_expr; { Uop (Not,    e) }
  | TILDE;     e = cast_expr; { Uop (BitNot, e) }
  | INC;       e = cast_expr; { Uop (PreInc, e) }
  | DEC;       e = cast_expr; { Uop (PreDec, e) }
  | ASTERISCO; e = cast_expr; { Uop (Deref,  e) }
  | AMPERSAND; e = cast_expr; { Uop (Addr,   e) }
  | SIZEOF; PAREN_INICIO; t = tipo; PAREN_FIM; { Sizeof t }
  | e = postfix_expr; { e }

let postfix_expr :=
  | e = postfix_expr; INC;                            { Uop (PostInc, e) }
  | e = postfix_expr; DEC;                            { Uop (PostDec, e) }
  | e = postfix_expr; COL_INICIO; i = expr; COL_FIM;  { Index (e, i) }
  | e = postfix_expr; PONTO;  m = id;                 { Member (e, m) }
  | e = postfix_expr; ARROW;  m = id;                 { Arrow (e, m) }
  | f = id; PAREN_INICIO; args = expr_lista; PAREN_FIM; { Call (f, args) }
  | e = primary_expr;                                 { e }

let primary_expr :=
  | a = id;               { Var a }
  | a = CONSTANTE_INT;    { Const (Int a) }
  | a = CONSTANTE_FLOAT;  { Const (Float a) }
  | a = CONSTANTE_CHAR;   { Const (Char a) }
  | a = CONSTANTE_STR;    { Const (Str a) }
  | NULL_KW;              { Const Null }
  | PAREN_INICIO; e = expr; PAREN_FIM; { e }

(* Lista de pelo menos um elemento — sem alternativa vazia *)
let nonempty_init_list :=
  | a = initializer_; VIRGULA; rest = nonempty_init_list; { a :: rest }
  | a = initializer_; VIRGULA?;                            { [a] }
