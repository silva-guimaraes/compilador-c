
type bop =
  | Soma
  | Sub
  | Mult
  | Div
  | Mod
  | Inc
  | Dec
[@@deriving show]

type id = Id of string
[@@deriving show]

type sinal = Signed | Unsigned
[@@deriving show]

type const =
  | Int of int
[@@deriving show]

type expr =
  | Var of id
  | Bop of bop
  | Const of const
[@@deriving show]

type var_decl = { tipo: id; nome: id; deref: int; array_len: int option;
  sinal: sinal option}
[@@deriving show]

type struct2 = { nome: id; atributos: var_decl list }
[@@deriving show]

type union = { nome: id; atributos: var_decl list }
[@@deriving show]

type enum = { nome: id; lista: id list}
[@@deriving show]

type func_prototipo = { tipo: id; nome: id; parametros: var_decl list }
[@@deriving show]

type stmt =
  | VarDecl
  | VarDeclInit of var_decl * expr
[@@deriving show]

type funcao = stmt list
[@@deriving show]

type func = { prototipo: func_prototipo; corpo: stmt list }
[@@deriving show]

type decl =
  | Struct of struct2
  | Union of union
  | Enum of enum
  | FuncProt of func_prototipo
  | Func of func
[@@deriving show]

type programa = Programa of decl list
[@@deriving show]

