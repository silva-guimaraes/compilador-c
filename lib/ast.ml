
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

type expr =
  | Var of id
  | Bop of bop
[@@deriving show]

type var_decl = { tipo: id; nome: id; deref: int; array_len: int option}
[@@deriving show]

type struct2 = { nome: id; atributos: var_decl list }
[@@deriving show]

type union = { nome: id; atributos: var_decl list }
[@@deriving show]

type enum = { nome: id; lista: id list}
[@@deriving show]

type func_prototipo = { tipo: id; nome: id; parametros: var_decl list }
[@@deriving show]

type decl =
  | Struct of struct2
  | Union of union
  | Enum of enum
  | FuncProt of func_prototipo
[@@deriving show]

type stmt =
  | VarDecl of id * id * expr
[@@deriving show]

type funcao = stmt list
[@@deriving show]

type programa = Programa of decl list
[@@deriving show]
