
type bop =
  | Soma
  | Sub
  | Mult
  | Div
  | Mod
  | Inc
  | Dec


type id = Id of string

type expr =
  | Var of id
  | Bop of bop

type atributo = { tipo: id; nome: id }

type struct2 = { nome: id; atributos: atributo list }

type union = { nome: id; atributos: atributo list }

type enum = { nome: id; lista: id list}

type decl =
  | Struct of struct2
  | Union of union
  | Enum of enum

type stmt =
  | VarDecl of id * id * expr

type funcao = stmt list

type programa = Programa of decl list
