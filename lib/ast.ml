
type bop =
  | Soma
  | Sub
  | Mult
  | Div
  | Mod
  | Inc
  | Dec

type stmt =
  | Block

type expr =
    | Word of string

type programa = Programa of expr list
