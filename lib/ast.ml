(* Operadores binários *)
type bop =
  | Soma | Sub | Mult | Div | Mod
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  | BitAnd | BitOr | BitXor | Shl | Shr
  | Assign
[@@deriving show]

(* Operadores unários *)
type uop =
  | Neg | Not | BitNot
  | PreInc | PreDec | PostInc | PostDec
  | Deref | Addr
[@@deriving show]

type id = Id of string
[@@deriving show]

type sinal = Signed | Unsigned
[@@deriving show]

(* Qualificadores de tipo *)
type qual = Const | Volatile | Register | Static | Extern | Auto
[@@deriving show]

(* Especificadores de tamanho *)
type tamanho = Long | Short
[@@deriving show]

type const_val =
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Null
[@@deriving show]

type tipo =
  { base: id
  ; sinal: sinal option
  ; tamanho: tamanho option
  ; quals: qual list
  }
[@@deriving show]

type expr =
  | Var of id
  | Const of const_val
  | Bop of bop * expr * expr
  | Uop of uop * expr
  | Call of id * expr list          (* f(args) *)
  | Index of expr * expr            (* a[i] *)
  | Member of expr * id             (* e.campo *)
  | Arrow of expr * id              (* e->campo *)
  | Cast of tipo * int (* deref *) * expr
  | Sizeof of tipo
  | Ternary of expr * expr * expr   (* cond ? a : b *)
  | CompoundLit of expr list        (* {1, 2, 3} – inicializadores *)
[@@deriving show]

type var_decl =
  { tipo: tipo
  ; nome: id
  ; deref: int
  ; array_dims: expr option list    (* [] por dim; None = sem tamanho *)
  }
[@@deriving show]

type struct2 = { nome: id; atributos: var_decl list }
[@@deriving show]

type union = { nome: id; atributos: var_decl list }
[@@deriving show]

type enum = { nome: id; lista: (id * expr option) list }
[@@deriving show]

type func_prototipo = { tipo: tipo; deref: int; nome: id; parametros: var_decl list }
[@@deriving show]

type stmt =
  | VarDecl of var_decl
  | VarDeclInit of var_decl * expr
  | Expr of expr
  | Return of expr option
  | Break
  | Continue
  | Goto of id
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | DoWhile of stmt * expr
  | For of for_init * expr option * expr option * stmt
  | Switch of expr * stmt
  | Case of expr * stmt list
  | Default of stmt list
  | Block of stmt list
  | TypedefDecl of tipo * int (* deref *) * id

and for_init =
  | ForInitDecl of var_decl * expr option
  | ForInitExpr of expr option
[@@deriving show]

type func = { prototipo: func_prototipo; corpo: stmt list }
[@@deriving show]

type decl =
  | Struct of struct2
  | Union of union
  | Enum of enum
  | FuncProt of func_prototipo
  | Func of func
  | GlobalVar of var_decl
  | GlobalVarInit of var_decl * expr
  | Typedef of tipo * int * id
[@@deriving show]

type programa = Programa of decl list
[@@deriving show]
