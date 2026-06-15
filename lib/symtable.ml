open Ast

type ctype =
  | Void
  | Int
  | Float
  | Pointer of ctype
  | Array   of ctype
  | Struct  of string
  | Union   of string
  | Enum    of string
  | Unknown

type symbol_kind =
  | Var     of ctype
  | Func    of { ret: ctype; arity: int }
  | TypeDef of ctype
  | EnumVal

type symbol = { name: string; kind: symbol_kind }

type scope = { bindings: (string, symbol) Hashtbl.t; parent: scope option }

type t = { mutable current: scope }

let make_scope parent =
  { bindings = Hashtbl.create 16; parent }

(* Converte Ast.tipo + deref em ctype. Recebe a tabela para expandir typedefs. *)
let rec ctype_of_tipo tbl (tip : tipo) (deref : int) : ctype =
  let (Id base) = tip.base in
  let base_ct =
    if String.length base > 7 && String.sub base 0 7 = "struct " then
      Struct (String.sub base 7 (String.length base - 7))
    else if String.length base > 6 && String.sub base 0 6 = "union " then
      Union (String.sub base 6 (String.length base - 6))
    else if String.length base > 5 && String.sub base 0 5 = "enum " then
      Enum (String.sub base 5 (String.length base - 5))
    else match base with
      | "int" | "char" | "short" | "long" -> Int
      | "float" | "double"                -> Float
      | "void"                            -> Void
      | other ->
          (* tenta expandir typedef *)
          (match lookup tbl other with
           | Some { kind = TypeDef ct; _ } -> ct
           | _ -> Unknown)
  in
  let wrap ct =
    if deref > 0 then
      let rec wrap_n n c = if n = 0 then c else wrap_n (n-1) (Pointer c)
      in wrap_n deref ct
    else ct
  in
  wrap base_ct

and lookup (tbl : t) name : symbol option =
  let rec search scope =
    match Hashtbl.find_opt scope.bindings name with
    | Some s -> Some s
    | None   ->
        match scope.parent with
        | Some p -> search p
        | None   -> None
  in
  search tbl.current

let lookup_current (tbl : t) name : symbol option =
  Hashtbl.find_opt tbl.current.bindings name

let push_scope (tbl : t) =
  tbl.current <- make_scope (Some tbl.current)

let pop_scope (tbl : t) =
  match tbl.current.parent with
  | Some p -> tbl.current <- p
  | None   -> invalid_arg "Symtable.pop_scope: já no escopo global"

let declare (tbl : t) name kind : (unit, string) result =
  match lookup_current tbl name with
  | Some _ -> Error name
  | None   ->
      Hashtbl.add tbl.current.bindings name { name; kind };
      Ok ()

let builtins : (string * symbol_kind) list = [
  ("printf",  Func { ret = Int;          arity = -1 });
  ("scanf",   Func { ret = Int;          arity = -1 });
  ("fprintf", Func { ret = Int;          arity = -1 });
  ("sprintf", Func { ret = Int;          arity = -1 });
  ("sscanf",  Func { ret = Int;          arity = -1 });
  ("malloc",  Func { ret = Pointer Void; arity =  1 });
  ("calloc",  Func { ret = Pointer Void; arity =  2 });
  ("realloc", Func { ret = Pointer Void; arity =  2 });
  ("free",    Func { ret = Void;         arity =  1 });
  ("exit",    Func { ret = Void;         arity =  1 });
  ("strlen",  Func { ret = Int;          arity =  1 });
  ("strcpy",  Func { ret = Pointer Int;  arity =  2 });
  ("strncpy", Func { ret = Pointer Int;  arity =  3 });
  ("strcmp",  Func { ret = Int;          arity =  2 });
  ("strncmp", Func { ret = Int;          arity =  3 });
  ("strcat",  Func { ret = Pointer Int;  arity =  2 });
  ("memcpy",  Func { ret = Pointer Void; arity =  3 });
  ("memset",  Func { ret = Pointer Void; arity =  3 });
  ("fopen",   Func { ret = Pointer Void; arity =  2 });
  ("fclose",  Func { ret = Int;          arity =  1 });
  ("fgets",   Func { ret = Pointer Int;  arity =  3 });
  ("fputs",   Func { ret = Int;          arity =  2 });
  ("atoi",    Func { ret = Int;          arity =  1 });
  ("atof",    Func { ret = Float;        arity =  1 });
  ("abs",     Func { ret = Int;          arity =  1 });
  ("fabs",    Func { ret = Float;        arity =  1 });
  ("sqrt",    Func { ret = Float;        arity =  1 });
]

let create () : t =
  let tbl = { current = make_scope None } in
  List.iter (fun (name, kind) ->
    ignore (declare tbl name kind)
  ) builtins;
  tbl
