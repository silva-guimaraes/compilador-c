open Ast
module S = Symtable

type error_kind =
  | UndeclaredVar  of string
  | UndeclaredFunc of string
  | Redeclaration  of string
  | ArityMismatch  of { fname: string; expected: int; got: int }
  | TypeMismatch   of { context: string; expected: string; got: string }

type sem_error = { kind: error_kind; hint: string }

type result = Ok | Errors of sem_error list

type env = {
  symtab          : S.t;
  mutable errors  : sem_error list;
  mutable func_ret: S.ctype;
}

let add_error env kind hint =
  env.errors <- { kind; hint } :: env.errors

(* ── Helpers de tipo ─────────────────────────────────────────────── *)

let arith_result a b = match a, b with
  | S.Unknown, _ | _, S.Unknown -> S.Unknown
  | S.Float,   _ | _, S.Float   -> S.Float
  | S.Pointer _, _               -> S.Pointer S.Void
  | _, S.Pointer _               -> S.Pointer S.Void
  | _                            -> S.Int

let string_of_ctype : S.ctype -> string = function
  | S.Void        -> "void"
  | S.Int         -> "int"
  | S.Float       -> "float"
  | S.Pointer _   -> "pointer"
  | S.Array _     -> "array"
  | S.Struct s    -> "struct " ^ s
  | S.Union  s    -> "union "  ^ s
  | S.Enum   s    -> "enum "   ^ s
  | S.Unknown     -> "?"

let compatible (lhs : S.ctype) (rhs : S.ctype) = match lhs, rhs with
  | _, S.Unknown | S.Unknown, _  -> true
  | S.Int, S.Float               -> false
  | S.Void, _ | _, S.Void       -> false
  | _                            -> true

(* ── Inferência de tipo de expressão ─────────────────────────────── *)

let rec infer_expr env e : S.ctype = match e with
  | Var (Id s) ->
      (match S.lookup env.symtab s with
       | Some { kind = S.Var ct; _ }     -> ct
       | Some { kind = S.EnumVal; _ }    -> S.Int
       | Some { kind = S.TypeDef ct; _ } -> ct
       | Some { kind = S.Func _; _ }     -> S.Pointer S.Void
       | None ->
           add_error env (UndeclaredVar s) ("uso de '" ^ s ^ "'");
           S.Unknown)

  | Const (Int _)   -> S.Int
  | Const (Float _) -> S.Float
  | Const (Char _)  -> S.Int
  | Const (Str _)   -> S.Pointer S.Int
  | Const Null      -> S.Pointer S.Void

  | Cast (t, d, e2) ->
      check_expr env e2;
      S.ctype_of_tipo env.symtab t d

  | Sizeof _ -> S.Int

  | Uop (Deref, e2) ->
      (match infer_expr env e2 with
       | S.Pointer inner -> inner
       | S.Array   inner -> inner
       | S.Unknown       -> S.Unknown
       | other ->
           add_error env
             (TypeMismatch { context = "deref (*)";
                             expected = "pointer"; got = string_of_ctype other })
             "operador *";
           S.Unknown)

  | Uop (Addr, e2)  -> S.Pointer (infer_expr env e2)
  | Uop (_, e2)     -> infer_expr env e2

  | Bop (Assign, lhs, rhs) ->
      let lt = infer_expr env lhs in
      let rt = infer_expr env rhs in
      if not (compatible lt rt) then
        add_error env
          (TypeMismatch { context = "atribuição";
                          expected = string_of_ctype lt; got = string_of_ctype rt })
          (Printf.sprintf "atribuição de '%s' para '%s'"
             (string_of_ctype rt) (string_of_ctype lt));
      lt

  | Bop ((Soma|Sub|Mult|Div|Mod), a, b) ->
      arith_result (infer_expr env a) (infer_expr env b)

  | Bop ((Eq|Neq|Lt|Gt|Le|Ge|And|Or), a, b) ->
      check_expr env a; check_expr env b; S.Int

  | Bop ((BitAnd|BitOr|BitXor|Shl|Shr), a, b) ->
      check_expr env a; check_expr env b; S.Int

  | Call (Id fname, args) ->
      let nargs = List.length args in
      List.iter (check_expr env) args;
      (match S.lookup env.symtab fname with
       | Some { kind = S.Func { ret; arity }; _ } ->
           if arity >= 0 && nargs <> arity then
             add_error env
               (ArityMismatch { fname; expected = arity; got = nargs })
               (Printf.sprintf "chamada de '%s'" fname);
           ret
       | Some _ ->
           add_error env (UndeclaredFunc fname)
             ("'" ^ fname ^ "' não é uma função");
           S.Unknown
       | None ->
           add_error env (UndeclaredFunc fname)
             ("chamada de '" ^ fname ^ "'");
           S.Unknown)

  | Index (e2, idx) ->
      check_expr env idx;
      (match infer_expr env e2 with
       | S.Array inner | S.Pointer inner -> inner
       | S.Unknown                       -> S.Unknown
       | other ->
           add_error env
             (TypeMismatch { context = "indexação []";
                             expected = "array/pointer"; got = string_of_ctype other })
             "operador []";
           S.Unknown)

  | Member (e2, _) -> check_expr env e2; S.Unknown
  | Arrow  (e2, _) -> check_expr env e2; S.Unknown

  | Ternary (c, a, b) ->
      check_expr env c;
      arith_result (infer_expr env a) (infer_expr env b)

  | CompoundLit es ->
      List.iter (check_expr env) es;
      S.Unknown

and check_expr env e = ignore (infer_expr env e)

(* ── Statements ──────────────────────────────────────────────────── *)

and check_stmt env s = match s with
  | VarDecl v ->
      declare_var env v

  | VarDeclInit (v, e) ->
      let et = infer_expr env e in
      let vt = S.ctype_of_tipo env.symtab v.tipo v.deref in
      if not (compatible vt et) then begin
        let (Id nome) = v.nome in
        add_error env
          (TypeMismatch { context = "inicialização de '" ^ nome ^ "'";
                          expected = string_of_ctype vt; got = string_of_ctype et })
          ("declaração de '" ^ nome ^ "'")
      end;
      declare_var env v

  | MultiVarDecl vs ->
      List.iter (declare_var env) vs

  | Expr e ->
      check_expr env e

  | Return None ->
      if env.func_ret <> S.Void then
        add_error env
          (TypeMismatch { context = "return vazio";
                          expected = string_of_ctype env.func_ret; got = "void" })
          "return"

  | Return (Some e) ->
      let et = infer_expr env e in
      if not (compatible env.func_ret et) then
        add_error env
          (TypeMismatch { context = "return";
                          expected = string_of_ctype env.func_ret; got = string_of_ctype et })
          "return"

  | Block stmts ->
      S.push_scope env.symtab;
      List.iter (check_stmt env) stmts;
      S.pop_scope env.symtab

  | If (c, t, el) ->
      check_expr env c;
      check_stmt env t;
      Option.iter (check_stmt env) el

  | While (c, s2) ->
      check_expr env c;
      check_stmt env s2

  | DoWhile (s2, c) ->
      check_stmt env s2;
      check_expr env c

  | For (init, cond, step, s2) ->
      S.push_scope env.symtab;
      (match init with
       | ForInitDecl (v, e_opt) ->
           Option.iter (check_expr env) e_opt;
           declare_var env v
       | ForInitExpr e_opt ->
           Option.iter (check_expr env) e_opt);
      Option.iter (check_expr env) cond;
      Option.iter (check_expr env) step;
      check_stmt env s2;
      S.pop_scope env.symtab

  | Switch (e, s2) ->
      check_expr env e;
      check_stmt env s2

  | Case (e, ss) ->
      check_expr env e;
      List.iter (check_stmt env) ss

  | Default ss ->
      List.iter (check_stmt env) ss

  | TypedefDecl (t, d, Id n) ->
      let ct = S.ctype_of_tipo env.symtab t d in
      (match S.declare env.symtab n (S.TypeDef ct) with
       | Error _ -> add_error env (Redeclaration n) ("typedef '" ^ n ^ "'")
       | Ok ()   -> ())

  | Goto _ | Break | Continue -> ()

and declare_var env (v : var_decl) =
  let (Id nome) = v.nome in
  let ct = S.ctype_of_tipo env.symtab v.tipo v.deref in
  let ct = match v.array_dims with [] -> ct | _ -> S.Array ct in
  match S.declare env.symtab nome (S.Var ct) with
  | Error _ -> add_error env (Redeclaration nome) ("declaração de '" ^ nome ^ "'")
  | Ok ()   -> ()

(* ── Declarações de topo ─────────────────────────────────────────── *)

and check_decl env d = match d with
  | Func f ->
      let ret = S.ctype_of_tipo env.symtab f.prototipo.tipo f.prototipo.deref in
      S.push_scope env.symtab;
      List.iter (declare_var env) f.prototipo.parametros;
      let saved = env.func_ret in
      env.func_ret <- ret;
      List.iter (check_stmt env) f.corpo;
      env.func_ret <- saved;
      S.pop_scope env.symtab

  | FuncProt _ -> ()

  | GlobalVar v ->
      declare_var env v

  | GlobalVarInit (v, e) ->
      let et = infer_expr env e in
      let vt = S.ctype_of_tipo env.symtab v.tipo v.deref in
      if not (compatible vt et) then begin
        let (Id nome) = v.nome in
        add_error env
          (TypeMismatch { context = "inicialização global de '" ^ nome ^ "'";
                          expected = string_of_ctype vt; got = string_of_ctype et })
          ("declaração global de '" ^ nome ^ "'")
      end;
      declare_var env v

  | GlobalMultiVar vs ->
      List.iter (declare_var env) vs

  | Struct s ->
      let (Id nome) = s.nome in
      ignore (S.declare env.symtab ("struct " ^ nome) (S.TypeDef (S.Struct nome)))

  | Union u ->
      let (Id nome) = u.nome in
      ignore (S.declare env.symtab ("union " ^ nome) (S.TypeDef (S.Union nome)))

  | Enum e ->
      let (Id nome) = e.nome in
      ignore (S.declare env.symtab ("enum " ^ nome) (S.TypeDef (S.Enum nome)));
      List.iter (fun (Id v, _) ->
        ignore (S.declare env.symtab v S.EnumVal)
      ) e.lista

  | Typedef (t, d, Id n) ->
      let ct = S.ctype_of_tipo env.symtab t d in
      ignore (S.declare env.symtab n (S.TypeDef ct))

  | TopBlock stmts ->
      S.push_scope env.symtab;
      List.iter (check_stmt env) stmts;
      S.pop_scope env.symtab

(* Pré-passo: registra todas as funções antes do caminhamento principal *)
let pre_pass env decls =
  List.iter (function
    | Func f ->
        let (Id fname) = f.prototipo.nome in
        let ret = S.ctype_of_tipo env.symtab f.prototipo.tipo f.prototipo.deref in
        ignore (S.declare env.symtab fname
          (S.Func { ret; arity = List.length f.prototipo.parametros }))
    | FuncProt p ->
        let (Id fname) = p.nome in
        let ret = S.ctype_of_tipo env.symtab p.tipo p.deref in
        ignore (S.declare env.symtab fname
          (S.Func { ret; arity = List.length p.parametros }))
    | _ -> ()
  ) decls

(* ── Ponto de entrada ────────────────────────────────────────────── *)

let check (Programa decls) =
  let symtab = S.create () in
  let env = { symtab; errors = []; func_ret = S.Void } in
  pre_pass env decls;
  List.iter (check_decl env) decls;
  if env.errors = [] then Ok
  else Errors (List.rev env.errors)
