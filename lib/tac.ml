open Ast

(* ── Tipos ──────────────────────────────────────────────────────── *)

(* place: nome de variável, temporário ("t0") ou literal ("5", "3.14") *)
type place = string
type label = string

type instr =
  | FuncBegin of string
  | FuncEnd
  | Label     of label
  | BinOp     of place * string * place * place   (* d := a op b *)
  | UnOp      of place * string * place           (* d := op a   *)
  | Copy      of place * place                    (* d := a      *)
  | Addr      of place * string                   (* d := &x     *)
  | Deref     of place * place                    (* d := *a     *)
  | DerefSet  of place * place                    (* *a := b     *)
  | ArrGet    of place * place * place            (* d := a[i]   *)
  | ArrSet    of place * place * place            (* a[i] := b   *)
  | Param     of place                            (* param a     *)
  | Call      of place option * string * int      (* [d :=] call f, n *)
  | Return    of place option                     (* return [a]  *)
  | Goto      of label
  | IfGoto    of place * label                    (* if a goto L  *)
  | IfFGoto   of place * label                    (* ifFalse a goto L *)
  | Global    of string * string                  (* global tipo nome *)
  | GlobalSet of string * place                   (* global nome := a *)

(* ── Estado ─────────────────────────────────────────────────────── *)

type state = {
  mutable temp_n  : int;
  mutable label_n : int;
  mutable code    : instr list;   (* revertida em generate *)
}

type ctx = {
  st           : state;
  break_lbl    : label option;
  continue_lbl : label option;
}

let create_state () = { temp_n = 0; label_n = 0; code = [] }

let new_temp  st = let n = st.temp_n  in st.temp_n  <- n + 1; Printf.sprintf "t%d" n
let new_label st = let n = st.label_n in st.label_n <- n + 1; Printf.sprintf "L%d" n

let emit ctx i = ctx.st.code <- i :: ctx.st.code

let string_of_bop = function
  | Soma   -> "+"  | Sub    -> "-"  | Mult -> "*"  | Div  -> "/"
  | Mod    -> "%"  | Eq     -> "==" | Neq  -> "!="
  | Lt     -> "<"  | Gt     -> ">"  | Le   -> "<=" | Ge   -> ">="
  | And    -> "&&" | Or     -> "||"
  | BitAnd -> "&"  | BitOr  -> "|"  | BitXor -> "^"
  | Shl    -> "<<" | Shr    -> ">>"
  | Assign -> ":="

let tipo_str (t : tipo) (d : int) =
  let (Id b) = t.base in
  b ^ String.make d '*'

(* ── Geração de expressões ──────────────────────────────────────── *)

let rec gen_expr (ctx : ctx) (e : expr) : place =
  match e with
  | Const (Int n)   -> string_of_int n
  | Const (Float f) -> Printf.sprintf "%g" f
  | Const (Char c)  -> Printf.sprintf "'%c'" c
  | Const (Str s)   -> Printf.sprintf "%S" s
  | Const Null      -> "NULL"
  | Var (Id s)      -> s

  | Cast (_, _, e2) -> gen_expr ctx e2

  | Sizeof t ->
      let r = new_temp ctx.st in
      emit ctx (Copy (r, Printf.sprintf "sizeof(%s)" (tipo_str t 0)));
      r

  | Bop (Assign, lhs, rhs) ->
      let rv = gen_expr ctx rhs in
      gen_assign ctx lhs rv

  | Bop (op, a, b) ->
      let av = gen_expr ctx a in
      let bv = gen_expr ctx b in
      let r  = new_temp ctx.st in
      emit ctx (BinOp (r, string_of_bop op, av, bv));
      r

  | Uop (Neg,    e2) -> gen_unop ctx "-"  e2
  | Uop (Not,    e2) -> gen_unop ctx "!"  e2
  | Uop (BitNot, e2) -> gen_unop ctx "~"  e2

  | Uop (Addr, e2) ->
      let r = new_temp ctx.st in
      let v = (match e2 with Var (Id s) -> s | _ -> gen_expr ctx e2) in
      emit ctx (Addr (r, v));
      r

  | Uop (Deref, e2) ->
      let v = gen_expr ctx e2 in
      let r = new_temp ctx.st in
      emit ctx (Deref (r, v));
      r

  | Uop (PreInc,  e2) -> gen_inc_dec ctx e2 "+" true
  | Uop (PreDec,  e2) -> gen_inc_dec ctx e2 "-" true
  | Uop (PostInc, e2) -> gen_inc_dec ctx e2 "+" false
  | Uop (PostDec, e2) -> gen_inc_dec ctx e2 "-" false

  | Call (Id f, args) ->
      let n = List.length args in
      List.iter (fun a -> emit ctx (Param (gen_expr ctx a))) args;
      let r = new_temp ctx.st in
      emit ctx (Call (Some r, f, n));
      r

  | Index (a, i) ->
      let av = gen_expr ctx a in
      let iv = gen_expr ctx i in
      let r  = new_temp ctx.st in
      emit ctx (ArrGet (r, av, iv));
      r

  | Member (e2, Id f) ->
      let v = gen_expr ctx e2 in
      let r = new_temp ctx.st in
      emit ctx (Copy (r, v ^ "." ^ f));
      r

  | Arrow (e2, Id f) ->
      let v = gen_expr ctx e2 in
      let r = new_temp ctx.st in
      emit ctx (Copy (r, v ^ "->" ^ f));
      r

  | Ternary (c, a, b) ->
      let cv      = gen_expr ctx c in
      let l_false = new_label ctx.st in
      let l_end   = new_label ctx.st in
      let r       = new_temp ctx.st in
      emit ctx (IfFGoto (cv, l_false));
      emit ctx (Copy (r, gen_expr ctx a));
      emit ctx (Goto l_end);
      emit ctx (Label l_false);
      emit ctx (Copy (r, gen_expr ctx b));
      emit ctx (Label l_end);
      r

  | CompoundLit _ ->
      let r = new_temp ctx.st in
      emit ctx (Copy (r, "{...}"));
      r

and gen_unop ctx op e2 =
  let v = gen_expr ctx e2 in
  let r = new_temp ctx.st in
  emit ctx (UnOp (r, op, v));
  r

and gen_assign (ctx : ctx) (lhs : expr) (rv : place) : place =
  match lhs with
  | Var (Id s) ->
      emit ctx (Copy (s, rv)); s
  | Uop (Deref, p) ->
      let pv = gen_expr ctx p in
      emit ctx (DerefSet (pv, rv)); rv
  | Index (a, i) ->
      let av = gen_expr ctx a in
      let iv = gen_expr ctx i in
      emit ctx (ArrSet (av, iv, rv)); rv
  | Member (e2, Id f) ->
      let v = gen_expr ctx e2 in
      emit ctx (Copy (v ^ "." ^ f, rv)); rv
  | Arrow (e2, Id f) ->
      let v = gen_expr ctx e2 in
      emit ctx (Copy (v ^ "->" ^ f, rv)); rv
  | other ->
      let lv = gen_expr ctx other in
      emit ctx (Copy (lv, rv)); lv

and gen_inc_dec ctx e2 op pre =
  match e2 with
  | Var (Id s) ->
      if pre then begin
        emit ctx (BinOp (s, op, s, "1")); s
      end else begin
        let r = new_temp ctx.st in
        emit ctx (Copy (r, s));
        emit ctx (BinOp (s, op, s, "1"));
        r
      end
  | Uop (Deref, p) ->
      let pv  = gen_expr ctx p in
      let cur = new_temp ctx.st in
      let nxt = new_temp ctx.st in
      emit ctx (Deref (cur, pv));
      emit ctx (BinOp (nxt, op, cur, "1"));
      emit ctx (DerefSet (pv, nxt));
      if pre then nxt else cur
  | _ ->
      let v = gen_expr ctx e2 in
      let r = new_temp ctx.st in
      let n = new_temp ctx.st in
      emit ctx (Copy (r, v));
      emit ctx (BinOp (n, op, v, "1"));
      ignore n; r

(* ── Geração de statements ──────────────────────────────────────── *)

let rec gen_stmt (ctx : ctx) (s : stmt) : unit =
  match s with
  | VarDecl _       -> ()
  | MultiVarDecl _  -> ()

  | VarDeclInit (v, e) ->
      let (Id nome) = v.nome in
      let ev = gen_expr ctx e in
      emit ctx (Copy (nome, ev))

  | Expr e -> ignore (gen_expr ctx e)

  | Return None    -> emit ctx (Return None)
  | Return (Some e) ->
      let ev = gen_expr ctx e in
      emit ctx (Return (Some ev))

  | Block ss -> List.iter (gen_stmt ctx) ss

  | If (c, t, None) ->
      let cv    = gen_expr ctx c in
      let l_end = new_label ctx.st in
      emit ctx (IfFGoto (cv, l_end));
      gen_stmt ctx t;
      emit ctx (Label l_end)

  | If (c, t, Some el) ->
      let cv      = gen_expr ctx c in
      let l_false = new_label ctx.st in
      let l_end   = new_label ctx.st in
      emit ctx (IfFGoto (cv, l_false));
      gen_stmt ctx t;
      emit ctx (Goto l_end);
      emit ctx (Label l_false);
      gen_stmt ctx el;
      emit ctx (Label l_end)

  | While (c, body) ->
      let l_top = new_label ctx.st in
      let l_end = new_label ctx.st in
      emit ctx (Label l_top);
      let cv = gen_expr ctx c in
      emit ctx (IfFGoto (cv, l_end));
      gen_stmt { ctx with break_lbl = Some l_end; continue_lbl = Some l_top } body;
      emit ctx (Goto l_top);
      emit ctx (Label l_end)

  | DoWhile (body, c) ->
      let l_top = new_label ctx.st in
      let l_end = new_label ctx.st in
      emit ctx (Label l_top);
      gen_stmt { ctx with break_lbl = Some l_end; continue_lbl = Some l_top } body;
      let cv = gen_expr ctx c in
      emit ctx (IfGoto (cv, l_top));
      emit ctx (Label l_end)

  | For (init, cond, step, body) ->
      let l_top  = new_label ctx.st in
      let l_step = new_label ctx.st in
      let l_end  = new_label ctx.st in
      (match init with
       | ForInitDecl (v, e_opt) ->
           Option.iter (fun e ->
             let (Id nome) = v.nome in
             emit ctx (Copy (nome, gen_expr ctx e))
           ) e_opt
       | ForInitExpr e_opt ->
           Option.iter (fun e -> ignore (gen_expr ctx e)) e_opt);
      emit ctx (Label l_top);
      Option.iter (fun c ->
        let cv = gen_expr ctx c in
        emit ctx (IfFGoto (cv, l_end))
      ) cond;
      gen_stmt { ctx with break_lbl = Some l_end; continue_lbl = Some l_step } body;
      emit ctx (Label l_step);
      Option.iter (fun e -> ignore (gen_expr ctx e)) step;
      emit ctx (Goto l_top);
      emit ctx (Label l_end)

  | Break ->
      (match ctx.break_lbl with Some l -> emit ctx (Goto l) | None -> ())

  | Continue ->
      (match ctx.continue_lbl with Some l -> emit ctx (Goto l) | None -> ())

  | Goto (Id s) -> emit ctx (Goto s)

  | Switch (e, body) -> gen_switch ctx e body

  | Case (_, ss)  -> List.iter (gen_stmt ctx) ss
  | Default ss    -> List.iter (gen_stmt ctx) ss

  | TypedefDecl _ -> ()

and gen_switch ctx e_sw body =
  let ev    = gen_expr ctx e_sw in
  let l_end = new_label ctx.st in
  let ctx2  = { ctx with break_lbl = Some l_end } in

  let stmts = match body with Block ss -> ss | s -> [s] in

  (* Associa cada Case/Default a um label novo *)
  let tagged = List.map (fun s ->
    match s with
    | Case _ | Default _ -> (s, Some (new_label ctx.st))
    | other              -> (other, None)
  ) stmts in

  (* Emite saltos de despacho *)
  let has_default = ref false in
  let default_lbl = ref "" in
  List.iter (fun (s, lbl_opt) ->
    match s, lbl_opt with
    | Case (cv_expr, _), Some lbl ->
        let cv = gen_expr ctx cv_expr in
        let t  = new_temp ctx.st in
        emit ctx (BinOp (t, "==", ev, cv));
        emit ctx (IfGoto (t, lbl))
    | Default _, Some lbl ->
        has_default := true;
        default_lbl := lbl
    | _ -> ()
  ) tagged;
  if !has_default
  then emit ctx (Goto !default_lbl)
  else emit ctx (Goto l_end);

  (* Emite os corpos *)
  List.iter (fun (s, lbl_opt) ->
    Option.iter (fun l -> emit ctx (Label l)) lbl_opt;
    match s with
    | Case (_, ss)  -> List.iter (gen_stmt ctx2) ss
    | Default ss    -> List.iter (gen_stmt ctx2) ss
    | other         -> gen_stmt ctx2 other
  ) tagged;

  emit ctx (Label l_end)

(* ── Geração de declarações ─────────────────────────────────────── *)

let gen_decl (ctx : ctx) (d : decl) : unit =
  match d with
  | Func f ->
      let (Id fname) = f.prototipo.nome in
      emit ctx (FuncBegin fname);
      (* reinicia contadores por função, mantendo a lista de código compartilhada *)
      let saved_temp  = ctx.st.temp_n in
      let saved_label = ctx.st.label_n in
      ctx.st.temp_n  <- 0;
      ctx.st.label_n <- 0;
      let ctx2 = { ctx with break_lbl = None; continue_lbl = None } in
      List.iter (gen_stmt ctx2) f.corpo;
      emit ctx (FuncEnd);
      ctx.st.temp_n  <- saved_temp;
      ctx.st.label_n <- saved_label

  | GlobalVar v ->
      let (Id nome) = v.nome in
      emit ctx (Global (tipo_str v.tipo v.deref, nome))

  | GlobalVarInit (v, e) ->
      let (Id nome) = v.nome in
      emit ctx (Global (tipo_str v.tipo v.deref, nome));
      let ev = gen_expr ctx e in
      emit ctx (GlobalSet (nome, ev))

  | GlobalMultiVar vs ->
      List.iter (fun (v : Ast.var_decl) ->
        let (Id nome) = v.nome in
        emit ctx (Global (tipo_str v.tipo v.deref, nome))
      ) vs

  | TopBlock ss ->
      emit ctx (FuncBegin "_toplevel");
      let saved_temp  = ctx.st.temp_n in
      let saved_label = ctx.st.label_n in
      ctx.st.temp_n  <- 0;
      ctx.st.label_n <- 0;
      let ctx2 = { ctx with break_lbl = None; continue_lbl = None } in
      List.iter (gen_stmt ctx2) ss;
      emit ctx (FuncEnd);
      ctx.st.temp_n  <- saved_temp;
      ctx.st.label_n <- saved_label

  | FuncProt _ | Struct _ | Union _ | Enum _ | Typedef _ -> ()

(* ── Ponto de entrada ───────────────────────────────────────────── *)

let generate (Programa decls) : instr list =
  let st  = create_state () in
  let ctx = { st; break_lbl = None; continue_lbl = None } in
  List.iter (gen_decl ctx) decls;
  List.rev ctx.st.code

(* ── Pretty-print ───────────────────────────────────────────────── *)

let string_of_instr = function
  | FuncBegin s          -> Printf.sprintf "function %s:" s
  | FuncEnd              -> "end"
  | Label l              -> Printf.sprintf "%s:" l
  | Copy (d, s)          -> Printf.sprintf "  %s := %s" d s
  | BinOp (d, op, a, b) -> Printf.sprintf "  %s := %s %s %s" d a op b
  | UnOp  (d, op, a)    -> Printf.sprintf "  %s := %s%s" d op a
  | Addr  (d, s)         -> Printf.sprintf "  %s := &%s" d s
  | Deref (d, s)         -> Printf.sprintf "  %s := *%s" d s
  | DerefSet (p, v)      -> Printf.sprintf "  *%s := %s" p v
  | ArrGet (d, a, i)     -> Printf.sprintf "  %s := %s[%s]" d a i
  | ArrSet (a, i, v)     -> Printf.sprintf "  %s[%s] := %s" a i v
  | Param p              -> Printf.sprintf "  param %s" p
  | Call (None, f, n)    -> Printf.sprintf "  call %s, %d" f n
  | Call (Some d, f, n)  -> Printf.sprintf "  %s := call %s, %d" d f n
  | Return None          -> "  return"
  | Return (Some v)      -> Printf.sprintf "  return %s" v
  | Goto l               -> Printf.sprintf "  goto %s" l
  | IfGoto  (v, l)       -> Printf.sprintf "  if %s goto %s" v l
  | IfFGoto (v, l)       -> Printf.sprintf "  ifFalse %s goto %s" v l
  | Global (t, n)        -> Printf.sprintf "global %s %s" t n
  | GlobalSet (n, v)     -> Printf.sprintf "global %s := %s" n v

let print_tac instrs =
  List.iter (fun i -> print_string (string_of_instr i); print_char '\n') instrs
