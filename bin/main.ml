let usage = {|
Uso: ccc [OPÇÕES] [ARQUIVO]

Se ARQUIVO não for fornecido, lê da entrada padrão.

Opções:
  -dot ARQUIVO   Exporta a AST como grafo DOT para ARQUIVO
  -h, --help     Exibe esta mensagem
|}

let dot_output = ref None
let input_file = ref None

let specs = [
  "-dot", Arg.String (fun s -> dot_output := Some s), " Exporta AST como grafo .dot";
]

let () =
  Arg.parse specs (fun s -> input_file := Some s) usage

let read_input () =
  match !input_file with
  | Some path ->
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
  | None ->
      let buf = Buffer.create 1024 in
      (try while true do
        Buffer.add_channel buf stdin 4096
      done with End_of_file -> ());
      Buffer.contents buf

(* ── DOT export ───────────────────────────────────────────────── *)

let node_id =
  let c = ref 0 in
  fun () -> incr c; !c

let buf = Buffer.create 4096

let node label =
  let id = node_id () in
  Buffer.add_string buf
    (Printf.sprintf "  n%d [label=%S];\n" id label);
  id

let edge parent child =
  Buffer.add_string buf
    (Printf.sprintf "  n%d -> n%d;\n" parent child)

open Ccc.Ast

let rec dot_expr parent e =
  let id = match e with
    | Var (Id s)        -> node ("Var\\n" ^ s)
    | Const (Int i)     -> node (string_of_int i)
    | Const (Float f)   -> node (string_of_float f)
    | Const (Char c)    -> node (Printf.sprintf "'%c'" c)
    | Const (Str s)     -> node (Printf.sprintf "%S" s)
    | Const Null        -> node "NULL"
    | Bop (op, a, b)    ->
        let id = node (Ccc.Ast.show_bop op) in
        dot_expr id a; dot_expr id b; id
    | Uop (op, e)       ->
        let id = node (Ccc.Ast.show_uop op) in
        dot_expr id e; id
    | Call (Id f, args) ->
        let id = node ("Call\\n" ^ f) in
        List.iter (dot_expr id) args; id
    | Index (a, i)      ->
        let id = node "Index" in
        dot_expr id a; dot_expr id i; id
    | Member (e, Id m)  ->
        let id = node ("." ^ m) in dot_expr id e; id
    | Arrow (e, Id m)   ->
        let id = node ("->" ^ m) in dot_expr id e; id
    | Cast (t, d, e)    ->
        let id = node ("Cast\\n" ^ dot_tipo t d) in
        dot_expr id e; id
    | Sizeof t          -> node ("sizeof\\n" ^ dot_tipo t 0)
    | Ternary (c, a, b) ->
        let id = node "?:" in
        dot_expr id c; dot_expr id a; dot_expr id b; id
    | CompoundLit es    ->
        let id = node "{...}" in
        List.iter (dot_expr id) es; id
  in
  edge parent id

and dot_tipo t d =
  let (Id b) = t.base in
  let s = (match t.sinal with Some Signed -> "signed " | Some Unsigned -> "unsigned " | None -> "")
        ^ (match t.tamanho with Some Long -> "long " | Some Short -> "short " | None -> "")
        ^ b
        ^ String.make d '*' in
  s

and dot_var_decl parent (v : var_decl) =
  let (Id nome) = v.nome in
  let dims = String.concat "" (List.map (fun _ -> "[]") v.array_dims) in
  let id = node (dot_tipo v.tipo v.deref ^ " " ^ nome ^ dims) in
  edge parent id; id

and dot_stmt parent s =
  let id = match s with
    | VarDecl v ->
        let id = node "VarDecl" in
        ignore (dot_var_decl id v); id
    | VarDeclInit (v, e) ->
        let id = node "VarDeclInit" in
        ignore (dot_var_decl id v); dot_expr id e; id
    | Expr e ->
        let id = node "Expr" in dot_expr id e; id
    | Return None     -> node "return"
    | Return (Some e) ->
        let id = node "return" in dot_expr id e; id
    | Break    -> node "break"
    | Continue -> node "continue"
    | Goto (Id s) -> node ("goto " ^ s)
    | If (c, t, el) ->
        let id = node "if" in
        dot_expr id c;
        dot_stmt id t;
        (match el with Some e -> dot_stmt id e | None -> ());
        id
    | While (c, s) ->
        let id = node "while" in dot_expr id c; dot_stmt id s; id
    | DoWhile (s, c) ->
        let id = node "do-while" in dot_stmt id s; dot_expr id c; id
    | For (init, cond, step, s) ->
        let id = node "for" in
        (match init with
         | ForInitDecl (v, e) ->
             let vid = dot_var_decl id v in
             (match e with Some e -> dot_expr vid e | None -> ())
         | ForInitExpr e -> (match e with Some e -> dot_expr id e | None -> ()));
        (match cond with Some e -> dot_expr id e | None -> ());
        (match step with Some e -> dot_expr id e | None -> ());
        dot_stmt id s; id
    | Switch (e, s) ->
        let id = node "switch" in dot_expr id e; dot_stmt id s; id
    | Case (e, ss) ->
        let id = node "case" in dot_expr id e; List.iter (dot_stmt id) ss; id
    | Default ss ->
        let id = node "default" in List.iter (dot_stmt id) ss; id
    | Block ss ->
        let id = node "block" in List.iter (dot_stmt id) ss; id
    | TypedefDecl (t, d, Id n) ->
        node ("typedef\\n" ^ dot_tipo t d ^ " " ^ n)
  in
  edge parent id

let dot_decl parent d =
  let id = match d with
    | Struct s ->
        let (Id nome) = s.nome in
        let id = node ("struct " ^ nome) in
        List.iter (fun v -> ignore (dot_var_decl id v)) s.atributos; id
    | Union u ->
        let (Id nome) = u.nome in
        let id = node ("union " ^ nome) in
        List.iter (fun v -> ignore (dot_var_decl id v)) u.atributos; id
    | Enum e ->
        let (Id nome) = e.nome in
        let id = node ("enum " ^ nome) in
        List.iter (fun (Id n, v) ->
          let eid = node n in
          edge id eid;
          match v with Some e -> dot_expr eid e | None -> ()
        ) e.lista; id
    | FuncProt p ->
        let (Id nome) = p.nome in
        let id = node ("proto\\n" ^ dot_tipo p.tipo p.deref ^ " " ^ nome) in
        List.iter (fun v -> ignore (dot_var_decl id v)) p.parametros; id
    | Func f ->
        let (Id nome) = f.prototipo.nome in
        let id = node ("func\\n" ^ dot_tipo f.prototipo.tipo f.prototipo.deref ^ " " ^ nome) in
        List.iter (fun v -> ignore (dot_var_decl id v)) f.prototipo.parametros;
        List.iter (dot_stmt id) f.corpo; id
    | GlobalVar v ->
        let id = node "GlobalVar" in ignore (dot_var_decl id v); id
    | GlobalVarInit (v, e) ->
        let id = node "GlobalVarInit" in
        ignore (dot_var_decl id v); dot_expr id e; id
    | Typedef (t, d, Id n) ->
        node ("typedef\\n" ^ dot_tipo t d ^ " " ^ n)
  in
  edge parent id

let export_dot (Programa decls) path =
  Buffer.clear buf;
  Buffer.add_string buf "digraph AST {\n  node [shape=box fontname=\"monospace\"];\n";
  let root = node "programa" in
  List.iter (dot_decl root) decls;
  Buffer.add_string buf "}\n";
  let oc = open_out path in
  Buffer.output_buffer oc buf;
  close_out oc;
  Printf.printf "AST exportada para %s\n%!" path

(* ── Entry point ──────────────────────────────────────────────── *)

let () =
  let src = read_input () in
  let ast = Ccc.Interface.parse src in
  (match !dot_output with
   | Some path -> export_dot ast path
   | None -> print_string (Ccc.Ast.show_programa ast); print_newline ())
