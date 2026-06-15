open Tac

(* ── Valores ─────────────────────────────────────────────────────── *)

type value =
  | VInt   of int
  | VFloat of float
  | VChar  of char
  | VStr   of string
  | VNull
  | VPtr   of value ref
  | VArr   of value array
  | VUndef

let string_of_value = function
  | VInt n   -> string_of_int n
  | VFloat f -> Printf.sprintf "%g" f
  | VChar c  -> String.make 1 c
  | VStr s   -> s
  | VNull    -> "(null)"
  | VPtr _   -> "<ptr>"
  | VArr _   -> "<array>"
  | VUndef   -> "<undef>"

(* ── Ambiente ────────────────────────────────────────────────────── *)

type env = (string, value ref) Hashtbl.t

let env_create () : env = Hashtbl.create 16

let env_get (e : env) k =
  match Hashtbl.find_opt e k with
  | Some r -> !r
  | None   -> VUndef

let env_set (e : env) k v =
  match Hashtbl.find_opt e k with
  | Some r -> r := v
  | None   -> Hashtbl.replace e k (ref v)

(* ── Máquina ─────────────────────────────────────────────────────── *)

type frame = {
  locals  : env;
  ret_pc  : int;
  ret_dst : string option;
}

type machine = {
  instrs  : instr array;
  labels  : (string, int) Hashtbl.t;
  funcs   : (string, int * string list) Hashtbl.t;
  globals : env;
  mutable pc      : int;
  mutable frames  : frame list;
  mutable params  : value list;
  mutable running : bool;
}

(* ── Avaliação de place ──────────────────────────────────────────── *)

(* Desescapa uma string como OCaml a produziu com %S.
   Suporta: backslash-n, backslash-t, backslash-backslash, backslash-NNN. *)
let unescape_str s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' then begin
    let buf = Buffer.create (n - 2) in
    let i = ref 1 in
    while !i < n - 1 do
      if s.[!i] = '\\' && !i + 1 < n - 1 then begin
        let c1 = s.[!i + 1] in
        if c1 >= '0' && c1 <= '9' then begin
          (* \NNN — até 3 dígitos decimais (estilo OCaml %S) *)
          let code = ref (Char.code c1 - Char.code '0') in
          let j = ref (!i + 2) in
          while !j < n - 1 && !j < !i + 4 && s.[!j] >= '0' && s.[!j] <= '9' do
            code := !code * 10 + (Char.code s.[!j] - Char.code '0');
            incr j
          done;
          Buffer.add_char buf (Char.chr (!code land 255));
          i := !j
        end else begin
          (match c1 with
           | 'n'  -> Buffer.add_char buf '\n'
           | 't'  -> Buffer.add_char buf '\t'
           | 'r'  -> Buffer.add_char buf '\r'
           | '\\' -> Buffer.add_char buf '\\'
           | '"'  -> Buffer.add_char buf '"'
           | '\'' -> Buffer.add_char buf '\''
           | 'b'  -> Buffer.add_char buf '\b'
           | c    -> Buffer.add_char buf c);
          i := !i + 2
        end
      end else begin
        Buffer.add_char buf s.[!i];
        i := !i + 1
      end
    done;
    Buffer.contents buf
  end else s

let current_locals m =
  match m.frames with
  | f :: _ -> f.locals
  | []     -> m.globals

let eval_place m (p : place) : value =
  match int_of_string_opt p with
  | Some n -> VInt n
  | None ->
  match float_of_string_opt p with
  | Some f -> VFloat f
  | None ->
  if p = "NULL" then VNull
  else if String.length p >= 2 && p.[0] = '"' then VStr (unescape_str p)
  else if String.length p >= 3 && p.[0] = '\'' then VChar p.[1]
  else
    let locals = current_locals m in
    match Hashtbl.find_opt locals p with
    | Some r -> !r
    | None ->
      match Hashtbl.find_opt m.globals p with
      | Some r -> !r
      | None   -> VUndef

let set_place m (p : place) v =
  let locals = current_locals m in
  (* prefer locals, fall through to globals if already there *)
  if Hashtbl.mem locals p then
    (Hashtbl.find locals p) := v
  else if Hashtbl.mem m.globals p then
    (Hashtbl.find m.globals p) := v
  else
    Hashtbl.replace locals p (ref v)

let lookup_ref m (p : place) : value ref =
  let locals = current_locals m in
  match Hashtbl.find_opt locals p with
  | Some r -> r
  | None ->
    match Hashtbl.find_opt m.globals p with
    | Some r -> r
    | None ->
        let r = ref VUndef in
        Hashtbl.replace locals p r; r

(* ── Operações ───────────────────────────────────────────────────── *)

let truthy = function
  | VInt 0 | VFloat 0.0 | VNull | VUndef -> false
  | VChar '\000'                          -> false
  | _                                     -> true

let to_int = function
  | VInt n   -> n
  | VFloat f -> int_of_float f
  | VChar c  -> Char.code c
  | VNull    -> 0
  | _        -> 0

let to_float = function
  | VFloat f -> f
  | VInt n   -> float_of_int n
  | _        -> 0.0

let is_float = function VFloat _ -> true | _ -> false

let arith op v1 v2 =
  match op with
  | "&&" -> VInt (if truthy v1 && truthy v2 then 1 else 0)
  | "||" -> VInt (if truthy v1 || truthy v2 then 1 else 0)
  (* comparações entre ponteiros/arrays e NULL *)
  | "==" -> (match v1, v2 with
      | VNull, VNull            -> VInt 1
      | VNull, (VPtr _ | VArr _)
      | (VPtr _ | VArr _), VNull -> VInt 0
      | VPtr r1, VPtr r2        -> VInt (if r1 == r2 then 1 else 0)
      | _ -> if is_float v1 || is_float v2
             then VInt (if to_float v1 = to_float v2 then 1 else 0)
             else VInt (if to_int v1 = to_int v2     then 1 else 0))
  | "!=" -> (match v1, v2 with
      | VNull, VNull            -> VInt 0
      | VNull, (VPtr _ | VArr _)
      | (VPtr _ | VArr _), VNull -> VInt 1
      | VPtr r1, VPtr r2        -> VInt (if r1 == r2 then 0 else 1)
      | _ -> if is_float v1 || is_float v2
             then VInt (if to_float v1 <> to_float v2 then 1 else 0)
             else VInt (if to_int v1 <> to_int v2     then 1 else 0))
  | _ ->
  if is_float v1 || is_float v2 then begin
    let a = to_float v1 and b = to_float v2 in
    match op with
    | "+"  -> VFloat (a +. b)
    | "-"  -> VFloat (a -. b)
    | "*"  -> VFloat (a *. b)
    | "/"  -> VFloat (a /. b)
    | "==" -> VInt (if a = b   then 1 else 0)
    | "!=" -> VInt (if a <> b  then 1 else 0)
    | "<"  -> VInt (if a < b   then 1 else 0)
    | ">"  -> VInt (if a > b   then 1 else 0)
    | "<=" -> VInt (if a <= b  then 1 else 0)
    | ">=" -> VInt (if a >= b  then 1 else 0)
    | _    -> VUndef
  end else begin
    let a = to_int v1 and b = to_int v2 in
    match op with
    | "+"  -> VInt (a + b)
    | "-"  -> VInt (a - b)
    | "*"  -> VInt (a * b)
    | "/"  -> if b = 0 then (print_string "divisão por zero\n"; VInt 0)
               else VInt (a / b)
    | "%"  -> if b = 0 then VInt 0 else VInt (a mod b)
    | "==" -> VInt (if a = b   then 1 else 0)
    | "!=" -> VInt (if a <> b  then 1 else 0)
    | "<"  -> VInt (if a < b   then 1 else 0)
    | ">"  -> VInt (if a > b   then 1 else 0)
    | "<=" -> VInt (if a <= b  then 1 else 0)
    | ">=" -> VInt (if a >= b  then 1 else 0)
    | "&"  -> VInt (a land b)
    | "|"  -> VInt (a lor  b)
    | "^"  -> VInt (a lxor b)
    | "<<" -> VInt (a lsl  b)
    | ">>" -> VInt (a asr  b)
    | _    -> VUndef
  end

let unary op v =
  match op with
  | "-"  -> (match v with VInt n -> VInt (-n) | VFloat f -> VFloat (-.f) | _ -> VUndef)
  | "!"  -> VInt (if truthy v then 0 else 1)
  | "~"  -> VInt (lnot (to_int v))
  | _    -> VUndef

(* ── Built-ins ───────────────────────────────────────────────────── *)

(* Formato simples de printf: %d %i %u %ld %f %g %e %s %c %% *)
let do_printf args =
  match args with
  | [] -> ()
  | fmt_v :: rest ->
    let fmt = match fmt_v with VStr s -> s | v -> string_of_value v in
    let args_arr = Array.of_list rest in
    let ai = ref 0 in
    let n = String.length fmt in
    let i = ref 0 in
    while !i < n do
      if fmt.[!i] = '%' && !i + 1 < n then begin
        let spec = fmt.[!i + 1] in
        let arg = if !ai < Array.length args_arr then args_arr.(!ai) else VUndef in
        (match spec with
         | 'd' | 'i' | 'u' ->
             print_string (string_of_int (to_int arg)); incr ai
         | 'l' when !i + 2 < n && fmt.[!i + 2] = 'd' ->
             print_string (string_of_int (to_int arg)); incr ai; incr i
         | 'f' | 'e' | 'E' ->
             Printf.printf "%f" (to_float arg); incr ai
         | 'g' | 'G' ->
             Printf.printf "%g" (to_float arg); incr ai
         | 's' ->
             print_string (match arg with VStr s -> s | v -> string_of_value v);
             incr ai
         | 'c' ->
             (match arg with
              | VChar c -> print_char c
              | VInt n  -> print_char (Char.chr (n land 255))
              | _       -> ());
             incr ai
         | '%' -> print_char '%'
         | _   -> print_char '%'; print_char spec);
        i := !i + 2
      end else begin
        print_char fmt.[!i];
        incr i
      end
    done

exception Exited of int

let builtins : (string, value list -> value) Hashtbl.t =
  let t = Hashtbl.create 16 in
  let add = Hashtbl.replace t in
  add "printf"  (fun args -> do_printf args; VInt (List.length args - 1));
  add "fprintf" (fun args -> (* ignora file descriptor *)
    (match args with _ :: rest -> do_printf rest | [] -> ()); VInt 0);
  add "sprintf" (fun _args -> VStr "");  (* simplificado *)
  add "puts"    (fun args ->
    (match args with
     | VStr s :: _ -> print_string s; print_char '\n'
     | v :: _      -> print_string (string_of_value v); print_char '\n'
     | []          -> print_char '\n');
    VInt 0);
  add "putchar" (fun args ->
    (match args with
     | VInt n :: _ -> print_char (Char.chr (n land 255))
     | VChar c :: _-> print_char c
     | _           -> ());
    VInt 0);
  add "getchar" (fun _ ->
    try VInt (Char.code (input_char stdin))
    with End_of_file -> VInt (-1));
  add "exit"    (fun args ->
    let code = match args with VInt n :: _ -> n | _ -> 0 in
    raise (Exited code));
  add "abort"   (fun _ -> raise (Exited 1));
  add "malloc"  (fun args ->
    let n = match args with VInt n :: _ -> n | _ -> 1 in
    VArr (Array.make n VUndef));
  add "calloc"  (fun args ->
    let n = match args with VInt n :: VInt _ :: _ -> n | _ -> 1 in
    VArr (Array.make n (VInt 0)));
  add "free"    (fun _ -> VNull);
  add "realloc" (fun args ->
    match args with
    | VArr a :: VInt n :: _ ->
        let b = Array.make n VUndef in
        Array.blit a 0 b 0 (min n (Array.length a)); VArr b
    | _ -> VNull);
  add "strlen"  (fun args ->
    match args with VStr s :: _ -> VInt (String.length s) | _ -> VInt 0);
  add "strcmp"  (fun args ->
    match args with
    | VStr a :: VStr b :: _ -> VInt (String.compare a b)
    | _ -> VInt 0);
  add "strncmp" (fun args ->
    match args with
    | VStr a :: VStr b :: VInt n :: _ ->
        let a' = if String.length a > n then String.sub a 0 n else a in
        let b' = if String.length b > n then String.sub b 0 n else b in
        VInt (String.compare a' b')
    | _ -> VInt 0);
  add "strcpy"  (fun args ->
    match args with VStr _ :: VStr s :: _ -> VStr s | _ -> VNull);
  add "strcat"  (fun args ->
    match args with VStr a :: VStr b :: _ -> VStr (a ^ b) | _ -> VNull);
  add "atoi"    (fun args ->
    match args with
    | VStr s :: _ -> (match int_of_string_opt (String.trim s) with
                      | Some n -> VInt n | None -> VInt 0)
    | VInt n :: _ -> VInt n
    | _ -> VInt 0);
  add "atof"    (fun args ->
    match args with
    | VStr s :: _ -> (match float_of_string_opt (String.trim s) with
                      | Some f -> VFloat f | None -> VFloat 0.0)
    | _ -> VFloat 0.0);
  add "abs"     (fun args ->
    match args with VInt n :: _ -> VInt (abs n) | _ -> VInt 0);
  add "fabs"    (fun args ->
    match args with VFloat f :: _ -> VFloat (Float.abs f) | _ -> VFloat 0.0);
  add "sqrt"    (fun args ->
    match args with
    | VFloat f :: _ -> VFloat (Float.sqrt f)
    | VInt n :: _   -> VFloat (Float.sqrt (float_of_int n))
    | _ -> VFloat 0.0);
  add "pow"     (fun args ->
    match args with
    | a :: b :: _ -> VFloat (Float.pow (to_float a) (to_float b))
    | _ -> VFloat 0.0);
  add "scanf"   (fun args ->
    (* lê uma linha e converte conforme o formato — simplificado *)
    let line = try input_line stdin with End_of_file -> "" in
    let _ = args in
    VInt (if String.length line > 0 then 1 else 0));
  t

(* ── Construção da máquina ───────────────────────────────────────── *)

let build_machine (instrs : instr list) : machine =
  let arr = Array.of_list instrs in
  let labels = Hashtbl.create 16 in
  let funcs  = Hashtbl.create 8  in
  let globals = env_create () in
  (* scan único *)
  Array.iteri (fun i instr ->
    match instr with
    | Label l             -> Hashtbl.replace labels l i
    | FuncBegin (n, ps)   -> Hashtbl.replace funcs n (i, ps)
    | Global (_, n)       -> if not (Hashtbl.mem globals n) then
                               Hashtbl.replace globals n (ref VUndef)
    | _                   -> ()
  ) arr;
  { instrs = arr; labels; funcs; globals;
    pc = 0; frames = []; params = []; running = true }

(* ── Execução ────────────────────────────────────────────────────── *)

let do_return m v_opt =
  match m.frames with
  | [] ->
      m.running <- false
  | frame :: rest ->
      m.frames <- rest;
      m.pc     <- frame.ret_pc;
      (match v_opt, frame.ret_dst with
       | Some v, Some dst -> set_place m dst v
       | _ -> ())

let do_call m dst fname n =
  let nparams = List.length m.params in
  let args =
    if n <= nparams then
      let skip = nparams - n in
      let rec drop k = function x :: xs -> if k = 0 then x :: xs else drop (k-1) xs | [] -> [] in
      let rec take k = function x :: xs -> if k = 0 then [] else x :: take (k-1) xs | [] -> [] in
      let relevant = drop skip m.params in
      m.params <- take skip m.params;
      relevant
    else begin
      let args = m.params in
      m.params <- [];
      args
    end
  in
  match Hashtbl.find_opt builtins fname with
  | Some f ->
      let result = f args in
      (match dst with
       | Some d -> set_place m d result
       | None   -> ())
  | None ->
    match Hashtbl.find_opt m.funcs fname with
    | None ->
        Printf.eprintf "Função não definida: '%s'\n" fname
    | Some (start_pc, pnames) ->
        let frame_locals = env_create () in
        List.iteri (fun i pname ->
          let v = if i < List.length args then List.nth args i else VUndef in
          Hashtbl.replace frame_locals pname (ref v)
        ) pnames;
        m.frames <- { locals = frame_locals; ret_pc = m.pc; ret_dst = dst } :: m.frames;
        m.pc <- start_pc + 1

let step m =
  if m.pc >= Array.length m.instrs then begin
    m.running <- false
  end else begin
    let instr = m.instrs.(m.pc) in
    m.pc <- m.pc + 1;
    let env = current_locals m in
    match instr with
    | FuncBegin _ -> ()
    | FuncEnd     -> do_return m None
    | Label _     -> ()

    | Copy (d, s) ->
        set_place m d (eval_place m s)

    | BinOp (d, op, a, b) ->
        let va = eval_place m a and vb = eval_place m b in
        set_place m d (arith op va vb)

    | UnOp (d, op, a) ->
        set_place m d (unary op (eval_place m a))

    | Addr (d, s) ->
        set_place m d (VPtr (lookup_ref m s))

    | Deref (d, p) ->
        (match eval_place m p with
         | VPtr r  -> set_place m d !r
         | VArr a  -> set_place m d (if Array.length a > 0 then a.(0) else VUndef)
         | _       -> set_place m d VUndef)

    | DerefSet (p, v) ->
        let vv = eval_place m v in
        (match eval_place m p with
         | VPtr r -> r := vv
         | _      -> ())

    | ArrGet (d, a, i) ->
        let idx = to_int (eval_place m i) in
        (match eval_place m a with
         | VArr arr -> set_place m d (if idx >= 0 && idx < Array.length arr then arr.(idx) else VUndef)
         | VStr s   -> set_place m d (if idx >= 0 && idx < String.length s
                                      then VChar s.[idx] else VInt 0)
         | _        -> set_place m d VUndef)

    | ArrSet (a, i, v) ->
        let vv  = eval_place m v in
        let idx = to_int (eval_place m i) in
        (match Hashtbl.find_opt env a with
         | Some r ->
             (match !r with
              | VArr arr ->
                  if idx >= 0 && idx < Array.length arr then arr.(idx) <- vv
              | VUndef ->
                  let arr = Array.make 1024 VUndef in
                  arr.(idx) <- vv;
                  r := VArr arr
              | _ -> ())
         | None ->
             let arr = Array.make 1024 VUndef in
             if idx >= 0 then arr.(idx) <- vv;
             Hashtbl.replace env a (ref (VArr arr)))

    | Param p ->
        m.params <- m.params @ [eval_place m p]

    | Call (dst, fname, n) ->
        do_call m dst fname n

    | Return v_opt ->
        let v = Option.map (eval_place m) v_opt in
        do_return m v

    | Goto l ->
        (match Hashtbl.find_opt m.labels l with
         | Some pc -> m.pc <- pc
         | None    -> Printf.eprintf "Label não encontrado: '%s'\n" l)

    | IfGoto (v, l) ->
        if truthy (eval_place m v) then
          (match Hashtbl.find_opt m.labels l with
           | Some pc -> m.pc <- pc
           | None    -> Printf.eprintf "Label não encontrado: '%s'\n" l)

    | IfFGoto (v, l) ->
        if not (truthy (eval_place m v)) then
          (match Hashtbl.find_opt m.labels l with
           | Some pc -> m.pc <- pc
           | None    -> Printf.eprintf "Label não encontrado: '%s'\n" l)

    | Global (_, n) ->
        if not (Hashtbl.mem m.globals n) then
          Hashtbl.replace m.globals n (ref VUndef)

    | GlobalSet (n, v) ->
        let vv = eval_place m v in
        (match Hashtbl.find_opt m.globals n with
         | Some r -> r := vv
         | None   -> Hashtbl.replace m.globals n (ref vv))
  end

(* ── Ponto de entrada ────────────────────────────────────────────── *)

let interpret (instrs : instr list) : unit =
  let m = build_machine instrs in
  (* Executar inicializações globais (antes do primeiro FuncBegin) *)
  let first_func =
    let r = ref (Array.length m.instrs) in
    Array.iteri (fun i instr ->
      match instr with
      | FuncBegin _ -> if i < !r then r := i
      | _ -> ()
    ) m.instrs;
    !r
  in
  while m.running && m.pc < first_func do
    step m
  done;
  (* Chamar main *)
  (match Hashtbl.find_opt m.funcs "main" with
   | None ->
       (* sem main: tentar _toplevel *)
       (match Hashtbl.find_opt m.funcs "_toplevel" with
        | None -> Printf.eprintf "Nenhuma função 'main' ou código toplevel encontrado\n"
        | Some (start_pc, _) ->
            m.frames <- [{ locals = env_create (); ret_pc = Array.length m.instrs; ret_dst = None }];
            m.pc <- start_pc + 1)
   | Some (start_pc, _) ->
       m.frames <- [{ locals = env_create (); ret_pc = Array.length m.instrs; ret_dst = None }];
       m.pc <- start_pc + 1);
  (try
     while m.running do step m done
   with
   | Exited _ -> ()
   | Stack_overflow -> Printf.eprintf "Stack overflow\n"
   | e ->
       flush stdout;
       Printf.eprintf "Erro interno do interpretador (PC=%d): %s\n"
         m.pc (Printexc.to_string e));
  flush stdout
