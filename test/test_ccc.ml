
open Ccc

let%expect_test _ =
  print_string "foobar";
  [%expect "foobar"]
;;

let%expect_test _ =
  let input = {| struct Data { int id; char name; }; |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
  (Ast.Programa
     [(Ast.Struct
         { Ast.nome = (Ast.Id \"Data\");
           atributos =
           [{ Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"id\"); deref = 0;
              array_len = None; sinal = None };
             { Ast.tipo = (Ast.Id \"char\"); nome = (Ast.Id \"name\"); deref = 0;
               array_len = None; sinal = None }
             ]
           })
       ])"]
;;

let%expect_test _ =
  let input = {| struct Data { int id; char name[50]; }; |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Struct
           { Ast.nome = (Ast.Id \"Data\");
             atributos =
             [{ Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"id\"); deref = 0;
                array_len = None; sinal = None };
               { Ast.tipo = (Ast.Id \"char\"); nome = (Ast.Id \"name\"); deref = 0;
                 array_len = (Some 50); sinal = None }
               ]
             })
         ])"]
;;

let%expect_test _ =
  let input = {| union Value { int i; float f; }; |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
        (Ast.Programa
           [(Ast.Union
               { Ast.nome = (Ast.Id \"Value\");
                 atributos =
                 [{ Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"i\"); deref = 0;
                    array_len = None; sinal = None };
                   { Ast.tipo = (Ast.Id \"float\"); nome = (Ast.Id \"f\"); deref = 0;
                     array_len = None; sinal = None }
                   ]
                 })
             ])
   "]
;;

let%expect_test _ =
  let input = {| enum Color { RED, GREEN, BLUE }; |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
  (Ast.Programa
     [(Ast.Enum
         { Ast.nome = (Ast.Id \"Color\");
           lista = [(Ast.Id \"RED\"); (Ast.Id \"GREEN\"); (Ast.Id \"BLUE\")] })
       ])
   "]
;;

let%expect_test _ =
  let input = {| void printArray(int *arr, int size); |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.FuncProt
           { Ast.tipo = (Ast.Id \"void\"); nome = (Ast.Id \"printArray\");
             parametros =
             [{ Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"arr\"); deref = 1;
                array_len = None; sinal = None };
               { Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"size\"); deref = 0;
                 array_len = None; sinal = None }
               ]
             })
         ])"] ;;

let%expect_test _ =
  let input = {| void main() { } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo = (Ast.Id \"void\"); nome = (Ast.Id \"main\");
               parametros = [] };
             corpo = [] })
         ])"] ;
  let input = {| void main() { int x = 10; } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo = (Ast.Id \"void\"); nome = (Ast.Id \"main\");
               parametros = [] };
             corpo =
             [(Ast.VarDeclInit (
                 { Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"x\"); deref = 0;
                   array_len = None; sinal = None },
                 (Ast.Const (Ast.Int 10))))
               ]
             })
         ])"];
  let input = {| void main() { signed int w = -20; } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo = (Ast.Id \"void\"); nome = (Ast.Id \"main\");
               parametros = [] };
             corpo =
             [(Ast.VarDeclInit (
                 { Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"w\"); deref = 0;
                   array_len = None; sinal = (Some Ast.Signed) },
                 (Ast.Const (Ast.Int -20))))
               ]
             })
         ])"];
  let input = {| void main() { unsigned int tt = 50; } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo = (Ast.Id \"void\"); nome = (Ast.Id \"main\");
               parametros = [] };
             corpo =
             [(Ast.VarDeclInit (
                 { Ast.tipo = (Ast.Id \"int\"); nome = (Ast.Id \"tt\"); deref = 0;
                   array_len = None; sinal = (Some Ast.Unsigned) },
                 (Ast.Const (Ast.Int 50))))
               ]
             })
         ])"]
;;

  

