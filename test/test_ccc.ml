
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
              array_len = None };
             { Ast.tipo = (Ast.Id \"char\"); nome = (Ast.Id \"name\"); deref = 0;
               array_len = None }
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
                array_len = None };
               { Ast.tipo = (Ast.Id \"char\"); nome = (Ast.Id \"name\"); deref = 0;
                 array_len = (Some 50) }
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
                    array_len = None };
                   { Ast.tipo = (Ast.Id \"float\"); nome = (Ast.Id \"f\"); deref = 0;
                     array_len = None }
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

