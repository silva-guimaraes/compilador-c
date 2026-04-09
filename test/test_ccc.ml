
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
           [{ Ast.tipo =
              { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                quals = [] };
              nome = (Ast.Id \"id\"); deref = 0; array_dims = [] };
             { Ast.tipo =
               { Ast.base = (Ast.Id \"char\"); sinal = None; tamanho = None;
                 quals = [] };
               nome = (Ast.Id \"name\"); deref = 0; array_dims = [] }
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
             [{ Ast.tipo =
                { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                  quals = [] };
                nome = (Ast.Id \"id\"); deref = 0; array_dims = [] };
               { Ast.tipo =
                 { Ast.base = (Ast.Id \"char\"); sinal = None; tamanho = None;
                   quals = [] };
                 nome = (Ast.Id \"name\"); deref = 0;
                 array_dims = [(Some (Ast.Const (Ast.Int 50)))] }
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
                 [{ Ast.tipo =
                    { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                      quals = [] };
                    nome = (Ast.Id \"i\"); deref = 0; array_dims = [] };
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"float\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"f\"); deref = 0; array_dims = [] }
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
           lista =
           [((Ast.Id \"RED\"), None); ((Ast.Id \"GREEN\"), None);
             ((Ast.Id \"BLUE\"), None)]
           })
       ])
   "]
;;

let%expect_test _ =
  let input = {| void printArray(int *arr, int size); |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.FuncProt
           { Ast.tipo =
             { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
               quals = [] };
             deref = 0; nome = (Ast.Id \"printArray\");
             parametros =
             [{ Ast.tipo =
                { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                  quals = [] };
                nome = (Ast.Id \"arr\"); deref = 1; array_dims = [] };
               { Ast.tipo =
                 { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                   quals = [] };
                 nome = (Ast.Id \"size\"); deref = 0; array_dims = [] }
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
             { Ast.tipo =
               { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                 quals = [] };
               deref = 0; nome = (Ast.Id \"main\"); parametros = [] };
             corpo = [] })
         ])"] ;
  let input = {| void main() { int x = 10; } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo =
               { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                 quals = [] };
               deref = 0; nome = (Ast.Id \"main\"); parametros = [] };
             corpo =
             [(Ast.VarDeclInit (
                 { Ast.tipo =
                   { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                     quals = [] };
                   nome = (Ast.Id \"x\"); deref = 0; array_dims = [] },
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
             { Ast.tipo =
               { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                 quals = [] };
               deref = 0; nome = (Ast.Id \"main\"); parametros = [] };
             corpo =
             [(Ast.VarDeclInit (
                 { Ast.tipo =
                   { Ast.base = (Ast.Id \"int\"); sinal = (Some Ast.Signed);
                     tamanho = None; quals = [] };
                   nome = (Ast.Id \"w\"); deref = 0; array_dims = [] },
                 (Ast.Uop (Ast.Neg, (Ast.Const (Ast.Int 20))))))
               ]
             })
         ])"];
  let input = {| void main() { unsigned int tt = 50; } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo =
               { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                 quals = [] };
               deref = 0; nome = (Ast.Id \"main\"); parametros = [] };
             corpo =
             [(Ast.VarDeclInit (
                 { Ast.tipo =
                   { Ast.base = (Ast.Id \"int\"); sinal = (Some Ast.Unsigned);
                     tamanho = None; quals = [] };
                   nome = (Ast.Id \"tt\"); deref = 0; array_dims = [] },
                 (Ast.Const (Ast.Int 50))))
               ]
             })
         ])"];
  let input = {| void main() { int tt[40]; } |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Func
           { Ast.prototipo =
             { Ast.tipo =
               { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                 quals = [] };
               deref = 0; nome = (Ast.Id \"main\"); parametros = [] };
             corpo =
             [(Ast.VarDecl
                 { Ast.tipo =
                   { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                     quals = [] };
                   nome = (Ast.Id \"tt\"); deref = 0;
                   array_dims = [(Some (Ast.Const (Ast.Int 40)))] })
               ]
             })
         ])"]
;;


let%expect_test _ =
  let input = {| 
#include <stdio.h>
#include <stdlib.h>
// Definição de macro
#define MAX 100
// Definição de um struct
struct Data {
  int id;
  char name[50];
};
// Definição de um union
union Value {
  int i;
  float f;
};
// Enumeração de tipos
enum Color { RED, GREEN, BLUE };
// Protótipo de função
void printArray(int *arr, int size);
int main() {
  // Declaração e inicialização de variáveis de vários tipos
  int x = 10;
  signed int w = -20;
  unsigned int tt = 50;
  long int rt = 100000;
  short int si = 5;
  float f = 3.14;
  double d = 2.718;
  char ch = 'A';
  const int constant_var = 500;
  volatile int volatile_var = 200;
  // Ponteiro e alocação dinâmica
  int *ptr = (int *)malloc(MAX * sizeof(int));
  if (ptr == NULL) {
    printf("Erro na alocação de memória\n");
    return 1;
  }
  // Inicialização de matriz
  int matrix[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
  // Inicializando struct
  struct Data data1 = {1, "John"};
  // Inicializando union
  union Value val;
  val.i = 10;
  // Inicializando enum
  enum Color color = RED;
  // Uso de if-else
  if (x > 0) {
    printf("x é positivo.\n");
  } else {
    printf("x é negativo ou zero.\n");
  }
  // Uso de switch-case
  switch (color) {
  case RED:
    printf("A cor é Vermelho.\n");
    break;
  case GREEN:
    printf("A cor é Verde.\n");
    break;
  case BLUE:
    printf("A cor é Azul.\n");
    break;
  default:
    printf("Cor desconhecida.\n");
  }
  // Uso de for
  for (int i = 0; i < 5; i++) {
    printf("For loop - Iteração %d\n", i);
  }
  // Uso de while
  int count = 0;
  while (count < 3) {
    printf("While loop - Contagem: %d\n", count);
    count++;
  }
  // Uso de do-while
  count = 0;
  do {
    printf("Do-while loop - Contagem: %d\n", count);
    count++;
  } while (count < 3);
  // Uso de ponteiro em uma função
  for (int i = 0; i < 10; i++) {
    ptr[i] = i * 10;
  }
  printArray(ptr, 10);
  // Registro de variável
  register int fast_var = 30;
  printf("Variável registrada: %d\n", fast_var);
  // Exemplo de typedef
  //
  typedef unsigned long ULong;
  ULong bigNum = 123456789;
  printf("ULong: %lu\n", bigNum);
  // Liberação de memória alocada
  free(ptr);
  printf("Memória desalocada com sucesso.\n");
  return 0;
}
// Função para imprimir um array usando ponteiro
void printArray(int *arr, int size) {
  printf("Array dinâmico: ");
  for (int i = 0; i < size; i++) {
    printf("%d ", arr[i]);
  }
  printf("\n");
}
    |} in
  print_string @@ Ast.show_programa @@ Interface.parse input;
  [%expect "
    (Ast.Programa
       [(Ast.Struct
           { Ast.nome = (Ast.Id \"Data\");
             atributos =
             [{ Ast.tipo =
                { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                  quals = [] };
                nome = (Ast.Id \"id\"); deref = 0; array_dims = [] };
               { Ast.tipo =
                 { Ast.base = (Ast.Id \"char\"); sinal = None; tamanho = None;
                   quals = [] };
                 nome = (Ast.Id \"name\"); deref = 0;
                 array_dims = [(Some (Ast.Const (Ast.Int 50)))] }
               ]
             });
         (Ast.Union
            { Ast.nome = (Ast.Id \"Value\");
              atributos =
              [{ Ast.tipo =
                 { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                   quals = [] };
                 nome = (Ast.Id \"i\"); deref = 0; array_dims = [] };
                { Ast.tipo =
                  { Ast.base = (Ast.Id \"float\"); sinal = None; tamanho = None;
                    quals = [] };
                  nome = (Ast.Id \"f\"); deref = 0; array_dims = [] }
                ]
              });
         (Ast.Enum
            { Ast.nome = (Ast.Id \"Color\");
              lista =
              [((Ast.Id \"RED\"), None); ((Ast.Id \"GREEN\"), None);
                ((Ast.Id \"BLUE\"), None)]
              });
         (Ast.FuncProt
            { Ast.tipo =
              { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                quals = [] };
              deref = 0; nome = (Ast.Id \"printArray\");
              parametros =
              [{ Ast.tipo =
                 { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                   quals = [] };
                 nome = (Ast.Id \"arr\"); deref = 1; array_dims = [] };
                { Ast.tipo =
                  { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                    quals = [] };
                  nome = (Ast.Id \"size\"); deref = 0; array_dims = [] }
                ]
              });
         (Ast.Func
            { Ast.prototipo =
              { Ast.tipo =
                { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                  quals = [] };
                deref = 0; nome = (Ast.Id \"main\"); parametros = [] };
              corpo =
              [(Ast.VarDeclInit (
                  { Ast.tipo =
                    { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                      quals = [] };
                    nome = (Ast.Id \"x\"); deref = 0; array_dims = [] },
                  (Ast.Const (Ast.Int 10))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = (Some Ast.Signed);
                       tamanho = None; quals = [] };
                     nome = (Ast.Id \"w\"); deref = 0; array_dims = [] },
                   (Ast.Uop (Ast.Neg, (Ast.Const (Ast.Int 20))))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = (Some Ast.Unsigned);
                       tamanho = None; quals = [] };
                     nome = (Ast.Id \"tt\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 50))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None;
                       tamanho = (Some Ast.Long); quals = [] };
                     nome = (Ast.Id \"rt\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 100000))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None;
                       tamanho = (Some Ast.Short); quals = [] };
                     nome = (Ast.Id \"si\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 5))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"float\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"f\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Float 3.14))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"double\"); sinal = None;
                       tamanho = None; quals = [] };
                     nome = (Ast.Id \"d\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Float 2.718))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"char\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"ch\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Char 'A'))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                       quals = [Ast.Const] };
                     nome = (Ast.Id \"constant_var\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 500))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                       quals = [Ast.Volatile] };
                     nome = (Ast.Id \"volatile_var\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 200))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"ptr\"); deref = 1; array_dims = [] },
                   (Ast.Cast (
                      { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                        quals = [] },
                      1,
                      (Ast.Call ((Ast.Id \"malloc\"),
                         [(Ast.Bop (Ast.Mult, (Ast.Var (Ast.Id \"MAX\")),
                             (Ast.Sizeof
                                { Ast.base = (Ast.Id \"int\"); sinal = None;
                                  tamanho = None; quals = [] })
                             ))
                           ]
                         ))
                      ))
                   ));
                (Ast.If (
                   (Ast.Bop (Ast.Eq, (Ast.Var (Ast.Id \"ptr\")),
                      (Ast.Const Ast.Null))),
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Call ((Ast.Id \"printf\"),
                             [(Ast.Const
                                 (Ast.Str
                                    \"Erro na aloca\\195\\167\\195\\163o de mem\\195\\179ria\\n\"))
                               ]
                             )));
                        (Ast.Return (Some (Ast.Const (Ast.Int 1))))]),
                   None));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"matrix\"); deref = 0;
                     array_dims =
                     [(Some (Ast.Const (Ast.Int 3)));
                       (Some (Ast.Const (Ast.Int 3)))]
                     },
                   (Ast.CompoundLit
                      [(Ast.CompoundLit
                          [(Ast.Const (Ast.Int 1)); (Ast.Const (Ast.Int 2));
                            (Ast.Const (Ast.Int 3))]);
                        (Ast.CompoundLit
                           [(Ast.Const (Ast.Int 4)); (Ast.Const (Ast.Int 5));
                             (Ast.Const (Ast.Int 6))]);
                        (Ast.CompoundLit
                           [(Ast.Const (Ast.Int 7)); (Ast.Const (Ast.Int 8));
                             (Ast.Const (Ast.Int 9))])
                        ])
                   ));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"struct Data\"); sinal = None;
                       tamanho = None; quals = [] };
                     nome = (Ast.Id \"data1\"); deref = 0; array_dims = [] },
                   (Ast.CompoundLit
                      [(Ast.Const (Ast.Int 1)); (Ast.Const (Ast.Str \"John\"))])
                   ));
                (Ast.VarDecl
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"union Value\"); sinal = None;
                       tamanho = None; quals = [] };
                     nome = (Ast.Id \"val\"); deref = 0; array_dims = [] });
                (Ast.Expr
                   (Ast.Bop (Ast.Assign,
                      (Ast.Member ((Ast.Var (Ast.Id \"val\")), (Ast.Id \"i\"))),
                      (Ast.Const (Ast.Int 10)))));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"enum Color\"); sinal = None;
                       tamanho = None; quals = [] };
                     nome = (Ast.Id \"color\"); deref = 0; array_dims = [] },
                   (Ast.Var (Ast.Id \"RED\"))));
                (Ast.If (
                   (Ast.Bop (Ast.Gt, (Ast.Var (Ast.Id \"x\")),
                      (Ast.Const (Ast.Int 0)))),
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Call ((Ast.Id \"printf\"),
                             [(Ast.Const (Ast.Str \"x \\195\\169 positivo.\\n\"))])))
                        ]),
                   (Some (Ast.Block
                            [(Ast.Expr
                                (Ast.Call ((Ast.Id \"printf\"),
                                   [(Ast.Const
                                       (Ast.Str \"x \\195\\169 negativo ou zero.\\n\"))
                                     ]
                                   )))
                              ]))
                   ));
                (Ast.Switch ((Ast.Var (Ast.Id \"color\")),
                   (Ast.Block
                      [(Ast.Case ((Ast.Var (Ast.Id \"RED\")),
                          [(Ast.Expr
                              (Ast.Call ((Ast.Id \"printf\"),
                                 [(Ast.Const
                                     (Ast.Str \"A cor \\195\\169 Vermelho.\\n\"))
                                   ]
                                 )));
                            Ast.Break]
                          ));
                        (Ast.Case ((Ast.Var (Ast.Id \"GREEN\")),
                           [(Ast.Expr
                               (Ast.Call ((Ast.Id \"printf\"),
                                  [(Ast.Const (Ast.Str \"A cor \\195\\169 Verde.\\n\"))
                                    ]
                                  )));
                             Ast.Break]
                           ));
                        (Ast.Case ((Ast.Var (Ast.Id \"BLUE\")),
                           [(Ast.Expr
                               (Ast.Call ((Ast.Id \"printf\"),
                                  [(Ast.Const (Ast.Str \"A cor \\195\\169 Azul.\\n\"))
                                    ]
                                  )));
                             Ast.Break]
                           ));
                        (Ast.Default
                           [(Ast.Expr
                               (Ast.Call ((Ast.Id \"printf\"),
                                  [(Ast.Const (Ast.Str \"Cor desconhecida.\\n\"))])))
                             ])
                        ])
                   ));
                (Ast.For (
                   (Ast.ForInitDecl (
                      { Ast.tipo =
                        { Ast.base = (Ast.Id \"int\"); sinal = None;
                          tamanho = None; quals = [] };
                        nome = (Ast.Id \"i\"); deref = 0; array_dims = [] },
                      (Some (Ast.Const (Ast.Int 0))))),
                   (Some (Ast.Bop (Ast.Lt, (Ast.Var (Ast.Id \"i\")),
                            (Ast.Const (Ast.Int 5))))),
                   (Some (Ast.Uop (Ast.PostInc, (Ast.Var (Ast.Id \"i\"))))),
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Call ((Ast.Id \"printf\"),
                             [(Ast.Const
                                 (Ast.Str
                                    \"For loop - Itera\\195\\167\\195\\163o %d\\n\"));
                               (Ast.Var (Ast.Id \"i\"))]
                             )))
                        ])
                   ));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"count\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 0))));
                (Ast.While (
                   (Ast.Bop (Ast.Lt, (Ast.Var (Ast.Id \"count\")),
                      (Ast.Const (Ast.Int 3)))),
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Call ((Ast.Id \"printf\"),
                             [(Ast.Const (Ast.Str \"While loop - Contagem: %d\\n\"));
                               (Ast.Var (Ast.Id \"count\"))]
                             )));
                        (Ast.Expr
                           (Ast.Uop (Ast.PostInc, (Ast.Var (Ast.Id \"count\")))))
                        ])
                   ));
                (Ast.Expr
                   (Ast.Bop (Ast.Assign, (Ast.Var (Ast.Id \"count\")),
                      (Ast.Const (Ast.Int 0)))));
                (Ast.DoWhile (
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Call ((Ast.Id \"printf\"),
                             [(Ast.Const
                                 (Ast.Str \"Do-while loop - Contagem: %d\\n\"));
                               (Ast.Var (Ast.Id \"count\"))]
                             )));
                        (Ast.Expr
                           (Ast.Uop (Ast.PostInc, (Ast.Var (Ast.Id \"count\")))))
                        ]),
                   (Ast.Bop (Ast.Lt, (Ast.Var (Ast.Id \"count\")),
                      (Ast.Const (Ast.Int 3))))
                   ));
                (Ast.For (
                   (Ast.ForInitDecl (
                      { Ast.tipo =
                        { Ast.base = (Ast.Id \"int\"); sinal = None;
                          tamanho = None; quals = [] };
                        nome = (Ast.Id \"i\"); deref = 0; array_dims = [] },
                      (Some (Ast.Const (Ast.Int 0))))),
                   (Some (Ast.Bop (Ast.Lt, (Ast.Var (Ast.Id \"i\")),
                            (Ast.Const (Ast.Int 10))))),
                   (Some (Ast.Uop (Ast.PostInc, (Ast.Var (Ast.Id \"i\"))))),
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Bop (Ast.Assign,
                             (Ast.Index ((Ast.Var (Ast.Id \"ptr\")),
                                (Ast.Var (Ast.Id \"i\")))),
                             (Ast.Bop (Ast.Mult, (Ast.Var (Ast.Id \"i\")),
                                (Ast.Const (Ast.Int 10))))
                             )))
                        ])
                   ));
                (Ast.Expr
                   (Ast.Call ((Ast.Id \"printArray\"),
                      [(Ast.Var (Ast.Id \"ptr\")); (Ast.Const (Ast.Int 10))])));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                       quals = [Ast.Register] };
                     nome = (Ast.Id \"fast_var\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 30))));
                (Ast.Expr
                   (Ast.Call ((Ast.Id \"printf\"),
                      [(Ast.Const (Ast.Str \"Vari\\195\\161vel registrada: %d\\n\"));
                        (Ast.Var (Ast.Id \"fast_var\"))]
                      )));
                (Ast.TypedefDecl (
                   { Ast.base = (Ast.Id \"int\"); sinal = (Some Ast.Unsigned);
                     tamanho = (Some Ast.Long); quals = [] },
                   0, (Ast.Id \"ULong\")));
                (Ast.VarDeclInit (
                   { Ast.tipo =
                     { Ast.base = (Ast.Id \"ULong\"); sinal = None; tamanho = None;
                       quals = [] };
                     nome = (Ast.Id \"bigNum\"); deref = 0; array_dims = [] },
                   (Ast.Const (Ast.Int 123456789))));
                (Ast.Expr
                   (Ast.Call ((Ast.Id \"printf\"),
                      [(Ast.Const (Ast.Str \"ULong: %lu\\n\"));
                        (Ast.Var (Ast.Id \"bigNum\"))]
                      )));
                (Ast.Expr
                   (Ast.Call ((Ast.Id \"free\"), [(Ast.Var (Ast.Id \"ptr\"))])));
                (Ast.Expr
                   (Ast.Call ((Ast.Id \"printf\"),
                      [(Ast.Const
                          (Ast.Str \"Mem\\195\\179ria desalocada com sucesso.\\n\"))
                        ]
                      )));
                (Ast.Return (Some (Ast.Const (Ast.Int 0))))]
              });
         (Ast.Func
            { Ast.prototipo =
              { Ast.tipo =
                { Ast.base = (Ast.Id \"void\"); sinal = None; tamanho = None;
                  quals = [] };
                deref = 0; nome = (Ast.Id \"printArray\");
                parametros =
                [{ Ast.tipo =
                   { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                     quals = [] };
                   nome = (Ast.Id \"arr\"); deref = 1; array_dims = [] };
                  { Ast.tipo =
                    { Ast.base = (Ast.Id \"int\"); sinal = None; tamanho = None;
                      quals = [] };
                    nome = (Ast.Id \"size\"); deref = 0; array_dims = [] }
                  ]
                };
              corpo =
              [(Ast.Expr
                  (Ast.Call ((Ast.Id \"printf\"),
                     [(Ast.Const (Ast.Str \"Array din\\195\\162mico: \"))])));
                (Ast.For (
                   (Ast.ForInitDecl (
                      { Ast.tipo =
                        { Ast.base = (Ast.Id \"int\"); sinal = None;
                          tamanho = None; quals = [] };
                        nome = (Ast.Id \"i\"); deref = 0; array_dims = [] },
                      (Some (Ast.Const (Ast.Int 0))))),
                   (Some (Ast.Bop (Ast.Lt, (Ast.Var (Ast.Id \"i\")),
                            (Ast.Var (Ast.Id \"size\"))))),
                   (Some (Ast.Uop (Ast.PostInc, (Ast.Var (Ast.Id \"i\"))))),
                   (Ast.Block
                      [(Ast.Expr
                          (Ast.Call ((Ast.Id \"printf\"),
                             [(Ast.Const (Ast.Str \"%d \"));
                               (Ast.Index ((Ast.Var (Ast.Id \"arr\")),
                                  (Ast.Var (Ast.Id \"i\"))))
                               ]
                             )))
                        ])
                   ));
                (Ast.Expr
                   (Ast.Call ((Ast.Id \"printf\"), [(Ast.Const (Ast.Str \"\\n\"))])))
                ]
              })
         ])"]
;;

