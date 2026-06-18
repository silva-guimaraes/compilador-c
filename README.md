# Compilador C em OCaml

Front-end de C escrito em OCaml. Funcionalidades:

- Lê um arquivo `.c`.

- Validação semântica.

- Geração código de três endereços (TAC).

- Execução do programa (bem limitado).

OCaml foi a nossa escolhada pela facilidade da definição da gramática da linguagem com a ajuda das bibliotecas [ocamllex](https://ocaml.org/manual/5.4/lexyacc.html) e [menhir](https://gallium.inria.fr/~fpottier/menhir/) em conjunto com a checagem de tipos forte.

Abaixo descrevemos um guia de como instalar o OCaml pela primeira vez.

## Instalação

### Linux / macOS

```sh
# 1. Instalar opam (gerenciador de pacotes do OCaml)
bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"

# 2. Inicializar o opam (só na primeira vez)
opam init --bare -a
eval $(opam env)

# 3. Criar um switch com OCaml 5.x
opam switch create 5.3.0
eval $(opam env)

# 4. Instalar as dependências do projeto
opam install dune menhir menhirLib ppx_deriving ppx_expect ounit2
```

### Windows

Use o [instalador oficial do opam para Windows](https://opam.ocaml.org/doc/Install.html#Windows) ou o WSL (recomendado) e siga os passos acima dentro do WSL.

---

## Compilando o projeto

```sh
# Dentro da pasta do projeto:
dune build
```

Se aparecer a mensagem `Done`, o projeto compilou com sucesso.

---

## Usando o compilador

### Analisar um arquivo C

```sh
dune exec ccc -- entrada_01.txt
```

Isso verifica semanticamente e imprime a AST (Árvore Sintática Abstrata). Se houver erros semânticos, eles são listados e o programa encerra.

### Executar o programa

```sh
dune exec ccc -- -run entrada_01.txt
```

Roda o arquivo C diretamente via interpretador TAC — sem precisar de GCC ou outro compilador.

### Ver o código de três endereços gerado

```sh
dune exec ccc -- -tac entrada_01.txt
```

### Listar os tokens reconhecidos

```sh
dune exec ccc -- -tokens entrada_01.txt
```

### Exportar a AST como grafo (requer Graphviz)

```sh
dune exec ccc -- -dot saida.dot entrada_01.txt
dot -Tpng saida.dot -o saida.png
```

### Ler da entrada padrão

```sh
echo "int main() { return 0; }" | dune exec ccc
```

---

## Rodando os testes

```sh
dune test
```

Todos os testes são *expect tests*: comparam a saída atual com a esperada, definida no próprio arquivo de teste. Se uma saída mudar intencionalmente, rode:

```sh
dune test --auto-promote
```

---

## O que o compilador suporta

- Tipos: `int`, `float`, `char`, ponteiros (`*`), arrays, `struct`, `union`, `enum`, `typedef`
- Expressões: operadores aritméticos, lógicos, bitwise, ternário, cast, `sizeof`, `++`/`--`
- Comandos: `if/else`, `while`, `do-while`, `for`, `switch/case`, `break`, `continue`, `goto`, `return`
- Funções com parâmetros e recursão
- Variáveis globais e locais
- Funções de biblioteca simuladas: `printf`, `scanf`, `puts`, `putchar`, `malloc`, `free`, `strlen`, `strcmp`, `atoi`, `abs`, `sqrt`, e outras
- Diretivas `#include` e `#define` são **ignoradas** pelo lexer (não há pré-processador real)

## O que não suporta

- Tipos `double`, `long long`, `unsigned` como tipos distintos (tratados como `int`/`float`)
- Ponteiros para funções
- Structs por valor (passagem/retorno) — acesso a campos funciona via variáveis simples
