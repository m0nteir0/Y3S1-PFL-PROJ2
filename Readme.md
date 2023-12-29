" 
The readme file should include the identification of the group (group name, student number, and full name of each member of the group), as well as an indication of the contribution
(in percentages, adding up to 100%) of each member of the group to the assignment. It
should also describe the strategy used for solving both parts of the assignment, detailing the
decisions made when defining the data and functions defined in the program.
"

--> ACRESCENTAR PEDAÇOS DE CODIGO
--> COLOCAR TUDO EM INGLES (> portugues)
--> DEFINIR TIPOS
--> COLOCAR IMAGENS (PRINTS)

# Projeto de Haskell - Compilador

## Grupo:

|Nome | Contribuição |
|---------------------------------------------|-----|
|Guilherme Brandão Monteiro (up202108668)     | 50% |
|Sofia Resende Ferreira de Sá (up202108676)   | 50% |


## Instalação e Execução

Para executar o projeto, é necessário ter instalado o [GHC](https://www.haskell.org/ghc/) e dar download [...]


## Descrição do Projeto

Para este projeto, foi-nos proposto a implementação de um compilador para uma máquina de baixo nível, que recebe como input um programa escrito numa linguagem de alto nível e o converte para uma lista de instruções que a máquina de baixo nível consegue interpretar.


Para tal, dividimos o projeto em 3 subtarefas:
- Parser: inicialmente, o Parser recebe o input, que é um programa escrito numa linguagem de alto nível, e converte-o para uma lista de **tokens**, a partir do `lexer`. De seguida, `buildData` recebe a lista de tokens e transforma em `Data`, que é uma representação interna do programa. 
- Compilador: recebe o `Data` e converte-o em código de máquina/assembled, de baixo nível, interpretável pela máquina.
- Assembler: recebe o código de máquina/assembled e executa-o, produzindo o output final.

Um exemplo de execução do programa é o seguinte:

<src img=".png" width="500"> **img**


## Parser

### Lexer

In our project, the Lexer_1.hs module plays a crucial role in the initial phase of processing the source code. It's responsible for lexical analysis, which is the process of converting a sequence of characters into a sequence of tokens.

The lexer recognizes a variety of tokens, including keywords like "while", "do", "if", "then", "else", and "var". It also recognizes operators such as "+", "-", "*", and ":=", as well as parentheses, integers, boolean values, and the end of statement symbol ";".

The VarTok token represents a variable, which always starts with a lowercase letter according to our language's syntax rules. The AssignTok token represents the assignment operator, which assigns a value to a variable.

The lexer also includes an instance of the Eq class for the Token type. This allows us to compare tokens for equality, which is essential for the parser to correctly interpret the sequence of tokens.

By breaking down the source code into these tokens, the lexer simplifies the code into a format that's easier for the parser to handle. This sets the stage for the parser to analyze the token sequence according to the rules of our programming language's grammar.



### BuildData (gerado pelo copilot)

The BuildData_1.hs module is responsible for converting the token sequence into a Data type, which is an internal representation of the program. This is the first step in the process of compiling the source code into machine code. The Data type is a list of instructions, which are represented by the Instr type. The Instr type is a sum type that can represent a variety of instructions, including arithmetic operations, boolean operations, and control flow operations. The Data type also includes a list of variables, which are represented by the Var type. The Var type is a sum type that can represent a variety of variables, including integers, booleans, and lists.



## Compiler


The Compiler_3.hs module in our project is responsible for the compilation process of our programming language. It takes the abstract syntax tree (AST) produced by the parser and generates a sequence of instructions that can be executed by the virtual machine.

The module defines several functions for compiling different types of expressions:

- `compA` compiles arithmetic expressions (`Aexp`). It handles integer numbers (`NUM`), addition (`ADD`), multiplication (`MULT`), and subtraction (`SUB`).

- `compB` compiles boolean expressions (`Bexp`). It handles boolean values (`BOOL`), logical AND (`AND`), logical NOT (`NOT`), equality of arithmetic expressions (`EQa`), equality of boolean expressions (`EQb`), and less than or equal to (`LE`).

- `compile` compiles statements (`Stm`). It handles variable assignment (`STORE`), conditional statements (`IF`), and while loops (`WHILE`). It also handles standalone arithmetic and boolean expressions (`AExp` and `BExp`).

The `compile` function is recursive, allowing it to handle a list of statements. For each statement, it generates the appropriate instructions and then recursively compiles the rest of the list.

*The module also includes a `runTests function, which tests the compiler with various expressions and statements. This helps ensure that the compiler is working correctly.*

The Compiler.hs module is a crucial part of our project, transforming the parsed source code into a format that can be executed by the virtual machine.




## Assembler

The Assembler_4.hs module in our project is responsible for the execution of the compiled code. It defines an abstract machine that executes the instructions generated by the compiler.

The module defines several data types:

- `Inst` represents the instructions that the machine can execute. These include arithmetic operations (`Push`, `Add`, `Mult`, `Sub`), boolean operations (`Tru`, `Fals`, `Equ`, `Le`, `And`, `Neg`), variable operations (`Fetch`, `Store`), and control flow operations (`Branch`, `Loop`).

- `StackItem` represents the items that can be stored on the stack. These include integers (`N`) and boolean values (`B`).

- `State` represents the state of the machine, which is a list of variable-value pairs.

The module also defines several functions:

- `run` executes the instructions. It takes a tuple of the remaining code, the current stack, and the current state, and returns the updated tuple after executing the next instruction. The function is recursive, allowing it to execute a list of instructions.

- `updateState` updates the state by adding a new variable-value pair or updating the value of an existing variable.

- `testAssembler` tests the assembler with a list of instructions. It returns the final stack and state as strings.

The Assembler.hs module is a crucial part of our project, executing the instructions generated by the compiler and maintaining the state of the machine.


## Exemplos de Execução para cada parte do projeto

### Parser

Input:
```
x := 1;
y := 2;
while x < 10 do
    x := x + 1;
    y := y * 2;
```

Output:
```
[Assign "x" (Num 1),Assign "y" (Num 2),While (Le (Var "x") (Num 10)) [Assign "x" (Add (Var "x") (Num 1)),Assign "y" (Mult (Var "y") (Num 2))]]
```

### Compiler

Input:
```
[Assign "x" (Num 1),Assign "y" (Num 2),While (Le (Var "x") (Num 10)) [Assign "x" (Add (Var "x") (Num 1)),Assign "y" (Mult (Var "y") (Num 2))]]
```

Output:
```
[Push (N 1),Store "x",Push (N 2),Store "y",Fetch "x",Push (N 10),Le,Branch [Fetch "x",Push (N 1),Add,Store "x",Fetch "y",Push (N 2),Mult,Store "y",Fetch "x",Push (N 10),Le,Branch [Fetch "x",Push (N 1),Add,Store "x",Fetch "y",Push (N 2),Mult,Store "y",Fetch "x",Push (N 10),Le,Branch [Fetch "x",Push (N 1),Add,Store "x",Fetch "y",Push (N 2),Mult,Store "y",Fetch "x",Push (N 10),Le,Branch []]]]]
```

### Assembler

Input:
```
[Push (N 1),Store "x",Push (N 2),Store "y",Fetch "x",Push (N 10),Le,Branch [Fetch "x",Push (N 1),Add,Store "x",Fetch "y",Push (N 2),Mult,Store "y",Fetch "x",Push (N 10),Le,Branch [Fetch "x",Push (N 1),Add,Store "x",Fetch "y",Push (N 2),Mult,Store "y",Fetch "x",Push (N 10),Le,Branch [Fetch "x",Push (N 1),Add,Store "x",Fetch "y",Push (N 2),Mult,Store "y",Fetch "x",Push (N 10),Le,Branch []]]]]
```

Output:
```
Final Stack: [N 10]
Final State: [("x",N 10),("y",N 1024)]
```



## Conclusões

Em suma, este projeto permitiu-nos consolidar os conhecimentos adquiridos ao longo das aulas teóricas e práticas, nomeadamente sobre a linguagem Haskell e a sua sintaxe, mas também levou à exploração de um assunto novo e relevante - estudar o funcionamento e implementação de um compilador.

Através da realização deste projeto, conseguimos compreender melhor o funcionamento de um compilador, desde a análise léxica e sintática, até à geração de código de máquina e execução do mesmo, tendo sido uma tarefa desafiante.




## Bibliografia
