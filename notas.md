ter espaços entre numeros e operadores -> usar words
- parentesis não têm espaços


span (<3) [1,2,3,4,5] = ([1,2],[3,4,5])
span (<3) [1,4,5,2,3] = ([1],[4,5,2,3]) --primeiro prefixo da lista de numeros que satisfaz a condicao <3 (o resto vai para o 2º argumento)
span (<3) [1,2,3] = ([1,2],[3])
span isAlpha "x:=x+1" = ("x",":=x+1")


lexer (c:cs)
    | isAlpha c = lexVar (c:cs) -- SO chamo o lexVar quando encontro um caracter | apanha o 1º caracter de 'while' por ex, por isso vamos ter um caso especial para isso
    | isDigit c = lexNum (c:cs)
    | isSpace c = lexer cs

lexVar cs = case (span isAlpha cs) of
    ("while",rest) -> TokenWhile : lexer rest
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest) -> TokenThen : lexer rest
    ("else",rest) -> TokenElse : lexer rest
    ("do",rest) -> TokenDo : lexer rest
    ("skip",rest) -> TokenSkip : lexer rest
    ("(",rest) -> TokenOB : lexer rest
    (")",rest) -> TokenCB : lexer rest
    (";",rest) -> TokenSC : lexer rest
    (":=",rest) -> TokenAsg : lexer rest
    ("+",rest) -> TokenPlus : lexer rest
    ("-",rest) -> TokenMinus : lexer rest
    ("*",rest) -> TokenTimes : lexer rest
    ("/",rest) -> TokenDiv : lexer rest
    ("<",rest) -> TokenLT : lexer rest
    ("<=",rest) -> TokenLE : lexer rest
    (">",rest) -> TokenGT : lexer rest
    (">=",rest) -> TokenGE : lexer rest
    ("==",rest) -> TokenEQ : lexer rest
    ("/=",rest) -> TokenNE : lexer rest
    (var,rest) -> TokenVar var : lexer rest --quando é uma variavel

**apontamentos #1**
```
1 - transformar string numa lista que faz sentido
2 - transf lista em data que representa a mesma expressao
3- passar para uma lista que é o nosso assembly --> compiler
4 - run que corre o programa de assembly, colocando em stack

simular memoria em Haskell com lista de pares variavel-valor

a stack é uma lista de inteiros 

se apanhar uma op, tipo 'add' (que tem 2 argumentos), gera codigo para o 1º arg -> o push para o 1º argumento, depois gera codigo para o 2º arg

compiler é facil, é recursivo. por ex para o add (a1, a2) , chamar recursivamente o compiler 
concantenar lista (++) recursivamente

mais trabalhoso vao ser as primeiras funcoes (lexer e buildData). prof diz para usar o "words", que admite espaços
lexer -> converte texto corrido em tokens numa lista
o buildData tem que ter logica , ter em conta as precedencias (+ e * por exemplo)
ter operadores associados a precedencia tipo Prolog, feito por nós

3 compiladores - compile A (expressoes aritmeticas, gera codigo por ex para X=2+3. gera codigo para expr aritmetica, primeiro para 2+3 e depois store X), compile B (expressoes boleanas, que aparece um ciclo while ou um if)

```
**apontamentos #2**
```
- assumir que nao tem espaços os parentesis
- usar modulo para separar as strings (o prof disse o nome)
- por causa da precedencia das operacoes --> ter varias funcoes parsers (a funcao de cima da foto chama as de baixo)
- teremos que ter outro caso (foto do quadro da esquerda) para cada uma das outras operacoes (falta - e /, o prof fez so + e *)
- as funcoes de parser tentam encontrar a operacao em questao na string 
- uma que reconhece somas e subtracoes que chama as funcoes que reconhecem o * e os / , que por sua vez chamam os parser de inteiros (a ordem da chamada das funcoes é a ordem das precedencias!!)
- "nao se preocupem com erros sintaticos"
- chamamos a funcao "error" em haskell com syntax error
- o parsec faz todo este processo, so que teriamos que aprender mt coisa, algo que nao foi ensinado
- COMEÇAR A FAZER TUDO MENOS PARSER (se nao tivermos tempo de parser, ja temos cotaçao, podemos ter o compiler a funcionar) 
```


-----

22dez2023

1. Lexer (also known as Lexical Analyzer): The first stage of the process. It takes the source code as input and breaks it down into a sequence of tokens. Tokens are the smallest meaningful units of the program, like keywords, identifiers, literals, operators, etc.

2. Parser: The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct. (ULTIMO A FAZER?)

3. Compiler: The final stage. It takes the AST produced by the parser and translates it into a lower-level representation, such as assembly code or machine code, or an intermediate representation for a virtual machine (like Java bytecode). The compiler also performs optimizations to improve the efficiency of the resulting code.


4. (DONE) Run (or Interpreter): The final stage. It takes the lower-level representation produced by the compiler and executes it, possibly producing some output.


gui:
**texto alto nível** -- (lexer, parser) --> **data** -- (compiler) --> **código assembled** -- (run) --> resultado (output do calculo)

-> lexer recebe linguagem de input (alto nivel) e transforma em tokens (palavras reservadas, variaveis, numeros, etc)
-> parser recebe tokens e transforma em data (expressoes aritmeticas, etc) - organiza os tokens numa arvore de sintaxe abstrata (AST)
-> compiler recebe data e transforma em codigo assembled (codigo de maquina)
-> run recebe codigo assembled e executa-o, produzindo um resultado


inicio:
x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)

lexer:
[TokenVar "x",TokenAsg,TokenNum 42,TokenSC,TokenIf,TokenVar "x",TokenLE,TokenNum 43,TokenThen,TokenVar "x",TokenAsg,TokenNum 1,TokenSC,TokenElse,TokenOB,TokenVar "x",TokenAsg,TokenNum 33,TokenSC,TokenVar "x",TokenAsg,TokenVar "x",TokenPlus,TokenNum 1,TokenSC,TokenCB]

parser:
Assign "x" (Num 42) : If (Le (Var "x") (Num 43)) (Assign "x" (Num 1)) (Assign "x" (Num 33) : Assign "x" (Plus (Var "x") (Num 1)) : [])

compiler:
[Push 42,Store "x",Fetch "x",Push 43,Le,Branch [Push 1,Store "x"] [Fetch "x",Push 33,Store "x",Fetch "x",Fetch "x",Push 1,Add,Store "x"]]

run/assembler:
(code, stack, state) 

([],[43,1],[])

([],[1],[])

([],[33,1],[])

([],[34], [])

([],[1], [])

([],[42], []) wth


---

Enunciado:

Assignment Description
1. Consider a low-level machine with configurations of the form (c, e, s) where c is a list
of instructions (or code) to be executed, e is the evaluation stack, and s is the storage.
We use the evaluation stack to evaluate arithmetic (composed of integer numbers only,
which can positive or negative) and boolean expressions.
The instructions of the machine are: push-n, add, mult, sub, true, false, eq, le,
and, neg, fetch-x, store-x, noop, branch(c1, c2) and loop(c1, c2).
There are several basic arithmetic and boolean operations:
• add, mult and sub add, subtract and multiply the top two integer values of the
stack, respectively, and push the result onto the top of the stack. In particular,
sub subtracts the topmost element of the stack with the second topmost element.
• eq and le compare the top two values of the stack for equality and inequality,
respectively, and push a boolean with the comparison result onto the top of the
stack. eq can compare both integers and booleans, while le only works for integers.
In particular, le determines whether the topmost stack element is less or equal to
the second topmost element.
In addition to the usual arithmetic and boolean operations, there are six instructions
that modify the evaluation stack:
• push-n pushes a constant value n onto the stack; true and false push the constants
tt and ff, respectively, onto the stack.
• fetch-x pushes the value bound to x onto the stack, whereas store-x pops the
topmost element of the stack and updates the storage so that the popped value is
bound to x.
• branch(c1, c2) will also change the flow of control: if the top of the stack is the
value tt (that is, some boolean expression has been evaluated to true), then the
stack is popped and c1 is to be executed next. Otherwise, if the top element of the
stack is ff, then it will be popped and c2 will be executed next.
• noop is a dummy instruction that returns the input stack and store.
There are two instructions that change the flow of control:
• branch(c1, c2) will be used to implement the conditional: as described above, it
will choose the code component c1 or c2 depending on the current value on top of
the stack. If the top of the stack is not a truth value, the machine will halt as there
is no next configuration (since the meaning of branch(···,···) is not defined in that
case).
• A looping construct such as a while loop can be implemented using the instruction
loop(c1, c2). The semantics of this instruction is defined by rewriting it to a combination of other constructs, including the branch instruction and itself. For example,
loop(c1, c2) may be transformed into c1 [branch([c2, loop(c1, c2)], [noop])].1
Example 1 Assuming that the initial storage s has the pair (x, 3) (meaning that variable
x has value 3), running step by step the program [push − 1, fetch − x, add, store − x]
with an empty stack and storage s, represented by the triple
([push − 1, fetch − x, add, store − x], [], [(x, 3)])
gives (step by step): ([fetch−x, add, store−x], [1], [(x, 3)]), ([add, store−x], [3, 1], [(x, 3)]),
([store − x], [4], [(x, 3)]) and finally the output ([], [], [(x, 4)])
Example 2 Running step by step the program [loop([true], [noop])] with an empty
stack and storage s, represented by the triple
([loop([true], [noop])], [], s)
gives (step by step):
• ([loop([true], [noop])], [], s)
• ([true, branch([noop, loop([true], [noop])], [noop])], [], s)
• ([branch([noop, loop(true, [noop])], [noop])], [tt], s)
• ([noop, loop([true], [noop])], [], s)
• ([loop([true], [noop])], [], s)
• ...
and the unfolding of the loop-instruction is repeated. This is an example of an infinite
looping computation sequence.
In your program use the following data declaration in Haskell to represent instructions in
the previous machine (Inst), alongside with a type synonym for a sequence of instructions
(Code). The auxiliary .hs file provided in Moodle already contains these declarations to
be use in the assignment.
data I n s t =
Push Integer | Add | Mult | Sub |
Tru | Fal s | Equ | Le | And | Neg |
Fetch String | S to r e String | Noop |
Branch Code Code | Loop Code Code
deriv ing Show
type Code = [ I n s t ]


QUESTIONS

(a) Define a new type to represent the machine’s stack. The type must be named Stack.
(b) Define a new type to represent the machine’s state. The type must be named State.
(c) Implement the createEmptyStack function which returns an empty machine’s stack.
createEmptyStack :: Stack
(d) Implement the createEmptyState function which returns an empty machine’s state.
createEmptyState :: State
(e) Implement the stack2Str function which converts a stack given as input to a string.
The string represents the stack as an ordered list of values, separated by commas
and without spaces, with the leftmost value representing the top of the stack.
stack2Str :: Stack → String
For instance, after executing the code [push−42, true, f alse], the string representing
the stack is: False,True,42