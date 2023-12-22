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
3- passar para uma lista que é o nosso assembly
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

2. Parser: The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct.

3. Compiler: The final stage. It takes the AST produced by the parser and translates it into a lower-level representation, such as assembly code or machine code, or an intermediate representation for a virtual machine (like Java bytecode). The compiler also performs optimizations to improve the efficiency of the resulting code.