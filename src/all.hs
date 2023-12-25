-- imports

import Data.List (sortOn)
import Distribution.Simple.Test.ExeV10 (runTest)



---------------------------------------------------------- LEXER ----------------------------------------------------------
-- linguagem alto nivel --> tokens
-- de:
-- x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)
-- para:
-- [TokenVar "x",TokenAsg,TokenNum 42,TokenSC,TokenIf,TokenVar "x",TokenLE,TokenNum 43,TokenThen,TokenVar "x",TokenAsg,TokenNum 1,TokenSC,TokenElse,TokenOB,TokenVar "x",TokenAsg,TokenNum 33,TokenSC,TokenVar "x",TokenAsg,TokenVar "x",TokenPlus,TokenNum 1,TokenSC,TokenCB]

-- x := 2+3*4
-- [TokenVar "x",TokenAsg,TokenNum 2,TokenAdd,TokenNum 3,TokenMult,TokenNum 4]


-- (... tbd)


---------------------------------------------------------- PARSER ----------------------------------------------------------
-- tokens -> arvore sintaxe abstrata

-- para: [Store "x" (Left (Num 42)),  If (Le (Var "x") (Num 43)) (Store "x" (Num 1)) (Assign "x" (Num 33) : Store "x" (Plus (Var "x") (Num 1)) : [])]

-- [ STORE "x" (Left (ADD (NUM 2) (MULT (NUM 3) (NUM 4)) )) ]    --> LEFT por ser expressao aritmetica no STORE


-- (... tbd)


---------------------------------------------------------- COMPILER ----------------------------------------------------------
-- ex: 
-- [Push 42,Store "x",Fetch "x",Push 43,Le,Branch [Push 1,Store "x"] [Fetch "x",Push 33,Store "x",Fetch "x",Fetch "x",Push 1,Add,Store "x"]]

-- [Push 4, Push 3, Mult, Push 2, Add, Store "x"]



data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- data Expr
--     = Num Int -- integer constants
--     | Add Expr Expr -- addition node
--     | Mult Expr Expr -- multiplication node
--     | Sub Expr Expr -- subtraction node
--     -- | Div Expr Expr -- division node
--     | Neg Expr -- negation node
--     | Boolean Bool -- boolean constants
--     | And Expr Expr -- and node
--     | Or Expr Expr -- or node
--     | Not Expr -- not node
--     | Eq Expr Expr -- equal node
--     | Le Expr Expr -- less than or equal node
--     -- | Var String -- variable node
--     -- | Let String Expr Expr -- let node
--     | Store String Expr -- store node
--     | If Expr Expr Expr -- if node
--     | Loop Expr Expr -- loop node

data Aexp --arithmetic expressions
    = NUM Integer -- integer constants
    | ADD Aexp Aexp -- addition node
    | MULT Aexp Aexp -- multiplication node
    | SUB Aexp Aexp -- subtraction node
    deriving Show

data Bexp --boolean expressions
    = BOOL Bool -- boolean constants
    | AND Bexp Bexp -- and node
    -- | Or Bexp Bexp -- or node
    | NOT Bexp -- not node
    | EQa Aexp Aexp -- equal node --
    | EQb Bexp Bexp -- equal node --
    | LE Aexp Aexp -- less than or equal node
    deriving Show

type Expr = Either Aexp Bexp

data Stm --statements --> TUDO
    = STORE String  (Either Aexp Bexp)-- store node
    | FETCH String
    | IF Bexp Stm Stm -- if node
    | WHILE Bexp Stm -- loop node
    | AExp Aexp
    | BExp Bexp
    deriving Show

compA :: Aexp -> Code
compA (NUM n) = [Push n]
compA (ADD e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (MULT e1 e2) = compA e2 ++ compA e1 ++ [Mult]
compA (SUB e1 e2) = compA e2 ++ compA e1 ++ [Sub]

compB :: Bexp -> Code
compB (BOOL b) = if b then [Tru] else [Fals]
compB (AND e1 e2) = compB e2 ++ compB e1 ++ [And]
compB (NOT e) = compB e ++ [Neg]
compB (EQa e1 e2) = compA e2 ++ compA e1 ++ [Equ]
compB (EQb e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (LE e1 e2) = compA e2 ++ compA e1 ++ [Le]

compile :: [Stm] -> Code
compile [] = []
compile ((STORE s expr):xs) = case expr of
    Left aexp -> compA aexp
    Right bexp -> compB bexp
    ++ [Store s] ++ compile xs
compile ((FETCH s):xs) = Fetch s : compile xs
compile ((IF bexp expr1 expr2):xs) = compB bexp ++ [Branch (compile [expr1]) (compile [expr2])] ++ compile xs
compile ((WHILE bexp expr):xs) = Loop (compB bexp) (compile [expr]) : compile xs
compile ((AExp aexp):xs) = compA aexp ++ compile xs
compile ((BExp bexp):xs) = compB bexp ++ compile xs




---------------------------------------------------------- ASSEMBLER/INTERPRETER ----------------------------------------------------------
-- de:  
-- [Push 4, Push 3, Mult, Push 2, Add, Store "x"]
-- para: 
-- ([], [], [("x", N 42)]) --> 2String   ("", "x=42")

-- import Data.List (sortOn)

-- data Inst =
--   Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
--   Branch Code Code | Loop Code Code
--   deriving Show
-- type Code = [Inst]

data ItemBool = TT | FF deriving Show
instance Eq ItemBool where
    (==) :: ItemBool -> ItemBool -> Bool
    TT == TT = True
    FF == FF = True
    _  == _  = False

data StackItem = N Integer | B ItemBool deriving Show
instance Eq StackItem where
    (==) :: StackItem -> StackItem -> Bool
    N n1 == N n2 = n1 == n2
    B b1 == B b2 = b1 == b2
    _    == _    = False

type Stack =  [StackItem]

type State = [(String, StackItem)]


createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stackItem2Str :: StackItem -> String
stackItem2Str (B TT) = "True"
stackItem2Str (B FF) = "False"
stackItem2Str (N n) = show n

-- (e) Implement the stack2Str function
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = stackItem2Str x
stack2Str (x:xs) = stackItem2Str x ++ "," ++ stack2Str xs

state2Str :: State -> String
state2Str [] = ""
state2Str [x] = fst x ++ "=" ++ stackItem2Str (snd x)
state2Str (x:xs) = fst x ++ "=" ++ stackItem2Str (snd x) ++ "," ++ state2Str xs


updateState :: String -> StackItem -> State -> State
updateState key value state = sortOn fst (updateStateUnsorted key value state)

updateStateUnsorted :: String -> StackItem -> State -> State
updateStateUnsorted key value [] = [(key, value)] --item nao foi encontrado, entao adiciona
updateStateUnsorted key value ((k,v):xs) --iterar ate encontrar o item ou chegar no final da lista
    | key == k  = (key, value) : xs
    | otherwise = (k, v) : updateStateUnsorted key value xs

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) = run (code, N n:stack, state)
run (Add:code, (N n1):(N n2):stack, state) = run (code, N (n1 + n2):stack, state)
run (Mult:code, (N n1):(N n2):stack, state) = run (code, N (n1 * n2):stack, state)
run (Sub:code, (N n1):(N n2):stack, state) = run (code, N (n1 - n2):stack, state) -- !DOUBT: n1 - n2 ou n2 - n1?
run (Tru:code, stack, state) = run (code, B TT:stack, state)
run (Fals:code, stack, state) = run (code, B FF:stack, state)
run (Equ:code, x1:x2:stack, state) = run (code, if x1 == x2 then B TT:stack else B FF:stack, state) 
run (Le:code, (N n1):(N n2):stack, state) = run (code, if n1 <= n2 then B TT:stack else B FF:stack, state)
run (And:code, (B b1):(B b2):stack, state) = run (code, if b1 == b2 && b1 == TT then B TT:stack else B FF:stack, state)
run (Neg:code, (B b):stack, state) = run (code, if b == TT then B FF:stack else B TT:stack, state)
run (Fetch var:code, stack, state) = case lookup var state of
    Just value -> run (code, value:stack, state)
    Nothing    -> error "Variable not found" 
run (Store var:code, value:stack, state) = run (code, stack, updateState var value state)
run (Noop:code, stack, state) = run (code, stack, state)
run (Branch code1 code2:code, (B b):stack, state) = run (if b == TT then code1 ++ code else code2 ++ code, stack, state)
run (Loop code1 code2:code, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)
run (_, _, _) = error "Run-time error"

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- runTests = do
--     print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
--     print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
--     print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
--     print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
--     print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
--     print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
--     print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
--     print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
--     print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    

runExpr :: [Stm] -> (String, String)
runExpr expr = testAssembler (compile expr)

runTests = do
    -- print $ runExpr [STORE "x" (Left (ADD (NUM 2) (MULT (NUM 3) (NUM 4))))] ==   ("", "x=14")                                             
    print $ runExpr [AExp (MULT (SUB (NUM 3) (NUM 4)) (NUM 10))] ==   ("-10", "")
    --  print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    print $ runExpr [STORE "someVar" (Right (BOOL False)), STORE "a" (Left (NUM 3)), STORE "var" (Right (BOOL True))] == ("","a=3,someVar=False,var=True")
    -- print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    -- idk how to test this
    -- print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    print $ runExpr [AExp (NUM (-20)), BExp (BOOL True), BExp (BOOL False)] == ("False,True,-20","")
    -- print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    print $ runExpr [AExp (NUM (-20)), BExp (BOOL True), BExp (NOT (BOOL True))] == ("False,True,-20","")
    -- print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    print $ runExpr [AExp (NUM (-20)), BExp (EQb (BOOL True) (NOT (BOOL True)))] == ("False,-20","")
    -- print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
    print $ runExpr [BExp (LE (NUM (-21)) (NUM (-20)))] == ("True","")
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    print $ runExpr [STORE "x" (Left (NUM 5)), STORE "x" (Left (SUB (FETCH "x") (NUM 1)))] == ("","x=4")