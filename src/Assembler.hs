module Assembler where

import Data.List (sortOn)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

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

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

runTests = do
    print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
    print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "x"]
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "y"]
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "x", Fetch "y"]
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "x", Fetch "y", Fetch "x"]
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "x", Fetch "y", Fetch "x", Fetch "y"]
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "x", Fetch "y", Fetch "x", Fetch "y", Fetch "x"]
    -- print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x", Fetch "x", Fetch "y", Fetch "x", Fetch "y", Fetch "x", Fetch "y"]