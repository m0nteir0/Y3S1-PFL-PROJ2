data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Stack accepts integers, tt and ff as values

-- data StackItem = IntItem Integer | String
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
-- data StackItem = N Integer | TT | FF deriving Show

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
-- stack2Str [x]
--   | TT <- x = "True"
--   | FF <- x = "False"
--   | N n <- x = show n
-- stack2Str (x:xs) 
--   | TT <- x = "True," ++ stack2Str xs
--   | FF <- x = "False," ++ stack2Str xs
--   | N n <- x = show n ++ "," ++ stack2Str xs

state2Str :: State -> String
state2Str [] = ""
state2Str [x] = fst x ++ "=" ++ stackItem2Str (snd x)
state2Str (x:xs) = fst x ++ "=" ++ stackItem2Str (snd x) ++ "," ++ state2Str xs

updateState :: String -> StackItem -> State -> State
updateState key value [] = [(key, value)] --item nao foi encontrado, entao adiciona
updateState key value ((k,v):xs) --iterar ate encontrar o item ou chegar no final da lista
    | key == k  = (key, value) : xs
    | otherwise = (k, v) : updateState key value xs

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) = run (code, N n:stack, state)
run (Add:code, (N n1):(N n2):stack, state) = run (code, N (n1 + n2):stack, state)
run (Mult:code, (N n1):(N n2):stack, state) = run (code, N (n1 * n2):stack, state)
run (Sub:code, (N n1):(N n2):stack, state) = run (code, N (n1 - n2):stack, state) -- !DOUBT: n1 - n2 ou n2 - n1?
run (Tru:code, stack, state) = run (code, B TT:stack, state)
run (Fals:code, stack, state) = run (code, B FF:stack, state)
run (Equ:code, x1:x2:stack, state) = run (code, if x1 == x2 then B TT:stack else B FF:stack, state) 
-- run (Equ:code, (N n1):(N n2):stack, state) = run (code, if n1 == n2 then B TT:stack else B FF:stack, state)
-- run (Equ:code, (B b1):(B b2):stack, state) = run (code, if b1 == b2 then B TT:stack else B FF:stack, state)
run (Le:code, (N n1):(N n2):stack, state) = run (code, if n1 <= n2 then B TT:stack else B FF:stack, state)
run (And:code, (B b1):(B b2):stack, state) = run (code, if b1 == b2 && b1 == TT then B TT:stack else B FF:stack, state)
run (Neg:code, (B b):stack, state) = run (code, if b == TT then B FF:stack else B TT:stack, state)
-- run (Fetch var:code, stack, state) = case lookupValue var state of
run (Fetch var:code, stack, state) = case lookup var state of
    Just value -> run (code, value:stack, state)
    Nothing    -> error "Variable not found" 
run (Store var:code, value:stack, state) = run (code, stack, updateState var value state)
run (Noop:code, stack, state) = run (code, stack, state)
run (Branch code1 code2:code, (B b):stack, state) = run (if b == TT then code1 ++ code else code2 ++ code, stack, state)
run (Loop code1 code2:code, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]], stack, state)
--in case of input error: Running a wrong configuration should return an error message. For example running the program [ Push 1, Push 2, And ] should raise an exception with the string: ”Run-time error” (calling function error ”Run-time error”)
run (_, _, _) = error "Run-time error"


