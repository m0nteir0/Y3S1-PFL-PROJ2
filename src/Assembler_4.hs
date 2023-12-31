-- =================== ASSEMBLER ===========================
module Assembler_4 where

import Data.List (sortOn)

-- Inst: A data type representing the different instructions that can be executed by the stack machine.
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
-- Code: A type alias for a list of instructions.
type Code = [Inst]

-- ItemBool: A data type representing the stack's and state's boolean values.
data ItemBool = TT | FF deriving Show
instance Eq ItemBool where
    (==) :: ItemBool -> ItemBool -> Bool
    TT == TT = True
    FF == FF = True
    _  == _  = False


-- StackItem: A data type representing the different types of items that can be stored on the stack.
data StackItem = N Integer | B ItemBool deriving Show
instance Eq StackItem where
    (==) :: StackItem -> StackItem -> Bool
    N n1 == N n2 = n1 == n2
    B b1 == B b2 = b1 == b2
    _    == _    = False


-- Stack: A type alias for a list of stack items.
type Stack =  [StackItem]


-- State: A type alias for a list of key-value pairs, representing the state of the stack machine.
type State = [(String, StackItem)]


-- createEmptyStack: A function that creates an empty stack.
createEmptyStack :: Stack
createEmptyStack = []

-- createEmptyState: A function that creates an empty state.
createEmptyState :: State
createEmptyState = []

-- stackItem2Str: A function that converts a stack item into a string.
stackItem2Str :: StackItem -> String
stackItem2Str (B TT) = "True"
stackItem2Str (B FF) = "False"
stackItem2Str (N n) = show n


-- stack2Str: A function that converts a stack into a string.
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = stackItem2Str x
stack2Str (x:xs) = stackItem2Str x ++ "," ++ stack2Str xs


-- state2Str: A function that converts a state into a string.
state2Str :: State -> String
state2Str [] = ""
state2Str [x] = fst x ++ "=" ++ stackItem2Str (snd x)
state2Str (x:xs) = fst x ++ "=" ++ stackItem2Str (snd x) ++ "," ++ state2Str xs


-- updateState: A function that updates the state with a new key-value pair, and sorts it alphabetically.
updateState :: String -> StackItem -> State -> State
updateState key value state = sortOn fst (updateStateUnsorted key value state)


-- updateStateUnsorted: A function that updates the state with a new key-value pair, or updates an already existing pair with the new value.
updateStateUnsorted :: String -> StackItem -> State -> State
updateStateUnsorted key value [] = [(key, value)] -- key was not found, add new pair
updateStateUnsorted key value ((k,v):xs)          -- iterate through the state to find if key already exists, and if so, updates it's value
    | key == k  = (key, value) : xs
    | otherwise = (k, v) : updateStateUnsorted key value xs

-- run: A function that executes the code on the machine.
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) = run (code, N n:stack, state)
run (Add:code, (N n1):(N n2):stack, state) = run (code, N (n1 + n2):stack, state)
run (Mult:code, (N n1):(N n2):stack, state) = run (code, N (n1 * n2):stack, state)
run (Sub:code, (N n1):(N n2):stack, state) = run (code, N (n1 - n2):stack, state)
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
