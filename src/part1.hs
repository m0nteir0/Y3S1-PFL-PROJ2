--  Define a new type to represent the machine’s stack. The type must be named Stack.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

data Stack a = Stack [a]

-- Define a new type to represent the machine’s state. The type must be named State.

data State = State [(String, Int)]

-- Implement the createEmptyStack function which returns an empty machine’s stack.
createEmptyStack :: Stack a
createEmptyStack = Stack []

-- Implement the createEmptyState function which returns an empty machine’s state.
createEmptyState :: State
createEmptyState = State []

-- Implement the stack2Str function which converts a stack given as input to a string.
-- The string represents the stack as an ordered list of values, separated by commas
-- and without spaces, with the leftmost value representing the top of the stack.
-- For instance, after executing the code [push−42, true, false], the string representing
-- the stack is: False,True,42.

stack2Str :: Show a => Stack a -> String
stack2Str (Stack []) = ""
stack2Str (Stack (x:xs)) = show x ++ "," ++ stack2Str (Stack xs)

-- Implement the state2Str function which converts a machine state given as input to
-- a string. The string represents the state as an list of pairs variable-value, separated
-- by commas and without spaces, with the pairs ordered in alphabetical order of the
-- variable name. Each variable-value pair is represented without spaces and using an
-- ”=”.
-- For instance, after executing the code [false, push − 3, true, store − var, store −
-- a, store−someV ar], the string representing the state is: a=3,someVar=False,var=True

state2Str :: State -> String
state2Str (State _) = ""
state2Str (State (x:xs)) = show x ++ "," ++ state2Str (State xs)

-- Write an interpreter for programs in the same machine, which given a list of instructions (type defined as Code, i.e. type Code = [Inst]), a stack (type defined as
-- Stack) and that is initially empty, and a storage (type defined as State), runs the
-- list of instructions returning as ouput an empty code list, a stack and the output
-- values in the storage. This evaluation function must be declared as:

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


-- EACH INSTRUCTION:
-- 1) add, mult and sub add, subtract and multiply the top two integer values of the
-- stack, respectively, and push the result onto the top of the stack. In particular,
-- sub subtracts the topmost element of the stack with the second topmost element.
-- 2) eq and le compare the top two values of the stack for equality and inequality,
-- respectively, and push a boolean with the comparison result onto the top of the
-- stack. eq can compare both integers and booleans, while le only works for integers.
-- In particular, le determines whether the topmost stack element is less or equal to
-- the second topmost element.
-- 3) push-n pushes a constant value n onto the stack; true and false push the constants
-- tt and ff, respectively, onto the stack.
-- 4) fetch-x pushes the value bound to x onto the stack, whereas store-x pops the
-- topmost element of the stack and updates the storage so that the popped value is
-- bound to x.
-- 5) branch(c1, c2) will also change the flow of control: if the top of the stack is the
-- value tt (that is, some boolean expression has been evaluated to true), then the
-- stack is popped and c1 is to be executed next. Otherwise, if the top element of the
-- stack is ff, then it will be popped and c2 will be executed next.
-- 6) noop is a dummy instruction that returns the input stack and store

run :: (Code, Stack a, State) -> (Code, Stack a, State)
run ([], stack, state) = ([], stack, state)
-- run ((Push n):code, Stack stack, state) = run (code, Stack (n:stack), state)
-- run (Add:code, Stack (x:y:stack), state) = run (code, Stack ((x + y):stack), state)
-- run (Sub:code, Stack (x:y:stack), state) = run (code, Stack ((y - x):stack), state)
-- run (Mult:code, Stack (x:y:stack), state) = run (code, Stack ((x * y):stack), state)
-- run (Fetch str:code, stack, State state) = run (code, Stack ((lookup str state):stack), State state)
-- run (Store str:code, Stack (x:stack), State state) = run (code, Stack stack, State ((str, x):state))
run (Noop:code, stack, state) = run (code, stack, state)
-- run (Branch c1 c2:code, Stack (x:stack), state) = if x == 1 then run (c1 ++ code, Stack stack, state) else run (c2 ++ code, Stack stack, state)

-- define Code
-- The instructions of the machine are: push-n, add, mult, sub, true, false, eq, le, and, neg, fetch-x, store-x, noop, branch(c1, c2) and loop(c1, c2).
-- The instructions are defined as a new type Inst, and the code is defined as a list of instructions, i.e. type Code = [Inst].

-- The instructions of the machine are: fetch-x, store-x, noop, branch(c1, c2) and loop(c1, c2).



-- PART 2)
-- Now consider a small imperative programming language with arithmetic and boolean
-- expressions, and statements consisting of assignments of the form x := a, sequence of
-- statements (instr1 ; instr2), if then else statements, and while loops.
-- One can now define a translation (a compiler) from this language into lists of instructions
-- in the previous machine.
-- • Arithmetic and boolean expressions will be evaluated on the evaluation stack of the
-- machine and the code to be generated must effect this. Thus, the code generated
-- for binary expressions consists of the code for the right argument followed by that
-- for the left argument and, finally, the appropriate instruction for the operator. In
-- this way, it is ensured that the arguments appear on the evaluation stack in the
-- order required by the instructions.
-- Example 3 The compilation of x + 1 is [push − 1, fetch − x, add].
-- The code generated for an arithmetic expression must ensure that the value of
-- the expression is on top of the evaluation stack when it has been computed.
-- • The code generated for x := a appends the code for the arithmetic expression a
-- with the instruction store−x. This instruction assigns x the appropriate value and
-- additionally pops the stack.
-- • For a sequence of two statements, we just concatenate the two instruction sequences.
-- • When generating code for the conditional, the code for the boolean expression
-- will ensure that a truth value will be placed on top of the evaluation stack, and the
-- branch instruction will then inspect (and pop) that value and select the appropriate
-- piece of code.
-- • Finally, the code for the while statement uses the loop-instruction.
-- Example 4 The compilation of the factorial program
-- y := 1; while ¬(x = 1) do (y := y ∗ x; x := x − 1)
-- outputs the code:
-- [push-1,store-y,loop([push-1,fetch-x,eq,neg],
-- [fetch-x,fetch-y,mult,store-y,push-1,fetch-x,sub,store-x)])]
-- (a) Define three datas in Haskell to represent expressions and statements of this imperative language:
-- • Aexp for arithmetic expressions
-- • Bexp for boolean expressions
-- • Stm for statements

-- data Aexp = Num Int | Var String | Add Aexp Aexp | Sub Aexp Aexp | Mult Aexp Aexp | Div Aexp Aexp deriving (Show, Eq) -- Aexp = arithmetic expression (e.g. 1 + 2) 
-- data Bexp = BTrue | BFalse | Eq Aexp Aexp | Le Aexp Aexp | Neg Bexp | And Bexp Bexp deriving (Show, Eq) -- Bexp = boolean expression (e.g. 1 == 2)
-- data Stm = Assign String Aexp | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm deriving (Show, Eq) -- Stm = statement (e.g. x := 1)
 
--  (b) Define a compiler from a program in this small imperative language into a list of
-- machine instructions (as defined in part 1). The main compiler is function:
-- • compile :: Stm → Code
-- that must use the two mandatory auxiliary functions which compile arithmetic and
-- boolean expressions, respectively:
-- • compA :: Aexp → Code
-- • compB :: Bexp → Code
