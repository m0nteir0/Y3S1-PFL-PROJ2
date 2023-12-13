--  Define a new type to represent the machine’s stack. The type must be named Stack.

data Stack = Stack [Int]


-- Define a new type to represent the machine’s state. The type must be named State.

data State = State

-- Implement the createEmptyStack function which returns an empty machine’s stack.
createEmptyStack :: Stack
createEmptyStack = Stack []

-- Implement the createEmptyState function which returns an empty machine’s state.
createEmptyState :: State
createEmptyState = State

-- Implement the stack2Str function which converts a stack given as input to a string.
-- The string represents the stack as an ordered list of values, separated by commas
-- and without spaces, with the leftmost value representing the top of the stack.
stack2Str :: Stack -> String
stack2Str (Stack []) = ""
stack2Str (Stack (x:xs)) = show x ++ "," ++ stack2Str (Stack xs)

-- For instance, after executing the code [push−42, true, f alse], the string representing
-- the stack is: False,True,42.


-- Implement the state2Str function which converts a machine state given as input to
-- a string. The string represents the state as an list of pairs variable-value, separated
-- by commas and without spaces, with the pairs ordered in alphabetical order of the
-- variable name. Each variable-value pair is represented without spaces and using an
-- ”=”.
state2Str :: State -> String
state2Str State = ""


-- For instance, after executing the code [f alse, push − 3, true, store − var, store −
-- a, store−someV ar], the string representing the state is: a=3,someVar=False,var=True

