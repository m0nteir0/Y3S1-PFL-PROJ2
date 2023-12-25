{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Stack (Stack, -- exportar o tipo
push, pop, top, -- e as operações
empty, isEmpty) where
data Stack a = Stk [a] -- implementação usando listas


-- push to stack
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

-- pop from stack
pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

-- retorna o topo da stack
top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"


-- cria uma stack vazia
empty :: Stack a
empty = Stk []


-- verifica se a stack está vazia
isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False