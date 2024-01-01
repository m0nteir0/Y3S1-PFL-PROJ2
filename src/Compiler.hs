-- =================== COMPILER ===========================

module Compiler where

import Assembler
import Parser

-- compA: Compiles an arithmetic expression (Aexp) into the corresponding list of instructions (Code). 
-- It handles numbers, addition, multiplication, subtraction, and numeric variables.
compA :: Aexp -> Code
compA (NUM n) = [Push n]
compA (ADD e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (MULT e1 e2) = compA e2 ++ compA e1 ++ [Mult]
compA (SUB e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (VAR s) = [Fetch s]


-- compB: Compiles a boolean expression (Bexp) into the corresponding list of instructions (Code). 
-- It handles boolean values, AND, NOT, equality of arithmetic expressions, equality of boolean expressions, and less than or equal comparisons for arithmetic expressions.
compB :: Bexp -> Code
compB (BOOL b) = if b then [Tru] else [Fals]
compB (AND e1 e2) = compB e2 ++ compB e1 ++ [And]
compB (NOT e) = compB e ++ [Neg]
compB (EQa e1 e2) = compA e2 ++ compA e1 ++ [Equ]
compB (EQb e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (LE e1 e2) = compA e2 ++ compA e1 ++ [Le]


-- compile: Compiles a list of statements into the corresponding list of instructions (Code). 
-- It handles storing of values, IF statements, WHILE loops, and arithmetic and boolean expressions.
compile :: [Stm] -> Code
compile [] = []
compile ((STORE s expr):xs) = compA expr ++ [Store s] ++ compile xs
compile ((IF bexp expr1 expr2):xs) = compB bexp ++ [Branch (compile expr1) (compile expr2)] ++ compile xs
compile ((WHILE bexp expr):xs) = Loop (compB bexp) (compile expr) : compile xs
compile ((AExp aexp):xs) = compA aexp ++ compile xs
compile ((BExp bexp):xs) = compB bexp ++ compile xs
