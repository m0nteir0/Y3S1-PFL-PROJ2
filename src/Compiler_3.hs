-- =================== COMPILER ===========================

-- - !TODO: Add FETCH Expression --> figure out how to use it

module Compiler_3 where

import Assembler_4
import Parser_2


-- Temp --> nned to learn how to import
-- data Inst =
--   Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
--   Branch Code Code | Loop Code Code
--   deriving Show
-- type Code = [Inst]

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

runTests = do 
    -- Test STORE with arithmetic expressions
    print $ compile [STORE "x" (Left (ADD (NUM 2) (MULT (NUM 3) (NUM 4))))]                                                                     -- == [Push 4, Push 3, Mult, Push 2, Add, Store "x"]
    print $ compile [STORE "y" (Left (SUB (MULT (NUM 7) (NUM 8)) (NUM 9)))]                                                                     -- == [Push 9, Push 8, Push 7, Mult, Sub, Store "y"]
    -- Test STORE with boolean expressions
    print $ compile [STORE "b1" (Right (BOOL True))]                                                                         --  == [Fals, Tru, And, Store "b1"]
    print $ compile [STORE "b1" (Right (AND (BOOL True) (BOOL False)))]                                                                         --  == [Fals, Tru, And, Store "b1"]
    -- Test IF statement
    print $ compile [IF (AND (BOOL True) (BOOL False)) (STORE "x" (Left (NUM 1))) (STORE "x" (Left (NUM 2)))]                                   --  == [Fals, Tru, And, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    -- Test IF with arithmetic expressions
    print $ compile [IF (EQa (NUM 1) (NUM 1)) (STORE "x" (Left (NUM 1))) (STORE "x" (Left (NUM 2)))]                                            -- == [Push 1, Push 1, Equ, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    print $ compile [IF (EQb (BOOL True) (BOOL False)) (STORE "x" (Left (NUM 1))) (STORE "x" (Left (NUM 2)))]                                   -- == [Fals, Tru, Equ, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    print $ compile [IF (LE (NUM 1) (NUM 2)) (STORE "x" (Left (NUM 3))) (STORE "x" (Left (NUM 4)))]                                             -- == [Push 2, Push 1, Le, Branch [Push 3, Store "x"] [Push 4, Store "x"]]
    -- Test IF with boolean expressions
    print $ compile [IF (AND (BOOL True) (BOOL False)) (STORE "x" (Left (NUM 1))) (STORE "x" (Left (NUM 2)))]                                   -- == [Fals, Tru, And, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    -- Test IF with nested IF
    print $ compile [IF (NOT (BOOL False)) (IF (BOOL True) (STORE "x" (Left (NUM 1))) (STORE "x" (Left (NUM 2)))) (STORE "x" (Left (NUM 3)))]   -- == [Fals, Neg, Branch [Tru, Branch [Push 1, Store "x"] [Push 2, Store "x"]] [Push 3, Store "x"]]
    -- Test WHILE statement
    print $ compile [WHILE (NOT (BOOL False)) (STORE "x" (Left (ADD (NUM 1) (NUM 2))))]             -- == [Loop [Fals, Neg] [Push 2, Push 1, Add, Store "x"]]
    print $ compile [WHILE (EQa (NUM 1) (NUM 1)) (STORE "x" (Left (ADD (NUM 1) (NUM 2))))]          -- == [Loop [Push 1, Push 1, Equ] [Push 2, Push 1, Add, Store "x"]]
    print "--------------------------"
    -- Arithmetic expressions
    print $ compile [AExp (ADD (NUM 1) (NUM 2))]                                                   -- == [Push 2, Push 1, Add]
    print $ compile [AExp (SUB (NUM 1) (NUM 2))]                                                   -- == [Push 2, Push 1, Sub]
    print $ compile [AExp (MULT (NUM 1) (NUM 2))]                                                  -- == [Push 2, Push 1, Mult]
    -- more complex arithmetic expressions
    print $ compile [AExp (ADD (NUM 1) (MULT (NUM 2) (NUM 3)))]                                    -- == [Push 3, Push 2, Mult, Push 1, Add]
    print $ compile [AExp (SUB (NUM 1) (MULT (NUM 2) (NUM 3)))]                                    -- == [Push 3, Push 2, Mult, Push 1, Sub]
    print $ compile [AExp (MULT (NUM 1) (MULT (NUM 2) (NUM 3)))]                                   -- == [Push 3, Push 2, Mult, Push 1, Mult]
    -- even more complex arithmetic expressions
    print $ compile [AExp (ADD (NUM 1) (MULT (NUM 2) (SUB (NUM 3) (NUM 4))))]                      -- == [Push 4, Push 3, Sub, Push 2, Mult, Push 1, Add]
    print "--------------------------"
    -- Boolean expressions
    print $ compile [BExp (AND (BOOL True) (BOOL False))]                                           -- == [Fals, Tru, And]
    print $ compile [BExp (NOT (BOOL False))]                                                       -- == [Fals, Neg]
    print $ compile [BExp (EQa (NUM 1) (NUM 1))]                                                    -- == [Push 1, Push 1, Equ]
    print $ compile [BExp (EQb (BOOL True) (BOOL False))]                                           -- == [Fals, Tru, Equ]
    print $ compile [BExp (LE (NUM 1) (NUM 2))]                                                     -- == [Push 2, Push 1, Le]
    -- more complex boolean expressions
    print $ compile [BExp (AND (BOOL True) (EQa (NUM 1) (NUM 1)))]                                  -- == [Push 1, Push 1, Equ, Tru, And]
    print $ compile [BExp (AND (BOOL True) (EQb (BOOL True) (BOOL False)))]                          -- == [Fals, Tru, Equ, Tru, And]
    print $ compile [BExp (AND (BOOL True) (LE (NUM 1) (NUM 2)))]                                    -- == [Push 2, Push 1, Le, Tru, And]
    print $ compile [BExp (NOT (AND (BOOL True) (BOOL False)))]                                      -- == [Fals, Tru, And, Neg]

