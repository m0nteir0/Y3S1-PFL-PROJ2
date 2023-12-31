import Lexer_1
import Parser_2
import Compiler_3
import Assembler_4

-- INDIVIDUAL TESTS


-- ============================================ LEXER ============================================
runLexerTests :: IO()
runLexerTests = do                                            
    print $ lexer "True"
    print $ lexer "False"
    print $ lexer "True and False"
    print $ lexer "123 + 1"
    print $ lexer "1 + 2 * 3"
    print $ lexer "x := 5; x := x - 1;"
    print $ lexer "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2"




-- ============================================ COMPILER ============================================
runCompilerTests = do 
    print $ compile [AExp (SUB (SUB (ADD (NUM 1) (MULT (NUM 2) (NUM 4))) (MULT (NUM 2) (NUM 6))) (NUM 4))]                      -- == [Push 4, Push 3, Sub, Push 2, Mult, Push 1, Add]
    -- Test STORE with arithmetic expressions
    print $ compile [STORE "x" (ADD (NUM 2) (MULT (NUM 3) (NUM 4)))]                                                                     -- == [Push 4, Push 3, Mult, Push 2, Add, Store "x"]
    print $ compile [STORE "y" (SUB (MULT (NUM 7) (NUM 8)) (NUM 9))]                                                                     -- == [Push 9, Push 8, Push 7, Mult, Sub, Store "y"]
    -- Test STORE with boolean expressions
    -- print $ compile [STORE "b1" (Right (BOOL True))]                                                                         --  == [Fals, Tru, And, Store "b1"]
    -- print $ compile [STORE "b1" (Right (AND (BOOL True) (BOOL False)))]                                                                         --  == [Fals, Tru, And, Store "b1"]
    -- Test IF statement
    print $ compile [IF (AND (BOOL True) (BOOL False)) (STORE "x" (NUM 1)) (STORE "x" (NUM 2))]                                   --  == [Fals, Tru, And, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    -- Test IF with arithmetic expressions
    print $ compile [IF (EQa (NUM 1) (NUM 1)) (STORE "x" (NUM 1)) (STORE "x" (NUM 2))]                                            -- == [Push 1, Push 1, Equ, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    print $ compile [IF (EQb (BOOL True) (BOOL False)) (STORE "x" (NUM 1)) (STORE "x"  (NUM 2))]                                   -- == [Fals, Tru, Equ, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    print $ compile [IF (LE (NUM 1) (NUM 2)) (STORE "x" (NUM 3)) (STORE "x"  (NUM 4))]                                             -- == [Push 2, Push 1, Le, Branch [Push 3, Store "x"] [Push 4, Store "x"]]
    -- Test IF with boolean expressions
    print $ compile [IF (AND (BOOL True) (BOOL False)) (STORE "x" (NUM 1)) (STORE "x" (NUM 2))]                                   -- == [Fals, Tru, And, Branch [Push 1, Store "x"] [Push 2, Store "x"]]
    -- Test IF with nested IF
    print $ compile [IF (NOT (BOOL False)) (IF (BOOL True) (STORE "x" (NUM 1)) (STORE "x" (NUM 2))) (STORE "x" (NUM 3))]   -- == [Fals, Neg, Branch [Tru, Branch [Push 1, Store "x"] [Push 2, Store "x"]] [Push 3, Store "x"]]
    -- Test WHILE statement
    print $ compile [WHILE (NOT (BOOL False)) (STORE "x" (ADD (NUM 1) (NUM 2)))]             -- == [Loop [Fals, Neg] [Push 2, Push 1, Add, Store "x"]]
    print $ compile [WHILE (EQa (NUM 1) (NUM 1)) (STORE "x" (ADD (NUM 1) (NUM 2)))]          -- == [Loop [Push 1, Push 1, Equ] [Push 2, Push 1, Add, Store "x"]]
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






-- ============================================ ASSEMBLER ============================================
    
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