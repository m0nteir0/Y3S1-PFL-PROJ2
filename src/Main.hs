module Main where

-- import Lexer
import Parser_2
import Compiler_3
import Assembler_4

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    runTestsP
    -- print $ runTestsP "x = 2 * (3 + 4)"
    