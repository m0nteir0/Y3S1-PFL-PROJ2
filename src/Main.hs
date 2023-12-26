module Main where

-- import Lexer
import Parser
import Compiler
import Assembler

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    runTestsP
    -- print $ runTestsP "x = 2 * (3 + 4)"
    