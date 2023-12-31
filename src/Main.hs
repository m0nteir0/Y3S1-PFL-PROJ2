module Main where

-- import Lexer
import Parser_2
import Compiler_3
import Assembler_4

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    -- runTestsP
    -- print $ runTestsP "x = 2 * (3 + 4)"
    -- print $ Assembler_4.run(compile(parse ))
    