module Main where

-- import Lexer
import Parser_2
import Compiler_3
import Assembler_4


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
    putStrLn "========= Project 2: COMPILER =========\n\nPFL 23/24 \nGroup:\n - Guilherme Monteiro | 202108668\n - Sofia Sá | 202108676\n"
    putStrLn "\nEnter program code:"
    programCode <- getLine
    let exps = Parser_2.parse programCode
    putStrLn ("\nParsed: \n" ++ show exps)
    let insts = Compiler_3.compile exps
    putStrLn ("\nCompiled: \n" ++ show insts)
    let machine = (insts, createEmptyStack, createEmptyState)
    putStrLn "\n======= Result ======="
    let (code, stack, state) =  Assembler_4.run machine
    putStrLn ("Stack: " ++ if null stack then "empty" else stack2Str stack)
    putStrLn ("State: " ++ state2Str state)
    putStrLn "\n\n"
    