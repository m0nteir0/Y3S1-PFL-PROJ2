module Main where
import Lexer
import Parser 
import Compiler
import Assembler


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
    putStrLn "========= Haskell Project - COMPILER =========\n\nPFL 23/24 \nGroup T06_G02:\n - Guilherme Monteiro | 202108668\n - Sofia Sá | 202108676\n"
    putStrLn "\nEnter program code:"
    programCode <- getLine
    let toks = Lexer.lexer programCode
    putStrLn ("\nLexed: \n" ++ show toks)
    let exps = Parser.parse programCode
    putStrLn ("\nParsed: \n" ++ show exps)
    let insts = Compiler.compile exps
    putStrLn ("\nCompiled: \n" ++ show insts)
    let machine = (insts, createEmptyStack, createEmptyState)
    putStrLn "\n======= Result ======="
    let (code, stack, state) =  Assembler.run machine
    putStrLn ("Stack: " ++ if null stack then "empty" else stack2Str stack)
    putStrLn ("State: " ++ state2Str state)
    putStrLn "\n\n"
    