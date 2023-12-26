-- -- 2. Parser
-- -- The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct.
module Parser_2 where
  
import Lexer_1

runTestsP = do
  print $ lexer "x = 2 * (3 + 4)"

        
data Aexp --arithmetic expressions
    = NUM Integer -- integer constants
    | ADD Aexp Aexp -- addition node
    | MULT Aexp Aexp -- multiplication node
    | SUB Aexp Aexp -- subtraction node
    deriving Show

data Bexp --boolean expressions
    = BOOL Bool -- boolean constants
    | AND Bexp Bexp -- and node
    -- | Or Bexp Bexp -- or node
    | NOT Bexp -- not node
    | EQa Aexp Aexp -- equal node --
    | EQb Bexp Bexp -- equal node --
    | LE Aexp Aexp -- less than or equal node
    deriving Show

-- type Expr = Either Aexp Bexp

data Stm --statements --> TUDO
    = STORE String  (Either Aexp Bexp)-- store node
    | FETCH String
    | IF Bexp Stm Stm -- if node
    | WHILE Bexp Stm -- loop node
    | AExp Aexp
    | BExp Bexp
    deriving Show

------------------------------------------------
-- linguagem
-- expressões
-- asssembly

-- x = 2 * (3 + 4)
-- Store "x" ( Mult (Num 2) (Add (Num 3) (Num 4)) )
-- [Push 4, Push 3, Add, Push 2, Mult, Store "x"]

-- y = 8 * 2 + 3 * 4 - 1
-- Store "y" (Sub (Add (Mult (Num 8) (Num 2)) (Mult (Num 3) (Num 4))) (Num 1))
-- [Push 1, Push 4, Push 3, Mult, Push 2, Push 8, Mult, Add, Sub, Store "y"]

-- [ ]> topo --> y=27
-- [("y",N 27)]

------------------------------------------------



parseInt :: [Token] -> Maybe (Stm, [Token])
parseInt (IntTok n : restTokens)
    = Just (n, restTokens)
parseInt tokens
    = Nothing


parseProdOrInt :: [Token] -> Maybe (Stm, [Token])
parseProdOrInt tokens
    = case parseInt tokens of
        Just (expr1, (MultTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (MULT expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- can be ’Nothing’ or valid

parseSumOrProdOrInt :: [Token] -> Maybe (Stm, [Token])
parseSumOrProdOrInt tokens
    = case parseProdOrInt tokens of
        Just (expr1, (AddTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (Add expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- could be ’Nothing’ or valid

parseIntOrParenExpr :: [Token] -> Maybe (Stm, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
    = Just (IntTok n, restTokens)
parseIntOrParenExpr (OpenTok : restTokens1)
    = case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr, (CloseTok : restTokens2)) ->
                Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing


parseProdOrIntOrPar :: [Token] -> Maybe (Stm, [Token])
parseProdOrIntOrPar tokens
    = case parseIntOrParenExpr tokens of
        Just (expr1, (MultTok : restTokens1)) ->
            case parseProdOrIntOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (Mult expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSumOrProdOrIntOrPar::[Token] -> Maybe (Stm, [Token])
parseSumOrProdOrIntOrPar tokens
    = case parseProdOrIntOrPar tokens of
        Just (expr1, (AddTok : restTokens1)) ->
            case parseSumOrProdOrIntOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                        Just (ADD expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result


parse :: [Token] -> Stm
parse tokens =
    case parseSumOrProdOrIntOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Parse error"


        -- 


