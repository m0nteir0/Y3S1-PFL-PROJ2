-- 2. Parser
-- The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct.


-- data Inst =
--   Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
--   Branch Code Code | Loop Code Code
--   deriving Show
-- type Code = [Inst]
data Token
    = PushTok
    | AddTok
    | MultTok
    | SubTok
    | TruTok
    | FalsTok
    | EquTok
    | LeTok
    | AndTok
    | NegTok
    | FetchTok
    | StoreTok
    | NoopTok
    | BranchTok
    | LoopTok
    | VarTok String
    | IntTok Int
    deriving (Show)
        

data Expr
    = Num Int -- integer constants
    | Add Expr Expr -- addition node
    | Mult Expr Expr -- multiplication node
    | Sub Expr Expr -- subtraction node
    -- | Div Expr Expr -- division node
    | Neg Expr -- negation node
    | Boolean Bool -- boolean constants
    | And Expr Expr -- and node
    | Or Expr Expr -- or node
    | Not Expr -- not node
    | Eq Expr Expr -- equal node
    | Le Expr Expr -- less than or equal node
    -- | Var String -- variable node
    -- | Let String Expr Expr -- let node
    | Store String Expr -- store node
    | If Expr Expr Expr -- if node
    | Loop Expr Expr -- loop node


-- parser :: [Token] -> Expr

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



parseInt :: [Token] -> Maybe (Expr, [Token])
parseInt (IntTok n : restTokens)
    = Just (IntLit n, restTokens)
parseInt tokens
    = Nothing


parseProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseProdOrInt tokens
    = case parseInt tokens of
        Just (expr1, (MultTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (Mult expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- can be ’Nothing’ or valid

parseSumOrProdOrInt :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrInt tokens
    = case parseProdOrInt tokens of
        Just (expr1, (AddTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (Add expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- could be ’Nothing’ or valid

parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
    = Just (IntLit n, restTokens)
parseIntOrParenExpr (OpenP : restTokens1)
    = case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr, (CloseP : restTokens2)) ->
                Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing


parseProdOrIntOrPar :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrPar tokens
    = case parseIntOrParenExpr tokens of
        Just (expr1, (MultTok : restTokens1)) ->
            case parseProdOrIntOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (Mult expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSumOrProdOrIntOrPar::[Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrPar tokens
    = case parseProdOrIntOrPar tokens of
        Just (expr1, (AddTok : restTokens1)) ->
            case parseSumOrProdOrIntOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                        Just (Add expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result


parse :: [Token] -> Expr
parse tokens =
    case parseSumOrProdOrIntOrPar tokens of
        Just (expr, []) -> expr
        _ -> error "Parse error"


