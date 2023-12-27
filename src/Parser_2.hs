-- -- 2. Parser

-- -- The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct.
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

module Parser_2 where
  
import Lexer_1

-- - !TODO: Add FETCH Expression --> figure out how to use it

runTestsP = do
  print $ lexer "x := 2 * (3 + 4)"

        
-- parsed information will have instructions of this kind of structure:
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




parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntTok n : restTokens) = Just (NUM (fromIntegral n), restTokens)
parseInt tokens = Nothing

parseTerm :: [Token] -> Maybe (Aexp, [Token])
parseTerm tokens =
  case parseInt tokens of
    Just (expr1, MultTok : restTokens1) ->
      case parseTerm restTokens1 of
        Just (expr2, restTokens2) -> Just (MULT expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
  case parseTerm tokens of
    Just (expr1, AddTok : restTokens1) ->
      case parseAexp restTokens1 of
        Just (expr2, restTokens2) -> Just (ADD expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, SubTok : restTokens1) ->
      case parseAexp restTokens1 of
        Just (expr2, restTokens2) -> Just (SUB expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens =
  case parseAexp tokens of
    Just (expr1, EqITok : restTokens1) ->
      case parseAexp restTokens1 of
        Just (expr2, restTokens2) -> Just (EQa expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseFactor :: [Token] -> Maybe (Bexp, [Token])
parseFactor (TruTok : restTokens) = Just (BOOL True, restTokens)
parseFactor (FalsTok : restTokens) = Just (BOOL False, restTokens)
parseFactor (NotTok : restTokens) =
  case parseFactor restTokens of
    Just (expr, restTokens') -> Just (NOT expr, restTokens')
    Nothing -> Nothing
parseFactor tokens =
  case parseBexp tokens of
    Just (expr1, (AndTok : restTokens1)) ->
      case parseFactor restTokens1 of
        Just (expr2, restTokens2) -> Just (AND expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (IntTok n : restTokens) = Just (NUM n, restTokens)
parseIntOrParenExpr (OpenTok : restTokens1) =
  case parseAexp restTokens1 of
    Just (expr, (CloseTok : restTokens2)) -> Just (expr, restTokens2)
    _ -> Nothing
parseIntOrParenExpr tokens = Nothing

parseTermOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseTermOrParenExpr tokens =
  case parseIntOrParenExpr tokens of
    Just (expr1, (MultTok : restTokens1)) ->
      case parseTermOrParenExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (MULT expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseAexpOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseAexpOrParenExpr tokens =
  case parseTermOrParenExpr tokens of
    Just (expr1, (AddTok : restTokens1)) ->
      case parseAexpOrParenExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (ADD expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (SubTok : restTokens1)) ->
      case parseAexpOrParenExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (SUB expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseVar :: [Token] -> Maybe (String, [Token])
parseVar (VarTok v : restTokens) = Just (v, restTokens)
parseVar _ = Nothing

parseAssignment :: [Token] -> Maybe (Stm, [Token])
parseAssignment tokens =
  case parseVar tokens of
    Just (var, (AttTok : restTokens1)) ->
      case parseAexp restTokens1 of
        Just (expr, restTokens2) -> Just (VARASSIGN var expr, restTokens2)
        Nothing -> Nothing
    result -> result

parseIfThenElse :: [Token] -> Maybe (Stm, [Token])
parseIfThenElse tokens =
  case parseBexp tokens of
    Just (cond, (ThenTok : restTokens1)) ->
      case parse restTokens1 of
        stmThen@(AExp _ : restTokens2) ->
          case restTokens2 of
            (ElseTok : restTokens3) ->
              case parse restTokens3 of
                stmElse@(AExp _ : restTokens4) ->
                  Just (IFTHENELSE cond stmThen stmElse, restTokens4)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    result -> result

parseWhileDo :: [Token] -> Maybe (Stm, [Token])
parseWhileDo tokens =
  case parseBexp tokens of
    Just (cond, (DoTok : restTokens1)) ->
      case parse restTokens1 of
        stm@(AExp _ : restTokens2) -> Just (WHILEDO cond stm, restTokens2)
        _ -> Nothing
    result -> result

parse :: [Token] -> Stm
parse tokens =
  case parseAexpOrParenExpr tokens of
    Just (expr, []) -> AExp expr
    _ ->
      case parseAssignment tokens of
        Just (stmt, []) -> stmt
        _ ->
          case parseIfThenElse tokens of
            Just (stmt, []) -> stmt
            _ ->
              case parseWhileDo tokens of
                Just (stmt, []) -> stmt
                _ -> error "Parse error"
                
