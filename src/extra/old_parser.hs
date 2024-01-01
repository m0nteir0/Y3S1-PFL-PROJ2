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
    | EQb Bexp Bexp -- equal node 
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


parseIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrPar (IntTok n : restTokens)
  = Just (NUM (fromIntegral n), restTokens)
parseIntOrPar (OpenTok : restTokens1)
  = case parseAexp restTokens1 of
  -- = case parseSumOrSubOrProdOrIntOrPar restTokens1 of
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseIntOrPar tokens = Nothing


-- Parsing products or parenthesised expressions
parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens
  = case parseIntOrPar tokens of
    Just (expr1, MultTok : restTokens1) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MULT expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parsing sums or products or parenthesised expressions
parseSumOrSubOrProdOrIntOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrIntOrPar tokens
  = case parseProdOrIntOrPar tokens of
    Just (expr1, AddTok : restTokens1) ->
      case parseSumOrSubOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ADD expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, SubTok : restTokens1) ->
      case parseSumOrSubOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (SUB expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- top-level
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
  case parseSumOrSubOrProdOrIntOrPar tokens of
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing  -- !WARNING: may need a result -> result here



parseBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrPar (TruTok : restTokens) = Just (BOOL True, restTokens)
parseBoolOrPar (FalsTok : restTokens) = Just (BOOL False, restTokens)
parseBoolOrPar (OpenTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar restTokens1 of -- !TODO: change function name
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing


parseEqAOrAexpOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqAOrAexpOrLTOrBoolOrPar tokens =
  case parseAexp tokens of --see if there is an integer expression
    Just (expr1, LeTok : restTokens1) -> --if there is, check if there is a less than or equal operator next
      case parseAexp restTokens1 of -- if there is, check if there is another integer expression
        Just (expr2, restTokens2) -> Just (LE expr1 expr2, restTokens2) -- if there is, return the expression
        Nothing -> Nothing
    -- Just result -> Just result 
    Just (expr1, EqITok : restTokens1) -> --if there is, check if there is a less than or equal operator next
      case parseAexp restTokens1 of -- if there is, check if there is another integer expression
        Just (expr2, restTokens2) -> Just (EQa expr1 expr2, restTokens2) -- if there is, return the expression
        Nothing -> Nothing
    Nothing -> parseBoolOrPar tokens -- if there is no integer expression, check if there is a boolean expression
  -- case parseBoolOrPar tokens -- será que é necessario?


parseNotOrEqAOrAexpOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrEqAOrAexpOrLTOrBoolOrPar tokens =
  case parseEqAOrAexpOrLTOrBoolOrPar tokens of
    Just (expr1, NotTok : restTokens1) -> Just (NOT expr1, restTokens1)      
    result -> result

parseEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar tokens =
  case parseNotOrEqAOrAexpOrLTOrBoolOrPar tokens of
    Just (expr1, EqBTok : restTokens1) ->
      case parseEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (EQb expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseAndOrEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar tokens =
  case parseEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar tokens of
    Just (expr1, AndTok : restTokens1) ->
      case parseAndOrEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AND expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just result -> Just result
    Nothing -> Nothing

parseBexpOrAexp :: [Token] -> Maybe (Bexp, [Token])
parseBexpOrAexp tokens =
  case parseAndOrEqBOrNotOrEqAOrAexpOrLTOrBoolOrPar tokens of -- !TODO: change function name  
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing