-- =================== PARSER ===========================
  
module Parser_2 where

import Lexer_1

-- Aexp: Data type that represents arithmetic expressions
data Aexp --arithmetic expressions
    = NUM Integer -- integer constants
    | ADD Aexp Aexp -- addition node
    | MULT Aexp Aexp -- multiplication node
    | SUB Aexp Aexp -- subtraction node
    | VAR String -- get the value of a variable
    deriving Show

-- Bexp: Data type that represents boolean expressions
data Bexp --boolean expressions
    = BOOL Bool -- boolean constants
    | AND Bexp Bexp -- and node
    | NOT Bexp -- not node
    | EQa Aexp Aexp -- equal node --
    | EQb Bexp Bexp -- equal node 
    | LE Aexp Aexp -- less than or equal node
    deriving Show

-- Stm: Data type that represents statements
data Stm --statements
    = STORE String Aexp-- store node
    | IF Bexp [Stm] [Stm] -- if node
    | WHILE Bexp [Stm] -- loop node
    | AExp Aexp
    | BExp Bexp
    deriving Show


-- parseIntOrPar: Parses an integer, variable, or parenthesized arithmetic expression from a list of tokens.
parseIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrPar (IntTok n : restTokens) = Just (NUM (fromIntegral n), restTokens) -- ^ Parses an integer constant
parseIntOrPar (VarTok s : restTokens) = Just (VAR s, restTokens) -- ^ Parses a variable
parseIntOrPar (OpenTok : restTokens1)
  = case parseAexp restTokens1 of
    Just (expr, CloseTok : restTokens2) -> -- ^ Parses a parenthesized expression
      Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrPar tokens = Nothing -- ^ Returns Nothing if the token list doesn't match any of the above patterns


-- parseProdOrIntOrPar: Parses a multiplication, integer, or parenthesized arithmetic expression from a list of tokens.
parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens = do
  (expr1, restTokens1) <- parseIntOrPar tokens -- ^ Parses an integer or parenthesized expression
  parseRest expr1 restTokens1
  where
    parseRest expr1 [] = Just (expr1, []) -- ^ If no more tokens, return the parsed expression
    parseRest expr1 (MultTok : restTokens1) = do -- ^ If a multiplication token is found, parse the next expression
      (expr2, restTokens2) <- parseIntOrPar restTokens1
      parseRest (MULT expr1 expr2) restTokens2 -- ^ Recursively parse the rest of the tokens
    parseRest expr1 restTokens1 = Just (expr1, restTokens1) -- ^ If no multiplication token, return the parsed expression


-- parseSumOrProdOrIntOrPar: Parses a sum, product, integer, or parenthesized arithmetic expression from a list of tokens.
parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens = do
  (expr1, restTokens1) <- parseProdOrIntOrPar tokens -- ^ Parses a product, integer, or parenthesized expression
  parseRest expr1 restTokens1
  where
    parseRest expr1 [] = Just (expr1, []) -- ^ If no more tokens, return the parsed expression
    parseRest expr1 (AddTok : restTokens1) = do -- ^ If an addition token is found, parse the next expression
      (expr2, restTokens2) <- parseProdOrIntOrPar restTokens1
      parseRest (ADD expr1 expr2) restTokens2 -- ^ Recursively parse the rest of the tokens
    parseRest expr1 (SubTok : restTokens1) = do -- ^ If a subtraction token is found, parse the next expression
      (expr2, restTokens2) <- parseProdOrIntOrPar restTokens1
      parseRest (SUB expr1 expr2) restTokens2 -- ^ Recursively parse the rest of the tokens
    parseRest expr1 restTokens1 = Just (expr1, restTokens1) -- ^ If no addition or subtraction token, return the parsed expression


-- parseAexp: Parses an arithmetic expression from a list of tokens.
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
  case parseSumOrProdOrIntOrPar tokens of
    Just (expr, restTokens) -> Just (expr, restTokens) -- ^ If a sum, product, integer, or parenthesized expression is parsed, return it
    _ -> Nothing -- ^ If parsing fails, return Nothing


-- parseBoolOrPar: Parses a boolean or parenthesized expression from a list of tokens.
parseBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseBoolOrPar (TruTok : restTokens) = Just (Right (BOOL True), restTokens)
parseBoolOrPar (FalsTok : restTokens) = Just (Right (BOOL False), restTokens)
parseBoolOrPar (OpenTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar restTokens1 of
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing 
    Nothing -> Nothing
parseBoolOrPar tokens = Nothing


-- parseEqAOrLTOrBoolOrPar: Parses an arithmetic expression, less than, equality, or boolean expression from a list of tokens.
parseEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseEqAOrLTOrBoolOrPar tokens =
  case parseAexp tokens of --see if there is an integer expression
    Just (expr1, LeTok : restTokens1) -> --if there is, check if there is a less than or equal operator next
      case parseAexp restTokens1 of -- if there is, check if there is another integer expression
        Just (expr2, restTokens2) -> Just (Right(LE expr1 expr2), restTokens2) -- if there is, return the expression
        Nothing -> Nothing
    Just (expr1, EqITok : restTokens1) -> --if there is, check if there is a less than or equal operator next
      case parseAexp restTokens1 of -- if there is, check if there is another integer expression
        Just (expr2, restTokens2) -> Just (Right (EQa expr1 expr2), restTokens2) -- if there is, return the expression
        Nothing -> Nothing
    Just (expr1, restTokens1) -> Just (Left expr1, restTokens1)-- means that it is exclusively an aexp, not part of a boolexp

    Nothing -> parseBoolOrPar tokens -- if there is no integer expression, check if there is a boolean expression


-- parseNotOrEqAOrLTOrBoolOrPar: Parses a NOT, arithmetic expression, less than, equality, or boolean expression from a list of tokens.
parseNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseNotOrEqAOrLTOrBoolOrPar (NotTok : restTokens) = do
  (Right expr, restTokens1) <- parseEqAOrLTOrBoolOrPar restTokens
  return (Right (NOT expr), restTokens1)
parseNotOrEqAOrLTOrBoolOrPar tokens = parseEqAOrLTOrBoolOrPar tokens


-- parseEqBOrNotOrEqAOrLTOrBoolOrPar: Parses an equality, NOT, arithmetic expression, less than, or boolean expression from a list of tokens.
parseEqBOrNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseEqBOrNotOrEqAOrLTOrBoolOrPar tokens = do
  result <- parseNotOrEqAOrLTOrBoolOrPar tokens
  case result of
    (Left aexp, restTokens1) -> 
      Just (Left aexp, restTokens1)
    (Right bexp, restTokens1) -> 
      parseRest bexp restTokens1
  where
    parseRest expr1 [] = Just (Right expr1, [])
    parseRest expr1 (EqBTok : restTokens1) = do
      result <- parseNotOrEqAOrLTOrBoolOrPar restTokens1
      case result of
        (Right bexp, restTokens2) -> 
          parseRest (EQb expr1 bexp) restTokens2
    parseRest expr1 restTokens1 = Just (Right expr1, restTokens1)


-- parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar: Parses an AND, equality, NOT, arithmetic expression, less than, or boolean expression from a list of tokens.
parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar tokens = do
  result <- parseEqBOrNotOrEqAOrLTOrBoolOrPar tokens
  case result of
    (Left aexp, restTokens1) -> 
      Just (Left aexp, restTokens1)
    (Right bexp, restTokens1) -> 
      parseRest bexp restTokens1
  where
    parseRest expr1 [] = Just (Right expr1, [])
    parseRest expr1 (AndTok : restTokens1) = do
      result <- parseEqBOrNotOrEqAOrLTOrBoolOrPar restTokens1
      case result of
        (Right bexp, restTokens2) -> 
          parseRest (AND expr1 bexp) restTokens2
    parseRest expr1 restTokens1 = Just (Right expr1, restTokens1)


-- parseBexp: Parses a boolean expression from a list of tokens.
parseBexp :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseBexp tokens =
  case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar tokens of -- !TODO: change function name  
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing


-- parseIf: Parses an if statement from a list of tokens.
parseIf :: [Token] -> Maybe (Stm, [Token])
parseIf (IfTok : restTokens) = do
  (Right bexp, restTokens1) <- parseBexp restTokens
  case restTokens1 of
    ThenTok : restTokens2 -> do
      case restTokens2 of
        OpenTok : restTokens3 -> do
          (stms, CloseTok : restTokens4) <- parseStms restTokens3
          continueWithElse bexp stms restTokens4
        _ -> do
          (stm1, restTokens3) <- parseStm restTokens2
          continueWithElse bexp [stm1] restTokens3          
    _ -> Nothing
    where 
    continueWithElse bexp stm1 (ElseTok : restTokens4) = do
      case restTokens4 of
        OpenTok : restTokens5 -> do
          (stms, CloseTok : EoSTok : restTokens6) <- parseStms restTokens5
          Just (IF bexp stm1 stms, restTokens6)
        _ -> do
          (stm2, restTokens5) <- parseStm restTokens4
          Just (IF bexp stm1 [stm2], restTokens5)
    continueWithElse _ _ _ = Nothing
parseIf tokens = Nothing


-- parseWhile: Parses a while loop from a list of tokens.
parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile (WhileTok : restTokens) = do
  (Right bexp, restTokens1) <- parseBexp restTokens
  case restTokens1 of
    DoTok : restTokens2 -> do
      case restTokens2 of
        OpenTok : restTokens3 -> do
          (stms, CloseTok : EoSTok : restTokens4) <- parseStms restTokens3
          Just (WHILE bexp stms, restTokens4)
        _ -> do
          (stm, restTokens3) <- parseStm restTokens2
          Just (WHILE bexp [stm], restTokens3)
    _ -> Nothing


-- parseStore: Parses a store operation from a list of tokens.
parseStore :: [Token] -> Maybe (Stm, [Token])
parseStore (VarTok s : AssignTok : restTokens) = 
   case parseAexp restTokens of
    Just (expr, EoSTok:restTokens1) -> Just (STORE s expr, restTokens1)
    Nothing -> Nothing
parseStore tokens = Nothing         


-- parseStm: Parses a statement from a list of tokens.
parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm (IfTok : tokens)  = parseIf (IfTok:tokens)
parseStm (WhileTok : tokens)  = parseWhile (WhileTok:tokens)
parseStm (VarTok s : AssignTok : tokens)  = parseStore (VarTok s : AssignTok : tokens)
parseStm tokens = 
  case parseBexp tokens of
    Just ( Right expr, restTokens) -> Just (BExp expr, restTokens)
    Just ( Left expr, restTokens) -> Just (AExp expr, restTokens)
    Nothing -> Nothing


-- parseStms: Parses multiple statements from a list of tokens.
parseStms :: [Token] -> Maybe ([Stm], [Token])
parseStms [] = Just ([], [])
parseStms tokens = 
  case parseStm tokens of
    Just (stm, restTokens) -> 
      case parseStms restTokens of
        Just (stms, restTokens1) -> Just (stm:stms, restTokens1)
        Nothing -> Just ([stm], restTokens)
    Nothing -> Nothing


-- buildData: Builds a list of statements from a list of tokens.
buildData:: [Token] -> [Stm]
buildData tokens = do
   case parseStms tokens of
    Just (stms, []) -> stms
    Nothing -> error "Syntax error" 


-- parse: Parses a string into a list of statements
parse :: String -> [Stm]
parse = buildData . lexer


