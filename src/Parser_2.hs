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


-- ===============================
-- !NOTE: Acho que este pedaço não é necessário, o restante codigo faz o mesmo mas 
-- parsing integers
parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntTok n : restTokens)
  = Just (NUM (fromIntegral n), restTokens)
parseInt tokens
  = Nothing

-- parsing products
parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens
  = case parseInt tokens of
    Just (expr1, MultTok : restTokens1) ->
      case parseProdOrInt restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MULT expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result -- can be ’Nothing’ or valid

-- parsing sums
parseSumOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrInt tokens
  = case parseProdOrInt tokens of
    Just (expr1, AddTok : restTokens1) ->
      case parseProdOrInt restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ADD expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result -- could be ’Nothing’ or valid

-- ========================


-- ========================
-- parethesised expressions
parseIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrPar (IntTok n : restTokens)
  = Just (NUM (fromIntegral n), restTokens)
parseIntOrPar (OpenTok : restTokens1)
  = case parseSumOrProdOrIntOrPar restTokens1 of
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
parseSumOrProdOrIntOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens
  = case parseProdOrIntOrPar tokens of
    Just (expr1, AddTok : restTokens1) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ADD expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, SubTok : restTokens1) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (SUB expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result


-- top-level
-- parseAexp :: [Token] -> Aexp
-- parseAexp tokens =
--   case parseSumOrProdOrIntOrPar tokens of
--     Just (expr, []) -> expr
--     _ -> error "Parse error"
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
  case parseSumOrProdOrIntOrPar tokens of
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing  -- !WARNING: may need a result -> result here



-- ========================
parseBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrPar (TruTok : restTokens) = Just (BOOL True, restTokens)
parseBoolOrPar (FalsTok : restTokens) = Just (BOOL False, restTokens)
parseBoolOrPar (OpenTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar restTokens1 of -- !TODO: change function name
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing


parseEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqAOrLTOrBoolOrPar tokens =
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

-- parseIEqORIneqOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
-- -- parseIEqORIneqOrBoolOrPar tokens

parseNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrEqAOrLTOrBoolOrPar tokens =
  case parseEqAOrLTOrBoolOrPar tokens of
    Just (expr1, NotTok : restTokens1) -> Just (NOT expr1, restTokens1)      
    result -> result


parseEqBOrNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqBOrNotOrEqAOrLTOrBoolOrPar tokens =
  case parseNotOrEqAOrLTOrBoolOrPar tokens of
    Just (expr1, EqBTok : restTokens1) ->
      case parseEqBOrNotOrEqAOrLTOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (EQb expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result


    

parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar tokens =
  case parseEqBOrNotOrEqAOrLTOrBoolOrPar tokens of
    Just (expr1, AndTok : restTokens1) ->
      case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) -> Just (AND expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just result -> Just result
    Nothing -> Nothing

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens =
  case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar tokens of -- !TODO: change function name  
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing


-- parseStm :: [Token] -> Stm
-- parseStm tokens =



-- buildData:: [Token] -> [Stm]
-- buildData tokens = 
--   case parseBexp tokens of
--     Just (expr, restTokens) -> expr : buildData restTokens
--     _ -> []

-- -- needs to read to EoTTok and call parseStm

-- parse :: String -> [Stm]
-- parse = buildData . lexer



-- ========================
  -- examples of use AEXP and BEXP in expressions
-- 3+2 <= 5
-- 3+2 <= 5 and 3+2 <= 5
-- 3+2==10   -- False
-- False and True
-- False and True = False
-- False and True = True
-- 3+2==10 = False   (boolean equality uses =, while integer equality uses ==)


-- !TODO: figure out what functions are needed
--  In boolean expressions, two instances of the same operator are also computed
-- from left to right. The order of precedence for the operations is (with the first
-- one being executed first): integer inequality (≤), integer equality (==), logical
-- negation (not), boolean equality (=), logical conjunction (and). For instance,
-- not True and 2 ≤ 5 = 3 == 4 is equivalent to (not True) and ((2 ≤ 5) = (3
-- == 4))
-- ORDER OF PRECEDENCE:
-- 0. Boolean Value & Arithmetic expressions
-- 1. integer inequality (≤)      -> arithmetic expressions
-- 2. integer equality (==)     -> arithmetic expressions
-- 3. logical negation (not)
-- 4. boolean equality (=)
-- 5. logical conjunction (and)   -> feito primeiro