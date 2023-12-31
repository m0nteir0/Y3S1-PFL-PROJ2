-- -- 2. Parser -> The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct.
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

-- missing:
-- 1. introduzir variaveis no parser
-- 2. corrigir o 'if' para verificar parentesis
-- 3. fazer parser do while
-- 4. parser highlevel e lidar com ;


module Parser_2 where

import Lexer_1
import Control.Exception (handle)
-- import Text.Parsec (parse)

-- - !TODO: Add FETCH Expression --> figure out how to use it

runTestsP = do
  print $ lexer "x := 2 * (3 + 4)"


-- parsed information will have instructions of this kind of structure:
data Aexp --arithmetic expressions
    = NUM Integer -- integer constants
    | ADD Aexp Aexp -- addition node
    | MULT Aexp Aexp -- multiplication node
    | SUB Aexp Aexp -- subtraction node
    | VAR String -- get the value of a variable
    deriving Show

data Bexp --boolean expressions
    = BOOL Bool -- boolean constants
    | AND Bexp Bexp -- and node
    -- | Or Bexp Bexp -- or node
    | NOT Bexp -- not node
    | EQa Aexp Aexp -- equal node --
    | EQb Bexp Bexp -- equal node 
    | LE Aexp Aexp -- less than or equal node
    -- | VARB String -- get the value of a variable
    deriving Show

-- type Expr = Either Aexp Bexp

data Stm --statements --> TUDO
    = STORE String Aexp-- store node
    -- = STORE String  (Either Aexp Bexp)-- store node
    -- | VAR String      -- get the value of a variable
    | IF Bexp Stm Stm -- if node
    | WHILE Bexp Stm -- loop node
    | AExp Aexp
    | BExp Bexp
    deriving Show


-- parethesised expressions
parseIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseIntOrPar (IntTok n : restTokens) = Just (NUM (fromIntegral n), restTokens)
parseIntOrPar (VarTok s : restTokens) = Just (VAR s, restTokens)
parseIntOrPar (OpenTok : restTokens1)
  = case parseAexp restTokens1 of
  -- = case parseSumOrProdOrIntOrPar restTokens1 of
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseIntOrPar tokens = Nothing


parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens = do
  (expr1, restTokens1) <- parseIntOrPar tokens
  parseRest expr1 restTokens1
  where
    parseRest expr1 [] = Just (expr1, [])
    parseRest expr1 (MultTok : restTokens1) = do
      (expr2, restTokens2) <- parseIntOrPar restTokens1
      parseRest (MULT expr1 expr2) restTokens2
    parseRest expr1 restTokens1 = Just (expr1, restTokens1)


parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens = do
  (expr1, restTokens1) <- parseProdOrIntOrPar tokens
  parseRest expr1 restTokens1
  where
    parseRest expr1 [] = Just (expr1, [])
    parseRest expr1 (AddTok : restTokens1) = do
      (expr2, restTokens2) <- parseProdOrIntOrPar restTokens1
      parseRest (ADD expr1 expr2) restTokens2
    parseRest expr1 (SubTok : restTokens1) = do
      (expr2, restTokens2) <- parseProdOrIntOrPar restTokens1
      parseRest (SUB expr1 expr2) restTokens2
    parseRest expr1 restTokens1 = Just (expr1, restTokens1)


-- top-level
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
  case parseSumOrProdOrIntOrPar tokens of
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing  -- !WARNING: may need a result -> result here

-- ========================
parseBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseBoolOrPar (TruTok : restTokens) = Just (Right (BOOL True), restTokens)
parseBoolOrPar (FalsTok : restTokens) = Just (Right (BOOL False), restTokens)
-- parseBoolOrPar (VarTok s : restTokens) = Just (Right (VARB s), restTokens)
parseBoolOrPar (OpenTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar restTokens1 of -- !TODO: change function name
    Just (expr, CloseTok : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseBoolOrPar tokens = Nothing

-- this old version of the parser works
parseEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseEqAOrLTOrBoolOrPar tokens =
  case parseAexp tokens of --see if there is an integer expression
    Just (expr1, LeTok : restTokens1) -> --if there is, check if there is a less than or equal operator next
      case parseAexp restTokens1 of -- if there is, check if there is another integer expression
        Just (expr2, restTokens2) -> Just (Right(LE expr1 expr2), restTokens2) -- if there is, return the expression
        Nothing -> Nothing
    -- Just result -> Just result 
    Just (expr1, EqITok : restTokens1) -> --if there is, check if there is a less than or equal operator next
      case parseAexp restTokens1 of -- if there is, check if there is another integer expression
        Just (expr2, restTokens2) -> Just (Right (EQa expr1 expr2), restTokens2) -- if there is, return the expression
        Nothing -> Nothing
    Just (expr1, restTokens1) -> Just (Left expr1, restTokens1)-- means that it is exclusively an aexp, not part of a boolexp

    Nothing -> parseBoolOrPar tokens -- if there is no integer expression, check if there is a boolean expression
  -- case parseBoolOrPar tokens -- será que é necessario?


parseNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseNotOrEqAOrLTOrBoolOrPar (NotTok : restTokens) = do
  (Right expr, restTokens1) <- parseEqAOrLTOrBoolOrPar restTokens
  return (Right (NOT expr), restTokens1)
parseNotOrEqAOrLTOrBoolOrPar tokens = parseEqAOrLTOrBoolOrPar tokens


parseEqBOrNotOrEqAOrLTOrBoolOrPar :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseEqBOrNotOrEqAOrLTOrBoolOrPar tokens = do
  result <- parseNotOrEqAOrLTOrBoolOrPar tokens
  case result of
    (Left aexp, restTokens1) -> 
      Just (Left aexp, restTokens1)
    (Right bexp, restTokens1) -> 
      parseRest bexp restTokens1
    -- Nothing -> Nothing
  where
    parseRest expr1 [] = Just (Right expr1, [])
    parseRest expr1 (EqBTok : restTokens1) = do
      result <- parseNotOrEqAOrLTOrBoolOrPar restTokens1
      case result of
        (Right bexp, restTokens2) -> 
          parseRest (EQb expr1 bexp) restTokens2
    parseRest expr1 restTokens1 = Just (Right expr1, restTokens1)


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
        -- Nothing -> Nothing
    parseRest expr1 restTokens1 = Just (Right expr1, restTokens1)


parseBexp :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseBexp tokens =
  case parseAndOrEqBOrNotOrEqAOrLTOrBoolOrPar tokens of -- !TODO: change function name  
    Just (expr, restTokens) -> Just (expr, restTokens)
    _ -> Nothing

-- ======================== Tudo kinder bueno acima

parseIf :: [Token] -> Maybe (Stm, [Token])
parseIf (IfTok : restTokens) = do
  (Right bexp, restTokens1) <- parseBexp restTokens
  case restTokens1 of
    ThenTok : restTokens2 -> do
      -- case restTokens2 of
      --   OpenTok : restTokens3 -> do
      --     (stm1, CloseTok : restTokens4) <- parseStm restTokens3
      (stm1, restTokens3) <- parseStm restTokens2
      case restTokens3 of
        -- EoSTok : ElseTok : restTokens4 -> do
        ElseTok : restTokens4 -> do
          -- (stm2, EoSTok :restTokens5) <- parseStm restTokens4
          (stm2, restTokens5) <- parseStm restTokens4
          return (IF bexp stm1 stm2, restTokens5)
        _ -> Nothing
    _ -> Nothing
parseIf tokens = Nothing


-- parseIf :: [Token] -> Maybe (Stm, [Token])
-- parseIf (IfTok : restTokens) = do
--   (Right bexp, restTokens1) <- parseBexp restTokens
--   case restTokens1 of
--     ThenTok : restTokens2 -> do
--       case restTokens2 of
--         OpenTok : restTokens3 -> do
--           (stms, CloseTok : EoSTok : restTokens4) <- parseStms restTokens3
--           let stm1 = foldr1 Seq stms
--           continueWithElse stm1 restTokens4
--         _ -> do
--           (stm1, restTokens3) <- parseStm restTokens2
--           continueWithElse stm1 restTokens3
--     _ -> Nothing
--   where
--     parseStms :: [Token] -> Maybe ([Stm], [Token])
--     parseStms (CloseTok : EoSTok : restTokens) = Just ([], restTokens)
--     parseStms tokens = do
--       (stm, restTokens1) <- parseStm tokens
--       (stms, restTokens2) <- parseStms restTokens1
--       Just (stm : stms, restTokens2)

--     continueWithElse :: Stm -> [Token] -> Maybe (Stm, [Token])
--     continueWithElse stm1 (ElseTok : restTokens4) = do
--       (stm2, restTokens5) <- parseStm restTokens4
--       Just (IF bexp stm1 stm2, restTokens5)
--     continueWithElse _ _ = Nothing
-- parseIf tokens = Nothing




parseWhile :: [Token] -> Maybe (Stm, [Token])
parseWhile (WhileTok : restTokens) = do
  (Right bexp, restTokens1) <- parseBexp restTokens
  case restTokens1 of
    DoTok : restTokens2 -> do
      -- (stm, EoSTok:restTokens3) <- parseStm restTokens2
      (stm, restTokens3) <- parseStm restTokens2
      return (WHILE bexp stm, restTokens3)
    _ -> Nothing


parseStore :: [Token] -> Maybe (Stm, [Token])
parseStore (VarTok s : AssignTok : restTokens) = 
   case parseAexp restTokens of -- maybe AExp is enough
    -- Just (expr, restTokens1) -> Just (STORE s expr, restTokens1)
    Just (expr, EoSTok:restTokens1) -> Just (STORE s expr, restTokens1)
    Nothing -> Nothing
parseStore tokens = Nothing         


-- parseStm :: [Token] -> Maybe (Stm, [Token])
-- parseStm tokens =
--   case parseIf tokens of
--     Just (stm, restTokens) -> Just (stm, restTokens)
--     -- Nothing -> case parseWhile tokens of
--     --   Just (stm, restTokens) -> Just (stm, restTokens)
--     Nothing -> case parseStore tokens of
--       Just (stm, restTokens) -> Just (stm, restTokens)
--       Nothing -> case parseBexp tokens of
--         Just (expr, restTokens) -> Just (BExp expr, restTokens)
--         Nothing -> case parseAexp tokens of
--           Just (expr, restTokens) -> Just (AExp expr, restTokens)
--           Nothing -> Nothing
  
--       -- this will continue

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm (IfTok : tokens)  = parseIf (IfTok:tokens)
parseStm (WhileTok : tokens)  = parseWhile (WhileTok:tokens)
parseStm (VarTok s : AssignTok : tokens)  = parseStore (VarTok s : AssignTok : tokens)
parseStm (EoSTok : tokens) = parseStm tokens --added
parseStm tokens = 
  case parseBexp tokens of
    Just ( Right expr, restTokens) -> Just (BExp expr, restTokens)
    Just ( Left expr, restTokens) -> Just (AExp expr, restTokens)
    Nothing -> Nothing

-- parseStm :: [Token] -> Maybe (Stm, [Token])
-- parseStm tokens =
--   case parseIf tokens of
--     Just (stm, restTokens) -> handleSemicolon stm restTokens
--     Nothing -> case parseStore tokens of   
--       Just (stm, restTokens1) -> handleSemicolon stm restTokens1
--       Nothing -> case parseBexp tokens of
--         Just (Left aexp, restTokens) -> 
--           handleSemicolon  (AExp aexp) restTokens
--         Just (Right bexp, restTokens) -> 
--           handleSemicolon (BExp bexp) restTokens
--         Nothing -> Nothing
--   where
--     handleSemicolon stm [] = Just (stm, [])
--     handleSemicolon stm (EoSTok : restTokens) = Just (stm, restTokens)
--     handleSemicolon stm restTokens = Nothing -- handle the case where there's no semicolon

buildData:: [Token] -> [Stm]
buildData [] = [] -- caso base, quando a lista de tokens estiver vazia
buildData tokens = 
  case parseStm tokens of
    Just (stm, restTokens) -> stm : buildData restTokens
    Nothing -> error "Syntax error"

-- -- needs to read to EoTTok and call parseStm

parse :: String -> [Stm]
parse = buildData . lexer



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


-- tests:
-- parse "x := 2 * (3 + 4)"
-- answer: [Store "x" (Mult (Num 2) (Add (Num 3) (Num 4)))]
-- with if:
-- parse "if true then x := 2 * (3 + 4); else x := 2 * (3 + 4);"
-- answer: [If (Bool True) (Store "x" (Mult (Num 2) (Add (Num 3) (Num 4)))) (Store "x" (Mult (Num 2) (Add (Num 3) (Num 4))))]
-- parse several statements:
-- parse "x := 2 * (3 + 4); y := 2 * (3 + 4);"
-- answer: [Store "x" (Mult (Num 2) (Add (Num 3) (Num 4))),Store "y" (Mult (Num 2) (Add (Num 3) (Num 4)))]