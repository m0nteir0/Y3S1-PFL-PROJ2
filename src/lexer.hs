{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- module Lexer (lexer, Token(..) ) where 

import Data.Char (isSpace, isDigit, isAlpha, isLower, digitToInt)

-- The string representing the program has the following syntactic constraints:
-- • All statements end with a semicolon (;).
-- • The string may contain spaces between each token (a token is basically a sequence of characters that are 
-- treated as a unit as it cannot be further broken down. Examples of tokens are the 
-- keywords (while, if, then, else, etc.) variables, operators (+, -, *, /),
-- delimiters/punctuators like the semicolon(;) or brackets, etc. integers and boolean values are also tokens).
-- • Variables begin with a lowercase letter (assume that no variable name can
-- contain a reserved keyword as a substring. For instance, anotherVar is an
-- invalid variable name as it contains the keyword not).
-- • Variables begin with a lowercase letter (assume that no variable name can
-- contain a reserved keyword as a substring. For instance, anotherVar is an
-- invalid variable name as it contains the keyword not).
-- • Operator precedence in arithmetic expressions is the usual: multiplications are
-- performed before additions and subtractions. Additions and subtractions have
-- the same level of precedence and are executed from left to right (i.e. they are
-- left-associative). Multiplications are also left-associative.
-- • Parentheses may be used to add priority to an operation. For instance, 1+2*3
-- = 7 but (1+2)*3 = 9.
-- • In boolean expressions, two instances of the same operator are also computed
-- from left to right. The order of precedence for the operations is (with the first
-- one being executed first): integer inequality (≤), integer equality (==), logical
-- negation (not), boolean equality (=), logical conjunction (and). For instance,
-- not True and 2 ≤ 5 = 3 == 4 is equivalent to (not True) and ((2 ≤ 5) = (3
-- == 4)).


-- 1) LEXER
-- (also known as Lexical Analyzer): The first stage of the process. It takes the source code as input and breaks it down into a sequence of tokens. Tokens are the smallest meaningful units of the program, like keywords, identifiers, literals, operators, etc.

lexer :: String -> [Token]

data Token
    = OpenTok           -- "("
    | CloseTok          -- ")"
    -- Artithmetic
    | AddTok            -- "+"
    | MultTok           -- "*"
    | SubTok            -- "-"
    | IntTok Int        -- "123" -> int
    -- Boolean
    | TruTok            -- "True"
    | FalsTok           -- "False"
    | EqITok          -- "=="
    | LeTok             -- "<="
    | EqBTok         -- "="
    | NotTok            -- "not"
    | AndTok            -- "and"
    -- While
    | WhileTok          -- "while"
    | DoTok             -- "do"
    -- If
    | IfTok             -- "if"
    | ThenTok           -- "then"
    | ElseTok           -- "else"
    -- Variable
    | VarTok String     -- "var" -> always starts with lowercase
    | AttTok            -- ":=" -> attributes value to variable
    -- end of statement
    |EoSTok             -- ";"
    deriving (Show)

instance Eq Token where
    OpenTok == OpenTok = True
    CloseTok == CloseTok = True
    AddTok == AddTok = True
    MultTok == MultTok = True
    SubTok == SubTok = True
    IntTok a == IntTok b = a == b
    TruTok == TruTok = True
    FalsTok == FalsTok = True
    EqITok == EqITok = True
    LeTok == LeTok = True
    EqBTok == EqBTok = True
    NotTok == NotTok = True
    AndTok == AndTok = True
    WhileTok == WhileTok = True
    DoTok == DoTok = True
    IfTok == IfTok = True
    ThenTok == ThenTok = True
    ElseTok == ElseTok = True
    VarTok a == VarTok b = a == b
    AttTok == AttTok = True
    EoSTok == EoSTok = True
    _ == _ = False

lexer [] = []
lexer ('+' : restStr) = AddTok : lexer restStr
lexer ('*' : restStr) = MultTok : lexer restStr
lexer ('-' : restStr) = SubTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('=' : '=' : restStr) = EqITok : lexer restStr
lexer ('<' : '=' : restStr) = LeTok : lexer restStr
lexer ('=' : restStr) = EqBTok : lexer restStr
lexer (';' : restStr) = EoSTok : lexer restStr
lexer (':' : '=' : restStr) = AttTok : lexer restStr
lexer str@(chr : restStr)
    | isSpace chr = lexer restStr
    | isAlpha chr = lexAlpha str
    | isDigit chr = lexDigit str
    | otherwise = error ("unexpected character: " ++ [head str])
    -- | isDigit chr = IntTok (stringToInt digitStr) : lexer restStr
    -- where
    --     (digitStr, restStr') = span isDigit str
    --     stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0


lexDigit str = IntTok (stringToInt digitStr) : lexer restStr
    where
        (digitStr, restStr) = span isDigit str
        stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0


lexAlpha :: [Char] -> [Token]
lexAlpha str
    | keyword == "True" = TruTok : lexer restStr
    | keyword == "False" = FalsTok : lexer restStr
    | keyword == "not" = NotTok : lexer restStr
    | keyword == "and" = AndTok : lexer restStr
    | keyword == "while" = WhileTok : lexer restStr
    | keyword == "do" = DoTok : lexer restStr
    | keyword == "if" = IfTok : lexer restStr
    | keyword == "then" = ThenTok : lexer restStr
    | keyword == "else" = ElseTok : lexer restStr
    | isLower (head varStr) = VarTok varStr : lexer restStr'
    | otherwise = error ("variables should start with lowecase char, not: " ++ [head str])
    -- | otherwise = error "unexpected character"
    where
        (keyword, restStr) = span isAlpha str
        (varStr, restStr') = span isAlpha str


runTests = do                                            
    print $ lexer "True"
    print $ lexer "False"
    print $ lexer "True and False"
    print $ lexer "123 + 1"
    print $ lexer "1 + 2 * 3"
    print $ lexer "x := 5; x := x - 1;"
    print $ lexer "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2"
    -- print $ lexer
    -- print $ lexer
    -- print $ lexer
    -- print $ lexer
    -- print $ lexer "."



-- --As-patterns allow you to pattern-match on a part of the structure, while still keeping a reference to the whole structure. In this case, str is the whole structure (a list of characters, or a string), and chr : _ is a pattern that matches the structure.
-- -- The pattern chr : _ is a way of decomposing the list str. In Haskell, a list can be split into its head and its tail. The head of the list is its first element, and the tail is the rest of the list. The : operator is used to construct a list by adding an element to the front of another list, but in the context of pattern matching, it's used to deconstruct a list into its head and tail.
-- lexer str@(chr : _) --string starts with a digit (@ means "as pattern")
--     | isDigit chr
--     = IntTok (stringToInt digitStr) : lexer restStr
--     where
--         -- (digitStr, restStr) = span isDigit str -- DOS SLIDES: binding uses the break function to split the input string into two parts: digitStr (a string of consecutive digits at the start of the input string) and restStr (the rest of the string).
--         (digitStr, restStr) = span isDigit str
--         -- convert a string to an integer
--         stringToInt :: String -> Int
--         stringToInt=foldl (\acc chr->10*acc+digitToInt chr) 0

--     -- runtime error:
-- lexer (_ : restString)
--     = error ("unexpected character: '" ++ show chr ++ "'")

