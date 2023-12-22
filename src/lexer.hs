{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- 1) LEXER
-- (also known as Lexical Analyzer): The first stage of the process. It takes the source code as input and breaks it down into a sequence of tokens. Tokens are the smallest meaningful units of the program, like keywords, identifiers, literals, operators, etc.

lexer :: String -> [Token]

data Token 
    = PlusTok
    | TimesTok
    | OpenTok
    | CloseTok
    | IntTok Int
    deriving (Show)

lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('(' : restStr) = OpenP : lexer restStr
lexer (')' : restStr) = CloseP : lexer restStr
lexer (chr : restStr)
    | isSpace chr = lexer restStr

--As-patterns allow you to pattern-match on a part of the structure, while still keeping a reference to the whole structure. In this case, str is the whole structure (a list of characters, or a string), and chr : _ is a pattern that matches the structure.
-- The pattern chr : _ is a way of decomposing the list str. In Haskell, a list can be split into its head and its tail. The head of the list is its first element, and the tail is the rest of the list. The : operator is used to construct a list by adding an element to the front of another list, but in the context of pattern matching, it's used to deconstruct a list into its head and tail.
lexer str@(chr : _) --string starts with a digit (@ means "as pattern")
    | isDigit chr
    = IntTok (stringToInt digitStr) : lexer restStr
    where
        (digitStr, restStr) = break (not . isDigit) str
        
        -- convert a string to an integer
        stringToInt :: String -> Int
        stringToInt=foldl (\acc chr->10*acc+digitToInt chr) 0
    
    -- runtime error:
lexer (_ : restString)
    = error ("unexpected character: '" ++ show chr ++ "'")

