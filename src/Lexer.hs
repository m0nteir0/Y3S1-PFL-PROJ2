-- =================== LEXER ===========================

module Lexer where 
import Data.Char (isSpace, isDigit, isAlpha, isLower, digitToInt)


-- Token: A data type representing the different tokens that can be produced by the lexer.
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
    | AssignTok         -- ":=" -> assign value to variable
    -- end of statement
    |EoSTok             -- ";"
    deriving (Show)


-- Eq Token: An instance of the Eq class for the Token data type. 
-- This allows us to compare tokens for equality and inequality.
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
    AssignTok == AssignTok = True
    EoSTok == EoSTok = True
    _ == _ = False


-- lexer: A function that takes a string and returns a list of tokens. 
-- It recursively processes the string, identifying the tokens one by one.
lexer :: String -> [Token]    
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
lexer (':' : '=' : restStr) = AssignTok : lexer restStr
lexer str@(chr : restStr)
    | isSpace chr = lexer restStr
    | isAlpha chr = lexAlpha str
    | isDigit chr = lexDigit str                
    | otherwise = error ("unexpected character: " ++ [head str])


-- lexDigit: A function that takes a string that starts with a digit and returns a list of tokens.
-- It separates the leading number from the rest of the string and converts it to an integer token, with the correct value.
lexDigit :: String -> [Token]
lexDigit str = IntTok (stringToInt digitStr) : lexer restStr
    where
        (digitStr, restStr) = span isDigit str
        stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0


-- lexAlpha: A function that takes a string that starts with an alphabetic character and returns a list of tokens.
-- It separates the leading keyword or variable from the rest of the string and converts it to the appropriate token.
lexAlpha :: String -> [Token]
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
    where
        (keyword, restStr) = span isAlpha str
        (varStr, restStr') = span isAlpha str

