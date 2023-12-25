-- 2. Parser
-- The second stage. It takes the sequence of tokens produced by the lexer and organizes them into a parse tree or abstract syntax tree (AST), which represents the syntactic structure of the program according to the language's grammar rules. The parser checks if the program is syntactically correct.

parser :: [Token] -> Expr

data Expr
    = IntLit Int -- integer constants
    | Add Expr Expr -- addition node
    | Mult Expr Expr -- multiplication node
    | Sub Expr Expr -- subtraction node
    | Div Expr Expr -- division node
    | Neg Expr -- negation node
    | BoolLit Bool -- boolean constants
    | And Expr Expr -- and node
    | Or Expr Expr -- or node
    | Not Expr -- not node
    | Eq Expr Expr -- equal node
    | Le Expr Expr -- less than or equal node
    | Var String -- variable node
    | Let String Expr Expr -- let node
    | If Expr Expr Expr -- if node