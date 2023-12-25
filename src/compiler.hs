-- data Expr
--     = Num Int -- integer constants
--     | Add Expr Expr -- addition node
--     | Mult Expr Expr -- multiplication node
--     | Sub Expr Expr -- subtraction node
--     -- | Div Expr Expr -- division node
--     | Neg Expr -- negation node
--     | Boolean Bool -- boolean constants
--     | And Expr Expr -- and node
--     | Or Expr Expr -- or node
--     | Not Expr -- not node
--     | Eq Expr Expr -- equal node
--     | Le Expr Expr -- less than or equal node
--     -- | Var String -- variable node
--     -- | Let String Expr Expr -- let node
--     | Store String Expr -- store node
--     | If Expr Expr Expr -- if node
--     | Loop Expr Expr -- loop node


data Aexp --arithmetic expressions
    = Num Int -- integer constants
    | Add Aexp Aexp -- addition node
    | Mult Aexp Aexp -- multiplication node
    | Sub Aexp Aexp -- subtraction node

data Bexp --boolean expressions
    = Boolean Bool -- boolean constants
    | And Bexp Bexp -- and node
    | Or Bexp Bexp -- or node
    | Not Bexp -- not node
    | Eq Aexp Aexp -- equal node
    | Le Aexp Aexp -- less than or equal node

type Expr = Either Aexp Bexp

data Stm --statements
    = Store String Expr -- store node
    | If Bexp Expr Expr -- if node
    | Loop Bexp (Either Expr Stm) -- loop node
