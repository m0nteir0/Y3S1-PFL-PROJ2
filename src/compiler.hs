data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

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
    = NUM Integer -- integer constants
    | ADD Aexp Aexp -- addition node
    | MULT Aexp Aexp -- multiplication node
    | SUB Aexp Aexp -- subtraction node

data Bexp --boolean expressions
    = BOOL Bool -- boolean constants
    | AND Bexp Bexp -- and node
    -- | Or Bexp Bexp -- or node
    | NOT Bexp -- not node
    | EQa Aexp Aexp -- equal node --
    | EQb Bexp Bexp -- equal node --
    | LE Aexp Aexp -- less than or equal node

-- type Expr = Either Aexp Bexp

data Stm --statements --> TUDO
    | 
    | STORE String Expr -- store node
    | IF Bexp Expr Expr -- if node
    | LOOP Bexp (Either Expr Stm) -- loop node


compA :: Aexp -> Code
compA (NUM n) = [Push n]
compA (ADD e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (MULT e1 e2) = compA e1 ++ compA e2 ++ [Mult]
compA (SUB e1 e2) = compA e1 ++ compA e2 ++ [Sub]

compB :: Bexp -> Code
compB (BOOL b) = if b then [Tru] else [Fals]
compB (AND e1 e2) = compB e1 ++ compB e2 ++ [And]
compB (NOT e) = compB e ++ [Neg]
compB (EQa e1 e2) = compA e1 ++ compA e2 ++ [Equ]
compB (EQb e1 e2) = compB e1 ++ compB e2 ++ [Equ]
compB (LE e1 e2) = compA e1 ++ compA e2 ++ [Le]

compile :: [Stm] -> Code


-- compile :: Expr -> Code
-- compile (NUM n) = [Push n]
-- compile (ADD e1 e2) = compile e1 ++ compile e2 ++ [Add]
-- compile (MULT e1 e2) = compile e1 ++ compile e2 ++ [Mult]
