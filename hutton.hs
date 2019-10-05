import Data.Char

data Expr
  = Lit Int
  | Add Expr Expr

eval :: Expr -> Int
eval (Lit i) = error "need full expression"
eval (Add (Lit a) (Lit b)) = a + b

--printExpr :: Expr -> String
--printExpr (Lit i) = 
