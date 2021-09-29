--Ege Sari s1034535
--Group 81

module AST where

data Expr = Lit Integer 
          | VarX 
          | Add Expr Expr 
          | Mul Expr Expr 
          | Sub Expr Expr 
          | Div Expr Expr

expr = Mul (VarX) (Mul (Lit 4) (Lit 2))
          
eval :: (Fractional a) => Expr -> a -> Maybe a
eval (Lit a) x =  Just (fromIntegral a)
eval (VarX) a = (Just a)
eval (Add e1 e2) b = Just (head(fromVal(eval e1 b)) + head(fromVal(eval e2 b)))
eval (Mul e1 e2) b = Just (head(fromVal(eval e1 b)) * head(fromVal(eval e2 b)))
eval (Sub e1 e2) b = Just (head(fromVal(eval e1 b)) - head(fromVal(eval e2 b)))
eval (Div e1 e2) b = if (eval(Mul e2 (VarX)) 0)== (Just 0.0) then Nothing else Just (head(fromVal(eval e1 b)) / head(fromVal(eval e2 b)))



fromVal :: Maybe a -> [a]
fromVal Nothing = []
fromVal (Just a) = [a]