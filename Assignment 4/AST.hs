--Ege Sari s1034535
--Group 81

module AST where

data Expr = Lit Integer 
          | VarX 
          | Add Expr Expr 
          | Mul Expr Expr 
          | Sub Expr Expr 
          | Div Expr Expr
          
eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit a) _ =  Just (fromIntegral a)
eval (VarX) a = (Just a)
eval (Add e1 e2) b = Just (head(fromVal(eval e1 b)) + head(fromVal(eval e2 b)))
eval (Mul e1 e2) b = Just (head(fromVal(eval e1 b)) * head(fromVal(eval e2 b)))
eval (Sub e1 e2) b = Just (head(fromVal(eval e1 b)) - head(fromVal(eval e2 b)))
eval (Div e1 e2) b = if head(fromVal(eval e2 b))== 0 then Nothing else Just (head(fromVal(eval e1 b)) / head(fromVal(eval e2 b)))



fromVal :: Maybe a -> [a]
fromVal Nothing = []
fromVal (Just a) = [a]