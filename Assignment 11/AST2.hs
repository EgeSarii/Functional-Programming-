module AST where

-- this template uses infix constructors; feel free to use AST.hs (which uses infix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  --fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error s) = Error s
  fmap f (Okay a) = Okay (f a)

instance Applicative Result where
 -- (<*>) :: Result (a -> b) -> Result a -> Result b
 --pure :: a -> Result a
 pure a = Okay a
 _ <*> (Error s) = Error s
 (Error s) <*> _ = Error s
 (Okay f) <*> (Okay a) = Okay (f a)
  



eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a 
eval (Lit k) _ = pure (fromInteger k) 

eval (Add x y) vars = (+) <$> (eval x vars) <*> (eval y vars)
                       
eval (Sub x y) vars = (-) <$> (eval x vars) <*> (eval y vars)

eval (Mul x y) vars = (*) <$> (eval x vars) <*> (eval y vars)

eval (Div x y) vars = if (eval y /= 0) then (/) <$> (eval x vars) <*> (eval y vars) 
  else Error ["division by zero"]

eval (Var name) vars = case vars of
                        [] -> Error["unknown variable: x","unknown variable: y"]
                        (x:xs) -> if name == (fst x) then pure (snd x)
                                  else eval (Var name) xs
