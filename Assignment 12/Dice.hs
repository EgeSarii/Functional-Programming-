module Dice where

import System.Random
import RandomState

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Integer -> m Integer


{-
evalM :: Expr -> DiceAction IO -> IO Integer  -- prototype
evalM (Lit i) da= return i
evalM (Dice i) da= da i
evalM (ex1 :+: ex2) da= (+) <$> (evalM ex1 da) <*> (evalM ex2 da)
evalM (Min ex1 ex2) da= min <$> (evalM ex1 da) <*> (evalM ex2 da)
evalM (Max ex1 ex2) da= max <$> (evalM ex1 da) <*> (evalM ex2 da)
-}

--fmap :: (a-> b) -> fa -> fb
--pure :: a -> f a
--(<*>) :: f (a -> b) -> f a -> f b
--return :: a -> m a
--(>>=) :: m a -> (a -> m b) -> m b
--(>>) :: m a -> m b -> m b


evalM ::(Monad m) => Expr -> DiceAction m -> m Integer -- final version
-- final version
evalM (Lit i) da= return i
evalM (Dice i) da= da i
evalM (ex1 :+: ex2) da= (+) <$> (evalM ex1 da) <*> (evalM ex2 da)
evalM (Min ex1 ex2) da= min <$> (evalM ex1 da) <*> (evalM ex2 da)
evalM (Max ex1 ex2) da= max <$> (evalM ex1 da) <*> (evalM ex2 da)



--evalRIO :: Expr -> IO Integer
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Integer
evalIO expr = evalM expr (\dice -> do {
  putStrLn ("enter for Dice " ++ show dice); 
  x <-getLine; 
  return (read x)
  })

--evalND :: Expr -> [Integer]

avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

--expectation :: (Fractional a) => Expr -> a
--expectation e = avg (evalND e)

--evalR :: Expr -> RandomState Integer

--observed :: (Fractional a) => Int -> Expr -> IO a
