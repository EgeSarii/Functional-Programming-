--Ege Sari s1034535
--Group 81

module Dice where

import System.Random
import Data.List
import RandomState
import RandomGen

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



evalRIO :: Expr -> IO Integer
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
  where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Integer
evalIO expr = evalM expr (\dice -> do {
  putStrLn ("enter for Dice " ++ show dice); 
  x <-getLine; 
  return (read x)
  })

evalND :: Expr -> [Integer]
evalND expr = nub (evalM expr (\dice -> [1..dice]))

avg :: (Fractional a, Show a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a, Show a) => Expr -> a 
expectation e = avg (evalND e)

--genRandInteger :: (Integer,Integer) -> RandomState Integer
--roll_2d6 :: RandomState Integer
evalR :: Expr -> RandomState Integer
evalR expr = evalM expr (\dice -> genRandInteger(1,dice))

-- (>>= ) RandomState Integer -> (Integer -> RandomState [Integer]) -> RandomState [Integer]
helper :: Int -> Expr -> RandomState [Integer]
helper 1 expr = (evalR expr) >>= (\result -> return [result])
helper i expr = (evalR expr) >>= (\result -> 
  (helper (i-1) expr >>= (\result2 -> return(result : result2)) ))

observed :: (Fractional a )=>Int -> Expr -> RandomState a
observed i expr = (helper i expr) >>=  (\list -> return(div (sum list)(toInteger i)))