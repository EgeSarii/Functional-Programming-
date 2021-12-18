--Ege Sari s1034535
--Group 81

module Dice where 

import System.Random
import Control.Monad 
import Control.Applicative
import RandomState 
import RandomGen 
import Data.List

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr | Expr :-: Expr | Expr :/: Integer
          | Min Expr Expr | Max Expr Expr
          | SumBestOf [Expr] Int
  deriving (Show,Eq)

infixl 6 :+:  
infixl 6 :-: 
infixl 7 :/: 

type DiceAction m = Integer -> m Integer

evalM :: (Monad m) => Expr -> DiceAction m -> m Integer
evalM (Lit m)          _ = return m 
evalM (lhs :+: rhs)    d = (+) <$> evalM lhs d <*> evalM rhs d
evalM (lhs :-: rhs)    d = (-) <$> evalM lhs d <*> evalM rhs d
evalM (lhs :/: k)      d = div <$> evalM lhs d <*> pure k
evalM (Min lhs rhs)    d = min <$> evalM lhs d <*> evalM rhs d
evalM (Max lhs rhs)    d = max <$> evalM lhs d <*> evalM rhs d
evalM (Dice k)         d = d k
evalM (SumBestOf еs n) d = sum . take n . sortOn negate <$> mapM (\e->evalM e d) еs 

{-
--alternative definition, using do-notation 
evalM (Lit m)          _ = return m
evalM (lhs :+: rhs)    d = do { x <- evalM lhs d; y <- evalM rhs d; return (x+y) } 
evalM (lhs :-: rhs)    d = do { x <- evalM lhs d; y <- evalM rhs d; return (x-y) } 
evalM (lhs :/: k)      d = do { x <- evalM lhs d; return (x `div` k) }
evalM (Min lhs rhs)    d = do { x <- evalM lhs d; y <- evalM rhs d; return (min x y) }
evalM (Max lhs rhs)    d = do { x <- evalM lhs d; y <- evalM rhs d; return (max x y) }
evalM (Dice k)         d = do { roll <- d k; return roll }
evalM (SumBestOf еs n) d = do { rolls <- mapM (flip evalM d) еs; return $ sum $ take n $ sortOn negate $ rolls } 
-}

evalRIO :: Expr -> IO Integer 
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version 
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version 
  where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Integer 
evalIO expr = evalM expr askUser
  where 
  askUser :: Integer -> IO Integer 
  askUser k = do 
    putStr $ "Roll a d" ++ show k ++ ": "
    result <- read <$> getLine 
    if 1 <= result && result <= k then do 
      return result 
    else do
      putStrLn "That is not allowed, try again!"
      askUser k 

evalND :: Expr -> [Integer] 
evalND expr = evalM expr (\dicе->[1..dicе])

avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e) 

evalR :: Expr -> RandomState Integer 
evalR expr = evalM expr (\dicе->genRandInteger (1,dicе))

observed :: (Fractional a) => Int -> Expr -> IO a 
observed n expr = safeR $ observedR n expr 
-- note, we can define this directly as well:
--observed n expr = avg <$> replicateM n (evalRIO expr)

observedR :: (Fractional a) => Int -> Expr -> RandomState a
observedR n expr = avg <$> replicateM n (evalR expr)

--in case you want to manually roll many dice rolls and get statistics: 
diceExperiment :: (Fractional a) => Int -> Expr -> IO a
diceExperiment n expr = avg <$> replicateM n (evalIO expr)