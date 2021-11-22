--Ege Sari s1034535
--Group 81


module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green |Red | Orange | Pink | Yellow |Blue
 deriving (Eq, Show, Ord, Bounded, Enum, Read)

type Code = [Colour]

scoreAttempt :: (Ord a) => [a] -> [a] -> (Int, Int)
scoreAttempt code guess = ( correctMatches code guess , (totalMatches code (nub guess))- correctMatches code guess )

correctMatches :: (Ord a) => [a] -> [a]-> Int
correctMatches [] _ = 0
correctMatches _ [] = 0
correctMatches (c:cs) (g:gs) = if c== g then (1 + correctMatches cs gs) else correctMatches cs gs 

contains :: (Ord a) => [a] -> a-> Bool
contains [] _ = False
contains (c:cs) g = if c==g then True else contains cs g

totalMatches ::(Ord a) => [a] -> [a]-> Int
totalMatches _ [] = 0
totalMatches code (g:gs) = if (contains code g) then (1 + totalMatches code gs) else (totalMatches code gs)

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)

{---------------------- IO parts -------------------------------}

-- only here for example; you can remove these from your final file
roll_d6 :: IO Int
roll_d6 = randomRIO (1,6)

roll_2d6 :: IO Int
roll_2d6 = do
  a <- roll_d6
  b <- roll_d6
  return (a + b)

instance Random Colour where
    random g = case randomR (fromEnum (minBound :: Colour), fromEnum (maxBound :: Colour)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

getCode :: IO [Colour]
getCode = do 
  let x = (fromEnum (minBound :: Colour), fromEnum (maxBound :: Colour))
  let s = sequence (replicate 4 (randomRIO x))
  fmap (fmap (\x -> toEnum x)) s

playGame :: Int -> IO ()
playGame i = do
  putStrLn "I picked a random code word with 4 colours. "
  putStrLn "Possible colours are White Silver Green Red Orange Pink Yellow Blue. "
  putStrLn ("Try to guess the secret code word, "++ show i ++" tries left ")
  s <- getLine
  putStrLn (show (fst(scoreAttempt getCode (words s)))) 

  


main :: IO ()
main = do
  putStrLn "IMPLEMENT ME"
