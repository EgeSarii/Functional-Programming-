--Ege Sari s1034535
--Group 81

-----------------------------------------------
{-
NOTE!!

I think I have implemented correctly but it seems it works only if you enter the colours as string.
So you should test it for example with the input "Red Blue White Green" rather than Red Blue White Green

If it is not sufficient I am sorry, but this is the best I can do :(

-}
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

convertFromString :: [String] -> [Colour]
convertFromString [] = []
convertFromString (x:xs) 
 |x!!0 == 'W' = White : convertFromString xs
 |x!!0 == 'S' = Silver : convertFromString xs
 |x!!0 == 'G' = Green : convertFromString xs
 |x!!0 == 'R' = Red : convertFromString xs
 |x!!0 == 'O' = Orange : convertFromString xs
 |x!!0 == 'P' = Pink : convertFromString xs
 |x!!0 == 'Y' = Yellow : convertFromString xs
 |x!!0 == 'B' = Blue : convertFromString xs

--colour = Red Yellow Blue Pink
 
getColor :: IO [String] 
getColor = do 
  x1 <- readLn
  return (words x1)

playGame :: Int -> IO ()
playGame tries =
  do
   putStrLn("Try to guess the secret code word," ++ show tries ++ " tries left")  
   hSetBuffering stdin LineBuffering
   code <- getCode
   s <- getColor
   if scoreAttempt code (convertFromString s) == (4,0) then putStrLn "Correct"
   else if tries == 0 then putStr (" No more tries, game over.\n The code was " ++ show code)
   else do
      putStrLn (" Incorrect \n")
      putStrLn (show (fst (scoreAttempt code (convertFromString s))) ++ " colour(s) in the correct position," )
      putStrLn (show (snd (scoreAttempt code (convertFromString s))) ++ " colour(s) in the correct position,")
      playGame (tries-1)  
 
  


main :: IO ()
main = do
  putStrLn("I picked a random code word with 4 colours.")
  putStrLn("Possible colours are White Silver Green Red Orange Pink Yellow Blue.")
  playGame 12
