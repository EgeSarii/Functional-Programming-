--Ege Sari s1034535
--Group 81

module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1


editDistance :: String -> String -> Int
editDistance xs ys = newDistance (0,0)
  where
  newDistance :: (Int,Int) -> Int
  newDistance (x,y) 
   | x>=length xs = (length ys-y)
   | y>=length ys = (length xs-x)
   | otherwise = minimum [(1 + distArray!(x+1,y)), (1 + distArray! (x,y+1)), ((cost (xs!!x) (ys!!y)) + distArray!(x+1,y+1) ) ]
  cost x y = if x==y then 0 else 1

  distArray :: Array (Int,Int) Int
  distArray = array((0,0), (length xs, length ys ))[(ij, newDistance ij) | i <- [0..length xs], j <- [0..length ys], let ij = (i,j)]
