module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt n xs = [ x| (i,x)<- (zip [1..] xs), i/=n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = nub[(y,i)| (y,j) <- zip (sort xs) [0..], (x,i) <- zip xs [0..], x==y ]

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = nub[(x,j) | (x,i) <- (zip xs [0..]), (y,j) <- (zip (sort xs) [0..]), y==x]