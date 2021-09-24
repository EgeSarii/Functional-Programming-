module ListComprehensions where

import Data.List

permutation as bs = [ (a,b) | a <- as, b <- bs ]
--This functions creates permutations of two parameters one from as and one from bs. It is polymorphic function. It is not overloaded.


repetition n y   = [ y | i <- [1..n] ]
-- This functions creates repetition of y for n times. It is polymorphic and it is overloaded.


takeFirstN n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]
--It is take function which creates a list of first n elements from the list xs. It is polymorphic and it is overloaded.


getPosition a xs  = [ i | (i,x) <- zip [0..] xs, x == a]
-- It returns the positions of a in the list xs. It is polymorphic and it is overloaded.


zipWithSamePosition xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]
--It returns a tuple of list elements from two different lists but with the same index. It is polymorphic function. It is not overloaded.


concatenate xss   = [ x | xs <- xss, x <- xs ]
--It concatenates the lists in xss. It is polymorphic function. It is not overloaded.
