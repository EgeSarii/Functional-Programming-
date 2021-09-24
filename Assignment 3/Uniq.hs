--Ege Sari s1034535
--Group 81

module Uniq where

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) = if (x == y) then uniq (x:xs) else [x] ++ uniq (y:xs)
