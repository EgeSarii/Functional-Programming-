--Ege Sari s1034535
--Group 81

module FunList where

compose :: [a -> a] -> (a -> a)
compose []  x = x
compose (f:fs) x = f (compose fs x)

compose' :: [a -> a] -> (a -> a)
compose' fs x = foldr compose x [fs]

 
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1
--This function does the exactly same what foldr (*) 1 [1..n] does. It gives the value of n! .


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f a xs = compose (map (f) xs) a
