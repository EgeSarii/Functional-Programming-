module FunList where


--foldr :: (a⟶b⟶b)⟶b⟶[a]⟶b


compose :: [a -> a] -> (a -> a)
compose []  x = x
compose (f:fs) x = f (compose fs x)

compose' :: [a -> a] -> (a -> a)
compose' fs x = foldr compose x [fs]

 
foo :: (Integral n) => n -> n
foo n = compose (map (+) [1..n]) 1
--This function 

--map :: (a->a) -> [a] -> [a]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f a (x:xs) = a

``