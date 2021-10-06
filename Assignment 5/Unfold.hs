module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))


bits :: Int -> [Int]
bits n = reverse (dropWhile (<1) (convert n 62))

convert :: Int -> Int -> [Int]
convert s k = unfoldr( \(n,k) -> if (k== -1) then Nothing else (if (div n (2^k))==0 then Just (0, ((n),k-1)) else Just (1, ((n- 2^k), (k-1)))))(s,k) 

zip :: [a] -> [b] -> [(a,b)]
zip a b= unfoldr (\(as, bs) -> case as of [] -> Nothing ; (x:xs)-> (case bs of []-> Nothing; (y:ys)-> Just((x,y),(xs,ys)))) (a,b)

take :: Int -> [a] -> [a]
take n a = unfoldr (\(s,as)-> case as of [] -> Nothing ; (x:xs)-> (if s<1 then Nothing else Just(x, (s-1, xs)))) (n,a)

primes :: [Integer]
primes = unfoldr (\as -> case as of [] -> Nothing ; (x:xs)-> (if (head xs)`mod` x /= 0 then Just(x, xs) else Nothing))[2..]



apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) a b = apo (\(as,bs)-> case as of [] -> Left bs; (x:xs)->(case bs of []-> (Left as); (y:ys)->Right(x, (xs,bs)))) (a,b)

insert :: (Ord a) => a -> [a] -> [a]
insert e l = apo (\(a,as) -> case as of []-> Left [a]; (x:xs)-> (if a<=x then (Left (a:x:xs)) else Right (x, (a,xs)))) (e,l)

unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo  f a = apo (\s -> case s of Nothing -> Left []; (Just (x,y))-> Right(x,(f y))) (f a) 
