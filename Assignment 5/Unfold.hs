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
primes= sieve [2..] where sieve(p:xs) = p :sieve[ n | n <- xs, n `mod` p /= 0 ]

primes1 :: [Integer]
primes1 = unfoldr (\as -> case as of [] -> Nothing ; (x:xs)-> (if (head xs)`mod` x /= 0 then Just(x, xs) else Nothing))[2..]
apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

-- (++) :: [a] -> [a] -> [a]
-- insert :: (Ord a) => a -> [a] -> [a]
-- unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
