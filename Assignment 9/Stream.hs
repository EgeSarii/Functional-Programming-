module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (5::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (a:> as) = a

tail :: Stream a -> Stream a
tail (a:> as) = as

repeat :: a -> Stream a
repeat x = x :> (repeat x)

map :: (a -> b) -> (Stream a -> Stream b)
map f (a :> as) = (f a) :> map f as

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (a:> as) (b:>bs) = (f a b) :> (zipWith f as bs)

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (a:> as) = if f a then (a :> filter f as) else filter f as

{-
3.)

If we call filter (\x→False) (from 0), then the filtered stream will be empty but since it is infinite
it will never terminate. So we will see an empty screen if we don't terminate manually.
-}


toList :: Stream a -> [a]
toList (a :> as) = [a] ++ toList as

cycle :: [a] -> Stream a
cycle (x: xs)= x :> cycleHelper (x:xs) (xs)

cycleHelper :: [a]-> [a] -> Stream a
cycleHelper init [] = cycle init
cycleHelper init (x:xs) = x :> (cycleHelper init xs)

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

primes :: Stream Integer
primes = 

--primetwins :: Stream (Integer,Integer)
 

--combine :: Stream a -> Stream a -> Stream a
