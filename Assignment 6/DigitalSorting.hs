module DigitalSorting where

import Data.List
import Data.Bool
import Data.Maybe
import Data.Either
class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd) . groupBy(\x y -> fst x == fst y) . sortOn fst 

instance Rankable Int where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

instance Rankable Bool where 
  rank  = (\x -> [fst x, snd x]). partitionEithers .map (\x -> if fst x then (Right (snd x)) else (Left (snd x)))

instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
  --rank:: (Rankable key1,Rankable key2) => [((key1,key2),a)] -> [[a]]
  rank = concat . map rank .rank . map assoc

assoc ::((k1,k2),a) -> (k1,(k2,a))
assoc ((k1,k2), a) = (k1, (k2,a))

--instance Rankable Maybe where
  --rank:: (Rankable key) => [(Maybe key,a)] -> [[a]]
 

--[(Nothing,1), (Nothing,2), (Just 1,3), (Just 1 ,4), (Just 2, 5)]
