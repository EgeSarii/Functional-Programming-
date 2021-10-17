--Ege Sari s1034535
--Group 81


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

instance (Rankable key) => Rankable (Maybe key) where
  --rank:: (Rankable key) => [(Maybe key,a)] -> [[a]] 
  rank =  helperRank rank . break (\(x,y)-> if isJust x then True else False) .reverse . helperMaybe

helperMaybe:: [(Maybe key, v)] -> [(Maybe key, v)]
helperMaybe[] =[]
helperMaybe(x:xs) = if isNothing (fst x) then helperMaybe xs ++ [x] else [x] ++ helperMaybe xs

turnToNormal :: (Maybe key ,v) -> (key, v)
turnToNormal (a,b) = (fromJust a, b)

helperRank :: (Rankable key) =>( [(key,a)] -> [[a]] ) -> ([(Maybe key, a) ],[(Maybe key, a) ])-> [[a]]
helperRank f ([],x) = f (map turnToNormal x)
helperRank f (x, []) = [map snd x]
helperRank f (x, y) = (map snd x) : f (map turnToNormal y)


instance (Rankable key ) => Rankable [key] where
--  rank = 


assoc2 :: (String,a) -> (Char,(String,a))
assoc2 (s,v) = (fst (fromJust(uncons s)), (snd(fromJust(uncons s)), v))

{-

Unfortunately, I was not able to finish 6.6 , 6.7. I know how to do 6.6 by using uncons, with the same logic 
in 6.4 but no time left. Sorry for this.
-}