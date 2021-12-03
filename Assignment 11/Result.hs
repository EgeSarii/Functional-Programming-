--Ege Sari s1034535
--Group 81

module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  --fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error s) = Error s 
  fmap f (Okay a) = Okay (f a)

instance Applicative Result where
 -- (<*>) :: Result (a -> b) -> Result a -> Result b
 --pure :: a -> Result a
 pure a = Okay a
 Okay (f) <*> r = fmap f r
 (Error s1) <*> (Error s2) = Error (s1 `union` s2) 
 (Error s) <*> _ = Error s


