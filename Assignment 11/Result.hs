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
 _ <*> (Error s) = Error s
 (Error s) <*> _ = Error s
 (Okay f) <*> (Okay a) = Okay (f a)
  
