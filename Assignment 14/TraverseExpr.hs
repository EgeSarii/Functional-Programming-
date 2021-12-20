{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

import Control.Monad.State
import Data.List
import Data.Maybe

data Expr var = Var var | Lit Integer | Op BinOp (Expr var) (Expr var)
  deriving (Show,Eq)
data BinOp    = Add | Sub | Mul | Div
  deriving (Show,Eq)

instance Functor Expr where
  --fmap :: (a -> b) -> (Expr a -> Expr b)

instance Foldable Expr where
  --foldMap :: (Monoid m) => (a -> m) -> Expr a -> m

instance Traversable Expr where
  --traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)

--allVars :: (Ord a) => Expr a -> [a]

--renameVar :: String -> State [(String,Int)] Int
--renameVar name = do ...

--indexVars :: Expr String -> Expr Int
