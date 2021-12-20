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
  fmap _ (Lit i) = Lit i
  fmap f (Var var) =  Var (f var)
  fmap f (Op op expr1 expr2) = Op op (fmap f expr1) (fmap f expr2)
  

instance Foldable Expr where
  --foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Lit i) = f i
  foldMap f (Var var) = (f var)
  foldMap f (Op op expr1 expr2) =  foldMap f expr1 <> foldMap f expr2

instance Traversable Expr where
  --traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)

--allVars :: (Ord a) => Expr a -> [a]

--renameVar :: String -> State [(String,Int)] Int
--renameVar name = do ...

--indexVars :: Expr String -> Expr Int
