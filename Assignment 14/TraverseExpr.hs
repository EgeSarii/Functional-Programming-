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
  foldMap f (Lit i) = mempty
  foldMap f (Var var) = (f var)
  foldMap f (Op op expr1 expr2) =  foldMap f expr1 <> foldMap f expr2

instance Traversable Expr where
  --traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Lit i) = pure(Lit i)
  traverse f (Var var) = pure(\x -> (Var x)) <*> (f var)
  traverse f (Op op expr1 expr2) = pure (Op op) <*> (traverse f expr1) <*> (traverse f expr2)

allVars :: (Ord a) => Expr a -> [a]
allVars expr = nub (foldMap (:[]) expr)

{-
return :: a → State s a,
get :: State s s
put :: s → State s ()
-}

--renameVar :: String -> State [(String,Int)] Int
--renameVar name = do var <- get 0
  --                  if (v)

renameHelper :: String -> Int -> State [(String,Int)] Int
renameHelper name index = do var <- get index
                             if (name == var ) then return 

--indexVars :: Expr String -> Expr Int
