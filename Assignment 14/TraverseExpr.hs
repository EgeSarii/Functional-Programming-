--Ege Sari s1034535
--Group 81


{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

import Control.Monad.State
import Data.Foldable
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

--Some information is ignored for instance we ignore the operations. We only care about the
--variables (which are important for the last part indexVars).

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

renameVar :: String -> State [(String,Int)] Int
renameVar name = renameHelper name 0


renameHelper :: String -> Int -> State [(String,Int)] Int
renameHelper name index = do var <-get
                             if length(var)<=index then do {put (var ++ [(name, index+1)]) ; return index} 
                               else 
                               if fst(var!!index) == name then return index
                                else (renameHelper name (index+1))


--evalState :: State s a → s → a
--traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)

renameAllVars :: Expr String -> State [(String,Int)] (Expr Int)                      
renameAllVars  = traverse renameVar

indexVars :: Expr String -> Expr Int
indexVars expr = evalState (renameAllVars expr) []