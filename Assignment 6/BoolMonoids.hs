--Ege Sari s1034535
--Group 81


module Monoids where
import Data.Monoid


--The operation And (&&) satisfies the monoid laws. The operation is associative and they have an identity
-- The identity is True so for each element x, True && x = x = x && True.
newtype And2 = AndOp {fromAnd :: Bool} deriving (Show)

instance Semigroup And2 where
    x <> y = AndOp (fromAnd x && fromAnd y)
    

instance Monoid And2 where
    mempty = AndOp True
    -- I assume I don't have to define mconcat and mappend

--The operation Or (||) satisfies the monoid laws. The operation is associative and they have an identity
-- The identity is False so for each element x, False && x = x = x && False.    
newtype Or2 = OrOp {fromOr :: Bool} deriving (Show)

instance Semigroup Or2 where
    x <> y = OrOp (fromOr x || fromOr y)
    

instance Monoid Or2 where
    mempty = OrOp False
    -- I assume I don't have to define mconcat and mappend


--The operation XOR (exclusive Or) satisfies the monoid laws. The operation is associative and they have an identity
-- The identity is False so for each element False XOR x = x = x XOR x.

newtype XOr2 = XOrOp {fromXOr :: Bool} deriving (Show)

instance Semigroup XOr2 where
    x <> y = XOrOp ((not(fromXOr y) && fromXOr x) ||(not(fromXOr x) && fromXOr y))
    

instance Monoid XOr2 where
    mempty = XOrOp False
    -- I assume I don't have to define mconcat and mappend
    
--The operation Equivalence (<->) satisfies the monoid laws. The operation is associative and they have an identity
-- The identity is True so for each element True <-> x = x = x <-> x.

newtype Equ2 = EquOp {fromEqu :: Bool} deriving (Show)

instance Semigroup Equ2 where
    x <> y = EquOp ((not(fromEqu y) && fromEqu x) ||(not(fromEqu x) && fromEqu y))
    

instance Monoid Equ2 where
    mempty = EquOp True
    -- I assume I don't have to define mconcat and mappend


{-
I am not sure about the meaning but I assume this is the semantic of the operation "mconcat". 
The operation is a foldr (or foldl) operation. It accumulates every value in the list with applying
(<>) operation on them an at last (or at the beginnig) applying to the mempty, the identity

So for the first, and, it applies and to each element in the list with the identity and accumulates.
For example mconcat [(AndOp True), AndOp False] makes AndOp True <> AndOp False <> AndOp True ( the identity)
The returned value is of course AndOp {fromAnd = False}.

For the second, or, it applies or to each element in the list with the identity and accumulates.
For example mconcat [(OrOp True), OrOp False] makes OrOp True <> OrOp False <> OrOp False ( the identity)
The returned value is of course OrOp {fromOr = True}

For the third, xor, it applies or to each element in the list with the identity and accumulates.
For example mconcat [(XOrOp True), XOrOp False] makes XOrOp True <> XOrOp False <> XOrOp False ( the identity)
The returned value is of course XOrOp {fromXOr = True}

For the fourth, equivalence, it applies equivalence to each element in the list with the identity and accumulates.
For example mconcat [(EquOp True), EquOp False] makes OrOEquOp True <> EquOp False <> EquOp False ( the identity)
The returned value is of course EquOp {fromEqu = False}


-}