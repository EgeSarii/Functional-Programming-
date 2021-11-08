> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t


{-
BASE CASE : We show that the property holds for t = Tip x

map f (tips (Tip x)) = map f ([x]) {definition of tips rule 1}
                     = map f (x:[]) {definition of []}
                     = f x: (map f []) {definition of map}
                     = f x: [] {definition of map}
                     = [f x] {definition of []}
                     = tips (Tip (f x)) {definition of tips rule 1}
                     = tips (mapBtree f Tip x) {definition of mapBtree rule 1}

We have proved that the propert holds for the base case.


Induction Step


-}