--Ege Sari s1034535
--Group 81


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
BASE CASE : We show that the property holds for t = Tip x, map f (tips (Tip x)) = tips (mapBtree f Tip x)

map f (tips (Tip x)) = map f ([x]) {by the definition of tips rule 1}
                     = map f (x:[]) {by the definition of []}
                     = f x: (map f []) {by the definition of map}
                     = f x: [] {by the definition of map}
                     = [f x] {by the definition of []}
                     = tips (Tip (f x)) {by the definition of tips rule 1}
                     = tips (mapBtree f Tip x) {by the definition of mapBtree rule 1}

We have proved that the propert holds for the base case.


Induction Step: We show that the property holds for t = Bin a b, map f (tips (Bin a b)) = tips (mapBtree f (Bin a b))
with IH :
    map f (tips b) = tips (mapBtree f b) for any b

map f (tips (Bin a b)) = map f (tips a ++ tips b) {by the definition of tips rule 2}
                       = (map f (tips a) ) ++ (map f (tips b)) {by the definition of ++}
                       = (tips (mapBtree f a) ) ++ (map f (tips b)) {by the definition of IH}
                       = (tips (mapBtree f a) ) ++ (tips (mapBtree f b)) {by the definition of IH}
                       = (tips Bin (mapBtree f a) (mapBtree f b)) {by the definition of tips rule 2}    
                       = (tips mapBtree f (Bin a b)) {by the definition of tips mapBtree rule 2}            
                                  
We have proved that the property holds for the inductive step. Hence We have proved that the property holds.
-}