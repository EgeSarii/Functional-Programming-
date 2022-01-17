module Prep where

import Data.Maybe
--Lists
--Streams
--Fold /unfold
--functor/applicative/monad
--Induction

{-
f1:: (a → a → b) → a → b
f1 f x = (f x x)

f2::Either(a → b) b → a → b
f2 (Left f) x = f x 
f2 (Right x) _ = x

f3:: [a] → [b]
f3 _ []

f4::Applicative f ⇒ f a → f b → f (a,b)
f4 x y = (,) <$> x <*> y

g5 a = a >>= \x → return(x + 1)

Monad m => ma -> ma

g6 =foldr (*) 1 . map length

g6 :: [[a]] -> Int

g7 x y =filter(x .head . y)

h7 :: (\a -> Bool) -> [a] -> [b] -> [b]

-}

compose :: [a -> a] -> a -> a
compose [] a = a 
compose (f:fs) a = f (compose fs (a))

compose' :: [a-> a] -> a -> a
--foldr :: (a → b → b) → b → [ a] → b
compose' fs a = foldr compose' a [fs]

{-
The fusion law for foldr states that 

IF, for all x,y:
  f (g x y) = h x (f y) 
THEN
  f . foldr g e = foldr h (f e) 


reverse :: [a] → [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse’ :: [a] → [a]
reverse’ xs = reverseCat xs []

reverseCat :: [a] → [a] → [a]
reverseCat [] ys = ys
reverseCat (x:xs) ys = reverseCat xs (x:ys)


reverseCat xs ys = reverse xs ++ ys

Base Case assume xs = []

reverseCat xs ys = reverseCat [] ys 
                 = ys                     {Definition of reverseCat 1}
                 = [] ++ ys               {Definition of ++ and []}
                 = reverse [] ++ ys       {Definition of reverse 1}

Inductive Case. Assume xs = (a: as) with IH = reverseCat as ys = reverse as ++ ys

reverseCat xs ys = reverseCat (a:as) ys
                 = reverseCat as (a:ys)     {Definition of reversecat 2 }
                 = reverse as ++ (a:ys)    {Definition of reverse 2 }
                 = reverse as ++ ([a] ++ ys)     {Definition of []}
                 = reverse (a:as) ++ ys     {Definition of reverse 2}


reverse xs = reverse’ xs

reverse' xs =   reverseCat xs []
            =   reverse xs ++ []
            =   reverse xs





(.) :: (b → c) → (a → b) → a → c
(f . g) x = f (g x) (0)

(++) :: [a] → [a] → [a]
[] ++ ys = ys (1)
(x:xs) ++ ys = x : (xs ++ ys) (2)

map :: (a → b) → [a] → [b]
map f [] = [] (3)
map f (x:xs) = f x : map f xs (4)

concat :: [[a]] → [a]
concat [] = [] (5)
concat (x:xs) = x ++ concat x (6)






map (f . g) xs = map f (map g xs)

Base Case
Assume xs = []

map (f.g) xs = map (f.g) []  
             = []           (3)
             = map f []      (3)
             = map f (map g [])  (3)
             = map f (map g xs)

Inductive Step 
Assume xs = (a: as) with IH map (f.g) as = map f (map g as)

map (f.g) (a:as) = (f.g) a : map (f.g) as       (4)
                   -------
                =  f(g a)  : map(f.g) as        (0)
                            -----------
                = f(g a)  :  map f (map g as )  (IH)
                 -----------------------------
                = map f (g a : map g as)           ()
                        ---------------
                = map f (map g (a:as))



foldr f b xs = compose (map f xs) b

Case 1 : xs = []

foldr f b []
----------------- by the definition of (7)
b
--------          by the definition of (9)
compose [] b
---------------------- by the definition of map
compose (map f []) b 


foldr :: (a → b → b) → b → [a] → b
foldr f b [] = b (7)
foldr f b (x:xs) = f x (foldr f b xs) (8)

compose :: [a → a] → a → a
compose [] = id (9)
compose (f:fs) b = f (compose fs b)(10)

Case 2 : xs = (a:as)
with IH =  foldr f b as = compose (map f as) b

foldr f b (a:as)
------------------------definition of (8)
f a (foldr f b as)
----------------------------definition of IH
f a (compose (map f as) b)
----------------------------definition 
f a .(compose (map f as)) b
----------------------------------
compose (f )



Case 1 : t = Tip a

map f ( tips (Tip a)) 
        -------------  definition of tips 1
map f  ([a])
------------- definition of map
[f a]
-------definition of tips 1
tips (Tip(f a))
    ------------definition of mapBtree
tips (mapBtree f (Tip a))

data Btree a = Tip a | Bin (Btree a) (Btree a)

mapBtree :: (a → b) → Btree a → Btree b
mapBtree f (Tip a) = Tip (f a)
mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

tips :: (Btree a) → [a]
tips (Tip x) = [x]
tips (Bin as bs) = tips as ++ tips bs


map f (tips t) = tips (mapBtree f t)

Case 2 : Assume t = Bin a b

IH : map f (tips b) = tips (mapBtree f b)

map f (tips (Bin a b))
      ----------------- definition of tips 1
map f (tips a ++ tips b)      
------------------------ definition of maps
map f tips a ++ (map f tips b)
------------       definition of IH
tips (mapBTree f a) ++ (map f tips b)
                       --------------definition of IH
tips (mapBTree f a) ++ tips (mapBTree f b)                     
----------------------------------------------definition of tips
tips (Bin (mapBtree f a ) (mapBtree f b))
------------------------------------------------definition of mapBTree 2
tips (mapBTree f (Bin a b))






if f (g x y) = h x (f y) then

f . foldr g e = foldr h (f e)



foldr g e . map f = foldr (g . f) e

***  map f = foldr (\x xs→f x : xs) []  ***


map func (ge x) = map func ( map ge z)
 
map func (ge z ) = 
  
foldr (\x xs ->func x : xs) [] (foldr (\x xs -> ge x : xs) [] z) = foldr (\x xs -> func x : xs) [] (ge z)

f : foldr (\x xs ->func x : xs) []
g :
h: (\x xs -> func x : xs)
e :



(++) = flip (foldr(:))
concat = foldr(++)[]
reverse = foldr (\x r -> r ++ [x])[]
reverse (xs ++ ys) = reverse ys ++ reverse xs

foldr op e [] = e
foldr op e (x:xs) = x ‘op‘ (foldrop e xs)

if f (g x y) = h x (f y) then

f . foldr g e =foldr h (f e)

reverse . concat= foldr(flip(++) . reverse) []
foldr (\x r -> r ++ [x])[] . foldr (++) [] = foldr(flip(++) . reverse) []

f : foldr (\x r -> r ++ [x])[]
g : (++)
e : []
h : flip(++) . reverse

f ( g x y) = h x (f y)
foldr (\x r -> r ++ [x])[] ((++) z y) =  flip (++) .reverse z ( foldr (\x r -> r ++ [x])[] y)
foldr (\x r -> r ++ [x])[] (z ++ y) = 
reverse (z ++ y) = reverse y ++ reverse z 

f ( g x y) = h x (f y)


f . foldr g e =foldr h (f e)

foldr g e . map f = foldr (g . f) e

foldr (\y xs -> f y : xs) [] . map gaa  =  foldr ((\y xs -> func y : xs) . gaa) [] 

g :(\y xs -> func y : xs)
e : []
f : gaa


head :: Stream a -> a
head a:> _ = a

tail :: Stream a → Stream a
tail _ :> as = as

repeat :: a → Stream a
repeat a = (a:> repeat a)

map :: (a → b) → (Stream a → Stream b)
map f (a:> as) = (f a) :> map f as

zipWith :: (a → b → c) → (Stream a → Stream b → Stream c)
zipWith f (a:> as) (b:> bs) = (f a b) :> zipWith f as bs

filter :: (a → Bool) → (Stream a → Stream a)
filter f (a:>as) = if f a then (a :> filter f as) else filter f as

toList :: Stream a → [a]
toList (a :> as) = a : toList as

cycle :: [a] → Stream a
cycle xs = printList xs  where
          printList (y:ys) = y:> (printList ys)
          printList [] = printList xs


naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1

editDistance :: String -> String -> Int
editDistance xs ys = newDistance (0,0)
  where
  newDistance :: (Int,Int) -> Int
  newDistance (x,y)
   |x == lengt


data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  --fmap :: (a-> b) -> Tree a -> Tree b
  fmap f (Leaf a) = Leaf ( f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)


mapM:: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapM f (Leaf a) = Leaf <$> ( f a )
mapM f (Node l r) = Node <$>mapM f l <*>mapM f r


newtype IdM r = IdM r

instance Functor IdM where
  fmap f (IdM x) =IdM (f x)
  
instance Monad IdM where
  --return :: a-> m a
  --(≫=) :: m a → (a → m b) → m b
  return x = (IdM x)
  (>>=) (IdM x) f = f x

instance Functor Tree where
  fmap f =fromIM . mapM (return . f)

relabel’::Tree a → Int→ (TreeInt, Int)
 
newtype StateM r = StateM (Int -> (r, Int) )

app :: StateM a -> (Int -> (a,Int))
app (StateM st) = st

instance Functor StateM where
  --fmap :: (a-> b) -> StateM a -> StateM b
  fmap f x = StateM (\s → let (a,s’) = app x s in (f a, s’) )  

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
  --fmap :: (a-> b) -> Tree a -> Tree b
  fmap _ (Leaf) = Leaf 
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

data Result a = Okay a | Error [String]

instance Functor Result where
  --fmap :: (a-> b) -> Result a -> Result b
  fmap _ (Error s) = Error s
  fmap f (Okay a) = Okay (f a)

instance Applicative Result where
  --pure :: a -> Result a
  pure a = Okay a
  --(<*>) :: f(a->b) -> f a -> f b
  (Okay f ) <*> r = fmap f r
  (Error s1) <*> (Error s2) = Error (union s1 s2)
  (Error s1) <*>  _ = Error s1
-}
intersperse:: a -> [a] ->[a]
intersperse a xs = init(concat[ ([x] ++[a]) | x<-xs])

allEqual::Eq a => [a] -> Bool
allEqual xs = all (== (xs!!0)) xs

f1::((a -> a) -> a) -> a
f1 f = f (\a -> a)

f2 :: Either a b -> (a -> b) -> b
f2 (Left a) f = f a
f2 (Right b) _ = b 

-- (>==) :: m a -> (a -> m b) -> m b 
f3 :: Monad m => m a -> m b -> m (a,b)
f3 ma mb = ma >>= (\a -> (mb >>= (\b -> return(a,b))))

f4 :: (a -> c, b -> c -> d) -> (a, b) -> d
f4 (fu1, fu2) (a,b) = fu2 b (fu1 a)

f5 :: (a-> b -> a) -> a -> b ->b ->a
f5 f x y z = f (f x y) z

f6 :: [(a,b)] -> [(b,a)]
f6 xs = reverse[(y,x) | (x,y) <- xs]

--foldMap :: Monoid m ⇒ (a → m) → f a → m
f7 :: (Foldable f, Num a) => f a -> [a]
f7 = foldMap (\x -> [x + 10, x + 20]) 

{-

--type Hash = 
--hashInt::Int -> Hash
--hashCombine:: Hash -> Hash -> Hash

class Hashable h where
  hash :: 



newtype Inflater a = IF { inflate::[Bit] -> Maybe(a, [Bit] ) }
--inflate :: Inflater a -> [Bit] -> Maybe(a, [Bit] ) 

instance Applicative Inflater where
  --pure :: a -> Inflater a
  --(<*>) :: Inflater (a-> b) -> Inflater a -> Inflater b 
  --fmap :: (a-> b) -> Inflater a -> Inflater b
  pure a = (IF (\b -> (Just (a,b))))
  f <*> a =IF $ \bs -> case inflate f bs of
                            Nothing -> Nothing
                            Just (func, out) -> inflate(fmap func a) out

instance Alternative Inflater where
  --empty :: f a
  empty = IF(\bs -> Nothing)

  --(<|>) :: f a -> f a -> f a
  fa1 <|> fa2 = IF(\bs -> case inflate fa1 bs of
                               Nothing -> inflate fa2 bs
                               Just -> Just)
bit:: Bit -> Inflater()
bit b = IF(\bs ->)
-}

{-
data Base = A | C | G | T
data Tree = Leaf Base | Fork Tree Tree
data Bit = O | I

compressBase :: Base -> [Bit]
compressBase b = case b of
                    A -> [O,O]
                    C -> [O,I]
                    G -> [I,O]
                    T -> [I,I]

compressTree :: Tree -> [Bit]
compressTree (Leaf b) = O : (compressBase b)
compressTree (Fork l r) = I : (compressTree l) ++ (compressTree r)
-}
newtype CircList a = CL {fromCL :: [a] }
--fromCL :: CircList a -> [a]

size' :: CircList a -> Int
size' cl = length(fromCL cl)

current :: CircList a -> Maybe a
current cl = case (fromCL cl) of
                  [] -> Nothing
                  (x:xs) -> Just x

insert :: [ a ] -> CircList a -> CircList a
insert xs cl = CL(xs ++ (fromCL cl))

delete :: Int -> CircList a -> CircList a
delete n cl = CL (drop n (fromCL cl))

rotate :: Int -> CircList a -> CircList a
rotate n cl = CL (snd((splitAt (length(fromCL cl)-n) (fromCL cl)))++ fst ((splitAt (length(fromCL cl)-n) (fromCL cl))))

takeFrom :: Int -> CircList a -> [a] 
takeFrom n cl = take n (cycle (fromCL cl))

equalCL :: (Eq a) => CircList a -> CircList a -> Bool
equalCL cl1 cl2 = or [fromCL(cl1) == fromCL(rotate n cl2) | n <-[0..size' cl1 -1]]

data QTree = Black |White |Node (QTree, QTree, QTree, QTree)
listToT4 [a, b, c, d ] = (a, b, c, d)
t4ToList (a, b, c, d) = [a, b, c, d ]

size :: QTree -> Int
size Black = 1
size White = 1
size (Node sts) = 1 + sum (map size (t4ToList sts))

diff :: QTree -> Int
diff Black = -1
diff White = 1
diff (Node sts) = sum (map diff (t4ToList sts))

data Bit = O |I deriving (Eq, Ord)

compress :: QTree -> [Bit]
compress Black = [O,O]
compress White = [O,I]
compress (Node sts) = I : concat (map compress (t4ToList sts))

decompress :: [ Bit ] -> QTree
decompress [] = []
decompress (O:O:bs) = (Black : decompress bs)
decompress (O:I:bs) = (White : decompress bs)

