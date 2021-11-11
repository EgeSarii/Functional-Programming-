> module FoldrFusion where
>
> import Prelude hiding (map)

The fusion law for foldr states that

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

--> g= a and e=b
----------------------------------------------
To prove:  foldr a b . map f = foldr (a . f) b

To show that foldr a b . map f = foldr (a . f) b, we can apply the fusion law using
  f ==> foldr a b
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> a . f

Namely as follows:

  foldr a b . map f
              ----- rewrite map as foldr
= foldr a b . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (a . f) (foldr (\x xs->f x : xs) b [])
                ------------------------------ definition of foldr
= foldr (a . f) b

Since the "THEN" part of the fsion law can only be applied if the "IF" part is true,
we need to show that for all x,y :
  foldr a b ((\x xs -> f x : xs) x y) = (a. f) x (f y)


Which is the case since:  

foldr a b ((\x xs -> f x : xs) x y) = foldr a b ((f x): y)
                                     = a (f x) (foldr a b y)
                                     = (a.f) x (f y) 


--------------------------------------
To prove:  map (a . b) = map a . map b
>map f = foldr (\x xs -> f x : xs) []

To show that map (a . b) = map a . map b, we can apply the fusion law using
  f ==> foldr a b
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> a . f

  Namely as follows:
  map (a .b) 
      --------- rewrite map as foldr
 =foldr (\x xs -> (a.b)x : xs) []
  




----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

