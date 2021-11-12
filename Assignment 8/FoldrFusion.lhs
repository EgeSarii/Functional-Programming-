--Ege Sari s1034535
--Group 81


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


----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f ==> foldr g e
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

>f (g x y) = h x (f y)
Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that for all x,y :
  foldr g e ((\x xs -> f x : xs) x y) = (g. f) x (foldr g e y)


Which is the case since:  

foldr g e ((\x xs -> f x : xs) x y) = foldr g e ((f x): y)
                                     = g (f x) (foldr g e y)
                                     = (g.f) x (f y) 

--------------------------------------
To prove:  map (g . e) = map g . map e
>map f = foldr (\x xs -> f x : xs) []
>f . foldr g e = foldr h (f e)
To show that map (g . e) = map g . map e, we can apply the fusion law using
  f ==> foldr (\y ys -> g y : ys) []
  g ==> (\x xs -> e x : xs)
  e ==> []
  h ==> (\x xs -> (g.e)x : xs)
  Namely as follows:
  map (g .e) 
      --------- rewrite map as foldr
 =foldr (\x xs -> (g.e)x : xs) []
                ----------------------foldr fusion
 = foldr (\y ys -> g y : ys) [] ((foldr(\x xs -> e x : xs)[]))
          -------------------------------rewrite foldr as map
 = map g (foldr(\x xs -> e x : xs)[])
  ---------------------------------------------rewrite foldr as map
 = map g . map e


 >f (g x y) = h x (f y)
 Since the "THEN" part of the fsion law can only be applied if the "IF" part is true,
  we need to show that for all x,y :
  foldr (\y ys -> g y : ys) [] ((\x xs -> e x : xs) x y) = (\x xs -> (g.e)x : xs) x (foldr (\y ys -> g y : ys) [] y)
  
  Which is the case since:  
  foldr (\y ys -> g y : ys) [] ((\x xs -> e x : xs) x y) = foldr (\y ys -> g y : ys) [] ((\x xs -> e x : xs) x y)
                            =foldr (\y ys -> g y : ys) [] (e x :y))
                            =(g.e) x foldr (\y ys -> g y : ys) [] y
                            =(g.e) x (map g y)
                            =(\x xs -> (g.e) x :xs) x (map g y)
                            =(\x xs -> (g.e)x : xs) x (foldr (\y ys -> g y : ys) [] y)

----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

