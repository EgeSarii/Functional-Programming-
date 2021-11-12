--Ege Sari s1034535
--Group 81


module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
Derive
  inorderCat t xs = inorder t ++ xs

Case t= Leaf ( Base Case)

inorderCat Leaf xs  = inorder Leaf ++ xs { specification of inorderCat}
                    = [] ++ xs {definition of inorder rule 1 }
                    = xs {definition of ++ }

Case t = (Node x l r ) (Inductive Step) 
  with IH : inorderCat r xs = inorder r ++ xs

inorderCat (Node x l r) xs = inorder (Node x l r) ++ xs
                           = inorder l ++ [x] ++ inorder r ++ xs{definition of inorder rule 2}
                           = (inorder l ++ [x]) ++ (inorder r ++ xs) {definition of ++}
                           = (inorderCat l [x]) ++ (inorderCat r xs) {definition of IH}

Now we can re-define inorderCat
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs = xs
inorderCat (Node x l r) xs= (inorderCat l [x]) ++ (inorderCat r xs)

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt
{-
Derive
  elemsCat t xs = elems t ++ xs

Case t= Leaf ( Base Case)

elemsCat Leaf xs  = elems Leaf ++ xs { specification of inorderCat}
                    = [] ++ xs {definition of inorder rule 1 }
                    = xs {definition of ++ }

Case t = (Node x l r ) (Inductive Step) 
  with IH : elemsCat r xs = elems r ++ xs

elemsCat (Node x l r) xs = elems (Node x l r) ++ xs
                         = x : elems l ++ elems r ++ xs {definition of elems rule 2}
                         = x : (elems l ++ []) ++ (elems r ++ xs) {definition of ++}
                         = x : (elemsCat l []) ++ (elemsCat r xs) {definition of IH}

-}
elemsCat :: Tree a -> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x l r) xs = x : (elemsCat l []) ++ (elemsCat r xs)

elems' :: Tree a -> [a]
elems' t = elemsCat t []
