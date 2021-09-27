module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

tree = Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) Leaf)


{----------- exercise 4.3 -------------}

--leaves :: Tree a -> Int
--nodes  :: Tree a -> Int
--height :: Tree a -> Int
--elems  :: Tree a -> [a]
--isSearchTree :: (Ord a) => Tree a -> Bool

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member _ Leaf = False
member x (Node a l r )= if (x==a) then True else False ||((member x l) || (member x r))

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node a l r) = if (x<=a) then (Node a (insert x l) r) else (Node a l (insert x r))


fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList [a] = insert a Leaf
fromList (x:xs)= insert(xs!!1)(insert x Leaf)

{-
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node a l r) 
 | not (member x (Node a l r)) = (Node a l r)
 | (member x (Node a l r)) = 
-}


{----------- exercise 4.5 -------------}

--inOrder :: Tree a -> [a]
--fromAscList :: [a] -> Tree a
--breadthFirst :: Tree a -> [a]

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

{-
layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree ++ "\n"
  where 
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  go pre _ Leaf = pre ++ "\n" -- change this to "" to get a more compact display
  go pre (preL,preR,preN) (Node k lt rt)
    = go (pre ++ preL) (hfill,v_bar,lbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++ 
      go (pre ++ preR) (v_bar,hfill,rbend) lt

  junct = "┤\n"
  hfill = fill ++ "  " 
  lbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  rbend = fill ++ "╰─"  -- change to "\-" if on Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
-}
