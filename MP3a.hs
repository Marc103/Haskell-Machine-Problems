module MP3a where

import Data.List
import Data.List.Split


{-
  Binary tree related type definitions.
-}
data BinTree a = Node a (BinTree a) (BinTree a)
data Direction = L | R deriving Show 


{-
  Creates a `BinTree` where all nodes contain the specified value.
-}
treeRepeat :: a -> BinTree a
treeRepeat val = Node val (treeRepeat val) (treeRepeat val)


{-
  Creates a `BinTree` where the nodes are populated with the natural numbers, 
  starting at the "root" of the tree and then downwards and from left to right 
  across each level of the tree.
-}
treeNats :: BinTree Integer
treeNats = trNaturals 1


trNaturals val = Node val (trNaturals next) (trNaturals (next+1))
                    where next = val*2


{-
  Takes a list of `Direction` values (`L`eft or `R`ight) and traverses the tree 
  to return the value in the target node. 
  
  Examples:
  
  treeVal [L,R] treeNats => 5
  
  treeVal [] treeNats => 1  
-}

instance Eq Direction where
    L == L = True
    R == R = True
    L == R = False
    R == L = False
    

treeVal :: [Direction] -> BinTree a -> a
treeVal [] (Node val _ _) = val
treeVal (d:ds) (Node val left right) | d == L = treeVal ds left
                                     | d == R = treeVal ds right





{-
  Converts a tree to a list; the root of the tree is the first list value, and 
  the values in the tree are taken downwards and across each level. 
  
  Examples:

  take 10 $ treeToList treeNats
  => [1,2,3,4,5,6,7,8,9,10]
-}
treeToList :: BinTree a -> [a]
treeToList node = twigToList (infinityAndBeyond) node
twigToList (d:ds) node = treeVal d node: twigToList ds node

-- need list of infinite directions that 'index' each node in order
infinityAndBeyond = preG ++ (g) ++ (cycleG g)
                      where g = genesis [L,R] [L,R] [L,R] [L,R]
-- hard part was finding a way to combine L,R with different "digit" spaces
genesis ox oy (x:xs) (y:ys) = [x,y]:genesis ox oy (x:xs) (ys)
genesis ox oy (x:xs) [] = genesis ox oy xs oy
genesis ox oy [] _ = []

postGL [] = []
postGL (g:enesis) = [L:g] ++ postGL enesis
postGR [] = []
postGR (g:enesis) = [R:g] ++ postGR enesis

cycleG ls = s ++ cycleG s
              where fls = postGL ls
                    frs = postGR ls
                    s = fls ++ frs
preG = [[],[L],[R]]






{-
  "Flips" the `BinTree` so that we obtain the mirror image of the original tree. 
  
  For instance, flipping the tree on the left gives us the one on the right:

             1                     1
           /   \                 /   \
          2     3      =>       3     2
         / \   / \             / \   / \
        4   5 6   7           7   6 5   4
-}
treeFlip :: BinTree a -> BinTree a
treeFlip (Node val left right) = Node val (treeFlip right) (treeFlip left)


{-
  Returns a `BinTree` based on an infinite list where the first item of the list
  is the root, and subsequent items from the list are assigned to nodes 
  downwards and across the levels of the tree.
  
  Examples:

  take 10 $ treeToList $ treeFromList [1..]
  => [1,2,3,4,5,6,7,8,9,10]
  
  Hint: check out your `treeNats` for inspiration!
-}



treeFromList :: [a] -> BinTree a
treeFromList inf = recreate inf treeNats

-- I just realized that my treeNats literally serves as an indexing system to get values from any infinite list (thanks for the hint)
recreate :: (Eq t, Num t) => [a] -> BinTree t -> BinTree a
recreate inf@(i:ls) (Node val left right) = Node (getItem val inf) (recreate inf left) (recreate inf right)
getItem 1 (i:ls) = i
getItem n (i:ls) = getItem (n-1) ls

{-
  Takes a function and an initial value, and returns a `BinTree` where the root 
  value is the initial value, and values in subsequent nodes are based on 
  repeated applications of the given function to the value.
  
  Examples:

  treeVal [R,R,R] $ treeIterate (2*) 1
  => 16384

  take 15 $ treeToList $ treeFlip $ treeIterate (2*) 1
  => [1,4,2,64,32,16,8,16384,8192,4096,2048,1024,512,256,128]

  Hint: checkout `iterate`.
-}
treeIterate :: (a -> a) -> a -> BinTree a
treeIterate f init = recreate (iterate f init) treeNats

{-
  BinTree instance of the Functor class.
-}
instance Functor BinTree where
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

{-
  BinTree instance of the Applicative class.  
-}
instance Applicative BinTree where
  pure x = treeRepeat x
  (Node f1 l1 r1) <*> (Node v2 l2 r2) = Node (f1 v2) (l1 <*> l2) (r1 <*> r2)


