{-# LANGUAGE UnicodeSyntax #-}
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show

insert :: Ord a => a → Tree a → Tree a
insert newVer Leaf = Node newVer Leaf Leaf
insert newVer (Node x l r)
    | newVer < x = Node x (insert newVer l) r
    | newVer > x = Node x l (insert newVer r)
    | newVer == x = Node x l r

delete :: Ord a => a → Tree a → Tree a
delete ver Leaf = Leaf

delete ver (Node x Leaf r) 
    | ver < x = Node x Leaf r
    | ver > x = Node x Leaf (delete ver r)
    | ver == x = r
            
delete ver (Node x l Leaf) 
    | ver < x = Node x (delete ver l) Leaf
    | ver > x = Node x l Leaf
    | ver == x = l

delete ver (Node x l r) 
    | ver < x = Node x (delete ver l) r
    | ver > x = Node x l (delete ver r)
    | ver == x = Node m l (delete m r) 
            where m = findMin r
 

findMin :: Ord a => Tree a -> a
findMin (Node x Leaf _) = x
findMin (Node _ l _) = findMin l
