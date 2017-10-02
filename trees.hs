import Data.Maybe
import Data.List (nub)

{-
  in one of the previous exams it was asked to
  add an Int to the Node, that would look like this:

  Node (Tree a) a Int (Tree a)

-}

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

tree :: Tree Int
tree = (Node
          (Node
              Leaf
                2
              Leaf
          )
            3
          (Node
              Leaf
                4
              (Node
                Leaf
                  5
                Leaf
              )
          )
        )

size :: Tree a -> Int
size Leaf = 0 -- end of a node
-- 1 + the size of left and right
-- remember to map this instead when we have
-- tree with [] for example
size (Node l _ r) = 1 + size l + size r

mirror :: Tree a -> Tree a
mirror Leaf = Leaf
-- just switch the right and left side and mirror those
mirror (Node l x r) = Node (mirror r) x (mirror l)

-- values from left most to right most
enumInfix :: Tree a -> [a]
enumInfix Leaf = []
enumInfix (Node l x r) = enumInfix l ++ [x] ++ enumInfix r

-- see if an element occues in a tree
-- our tree now does not have a value
-- making it always false but remember to check
-- if we have values in the leaves!
elem' :: Ord a => a -> Tree a -> Bool
elem' _ Leaf = False
elem' e (Node l x r) | e == x = True
                     | e < x = elem' e l
                     | e > x = elem' e r

toSearchTree :: Ord a => [a] -> Tree a
toSearchTree [] = Leaf
toSearchTree (x:xs) = insert x (toSearchTree xs)

{-
  this could also be done like this:
  insert = foldr insert Leaf
  but recursive makes more sense to keep
  track of what is going, remember that this
  insert replaces a value / overwrites it
-}
insert :: Ord a => a -> Tree a -> Tree a
-- hit a leaf and still have a value?
-- make a new node
insert e Leaf = Node Leaf e Leaf
                      -- match, set the value
insert e (Node l x r) | e == x    = Node l x r
                      -- val bigger then x ? insert on the left
                      | e > x     = Node (insert e l) x r
                      -- insert on the right
                      | otherwise = Node l x (insert e r)

-- we get free sorting!
sort' :: Ord a => [a] -> [a]
sort' = enumInfix . toSearchTree

-- deleting is a bit harder, because you need to
-- reconstruct the branches after removing one
delete :: Ord a => a -> Tree a -> Tree a
delete e Leaf = Leaf
delete e (Node l x r) | e == x    = case (l,r) of
                                      -- if left is Leaf, return right
                                      (Leaf, _) -> r
                                      (_, Leaf) -> l
                                      -- no Leaves?
                                      -- get the top value and insert
                                      -- it in a new node
                                      _ -> let (x, l') = topValue l
                                            in Node l' x r
                      | e < x     = Node (delete e l) x r
                      | otherwise = Node l x (delete e r)

topValue :: Tree a -> (a, Tree a)
topValue Leaf = error "no top"
topValue (Node l x Leaf) = (x, l)
-- get the top value from the right size, and return that
topValue (Node l x r)    = let (y, r') = topValue r in (y, Node l x r')
