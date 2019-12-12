-- |
module Relabelling where
import MyState

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- naive relabeling
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'


-- applicative relabeling
fresh :: ST Int
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- monadic relabelling
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')
